{-# LANGUAGE OverloadedStrings #-}
module WebBlog.Rules where

import Control.Applicative
import Data.Time
import Data.List
import System.Locale (defaultTimeLocale)
import Data.Monoid
import Hakyll
import Text.Highlighting.Kate (styleToCss, espresso)
import Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as A

compileRules :: Rules ()
compileRules = do
  tags <- buildTags "posts/*" (fromCapture "tags/*.html")
  archive <- buildArchive "posts/*" (fromCapture "archives/*.html")
  compileImage
  compileCss
  createSyntaxCss
  compilePosts tags
  compileTemplates
  compilePostList tags
  copyPostMedia
  makeTagPages tags
  makeTagPages archive

compileImage :: Rules ()
compileImage = match "images/*" $ do
                 route idRoute
                 compile copyFileCompiler

copyPostMedia :: Rules ()
copyPostMedia = match "media/*" $ do
                  route idRoute
                  compile copyFileCompiler

compileCss :: Rules ()
compileCss = match "css/*" $ do
               route idRoute
               compile copyFileCompiler

createSyntaxCss :: Rules ()
createSyntaxCss = create ["css/syntax.css"] $ do
               route idRoute
               compile $ makeItem (compressCss . styleToCss $ espresso)

compilePosts :: Tags -> Rules ()
compilePosts tags = match "posts/*" $ do
        route $ setExtension "html"
        compile $ do
          pandocCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/post.html" (postCtxWithTags tags)
            >>= loadAndApplyTemplate "templates/base.html" (postCtxWithTags tags)
            >>= relativizeUrls

makeTagPages :: Tags -> Rules ()
makeTagPages tags = tagsRules tags $ \_ pattern -> do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let ctx = postListCtx tags (return posts)
            makeItem ""
              >>= loadAndApplyTemplate "templates/tags.html" ctx
              >>= loadAndApplyTemplate "templates/base.html" ctx
              >>= relativizeUrls

compilePostList :: Tags -> Rules ()
compilePostList tags = create ["index.html"] $ do
        route $ idRoute
        compile $ do
          posts <- recentFirst =<< loadAllSnapshots "posts/*" "content"
          let ctx = postListCtx tags (return posts)
          makeItem ""
                  >>= loadAndApplyTemplate "templates/post-list.html" ctx
                  >>= loadAndApplyTemplate "templates/base.html" ctx
                  >>= relativizeUrls

compileTemplates :: Rules ()
compileTemplates = match "templates/*" $ compile templateCompiler

postListCtx :: Tags -> Compiler [Item String] -> Context String
postListCtx tags posts = listField "posts" teaserCtx posts <>
                         defaultContext <>
                         tagCloudField "tag-cloud" 100.0 200.0 tags

postCtxWithTags :: Tags -> Tags -> Context String
postCtxWithTags tags archive = tagsField "tags" tags <> postCtx <>
                       tagCloudField "tag-cloud" 100.0 200.0 tags <>
                       tagCloudFieldWith "archive-list" makeLinkList
                           (("<li>" ++) . (++ "</li>") . intercalate "\n")
                           100.0
                           100.0
                           archive

postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" <>
          defaultContext

teaserCtx :: Context String
teaserCtx = teaserField "teaser" "content" <>
            dateField "date" "%B %e, %Y"   <>
            defaultContext

buildArchive :: (MonadMetadata m, Functor m) => Pattern ->
                (String -> Identifier) -> m Tags
buildArchive = buildTagsWith getDate

getDate :: (MonadMetadata m, Functor m) => Identifier -> m [String]
getDate identifier = (return . formatTime defaultTimeLocale "%Y")
                     <$> getItemUTC defaultTimeLocale identifier

makeLinkList :: Double -> Double -> String -> String ->
                Int -> Int -> Int -> String
makeLinkList _ _ tag url cnt _ _ = renderHtml $
  H.li $ H.a H.! A.href (H.toValue url) $ H.toHtml tag
