{-# LANGUAGE OverloadedStrings #-}
module WebBlog.Rules where

import           Control.Applicative
import           Data.Time
import           Data.Monoid
import           Data.List
import qualified Data.Set                        as S
import           System.Locale                   (defaultTimeLocale)
import           Text.Highlighting.Kate          (styleToCss, espresso)
import           Text.Blaze.Html.Renderer.String (renderHtml)
import           Text.Pandoc.Options
import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as A
import           Hakyll

compileRules :: Rules ()
compileRules = do
  tags <- buildTags "posts/*" (fromCapture "tags/*.html")
  archive <- buildArchive "posts/*" (fromCapture "archives/*.html")
  compileImage
  compileCss
  createSyntaxCss
  compilePosts tags archive
  compileTemplates
  compilePostList tags archive
  copyPostMedia
  makeTagPages tags archive

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

compilePosts :: Tags -> Tags -> Rules ()
compilePosts tags archives = match "posts/*" $ do
        route $ setExtension "html"
        compile $ do
          let ctx = postCtxWithTags tags archives
          customPandocCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/post.html" ctx
            >>= loadAndApplyTemplate "templates/base.html" ctx
            >>= relativizeUrls

makeTagPages :: Tags -> Tags -> Rules ()
makeTagPages tags archives = do
  tagsRules tags $ \_ pattern -> do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll pattern
      let ctx = postListCtx tags archives (return posts)
      makeItem ""
        >>= loadAndApplyTemplate "templates/tags.html" ctx
        >>= loadAndApplyTemplate "templates/base.html" ctx
        >>= relativizeUrls
  tagsRules archives $ \_ pattern -> do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll pattern
      let ctx = postListCtx tags archives (return posts)
      makeItem ""
        >>= loadAndApplyTemplate "templates/tags.html" ctx
        >>= loadAndApplyTemplate "templates/base.html" ctx
        >>= relativizeUrls

compilePostList :: Tags -> Tags -> Rules ()
compilePostList tags archives = create ["index.html"] $ do
        route $ idRoute
        compile $ do
          posts <- recentFirst =<< loadAllSnapshots "posts/*" "content"
          let ctx = postListCtx tags archives (return posts)
          makeItem ""
                  >>= loadAndApplyTemplate "templates/post-list.html" ctx
                  >>= loadAndApplyTemplate "templates/base.html" ctx
                  >>= relativizeUrls

compileTemplates :: Rules ()
compileTemplates = match "templates/*" $ compile templateCompiler

postListCtx :: Tags -> Tags -> Compiler [Item String] -> Context String
postListCtx tags archives posts = listField "posts" teaserCtx posts <>
                                  defaultContext <>
                                  tagCloudField "tag-cloud" 100.0 200.0 tags <>
                                  archiveCloud archives


postCtxWithTags :: Tags -> Tags -> Context String
postCtxWithTags tags archive = tagsField "tags" tags <> postCtx <>
                       tagCloudField "tag-cloud" 100.0 200.0 tags <>
                       archiveCloud archive

archiveCloud :: Tags -> Context String
archiveCloud archives = tagCloudFieldWith "archive-list" makeLinkList
                           (("<ul>" ++) . (++ "</ul>") . intercalate "\n" . reverse . sort)
                           100.0
                           100.0
                           archives

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
  H.li $ H.a H.! A.href (H.toValue url) $
   H.toHtml (tag ++ " (" ++ show cnt ++ ")")

customPandocCompiler :: Compiler (Item String)
customPandocCompiler =
    let customExtensions = [Ext_tex_math_dollars]
        defaultExtensions = writerExtensions defaultHakyllWriterOptions
        newExtensions = foldr S.insert defaultExtensions customExtensions
        writerOptions = defaultHakyllWriterOptions {
                          writerExtensions = newExtensions
                        }
    in pandocCompilerWith defaultHakyllReaderOptions writerOptions
