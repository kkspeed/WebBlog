{-# LANGUAGE OverloadedStrings #-}
module WebBlog.Rules where

import           Data.Monoid
import qualified Data.Set                        as S
import           Text.Highlighting.Kate          (styleToCss, espresso)
import           Text.Pandoc.Options
import           Hakyll
import           Hakyll.Web.Archives

compileRules :: Rules ()
compileRules = do
  tags <- buildTags "posts/*" (fromCapture "tags/*.html")
  archives <- buildArchives "%Y-%m" "posts/*" (fromCapture "archives/*.html")

  match "images/*" $ do
    route idRoute
    compile copyFileCompiler

  match "media/*" $ do
    route idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route idRoute
    compile copyFileCompiler

  create ["css/syntax.css"] $ do
    route idRoute
    compile $ makeItem (compressCss . styleToCss $ espresso)

  match "posts/*" $ do
    route $ setExtension "html"
    compile $ do
      let ctx = postCtxWithTags tags archives
      customPandocCompiler
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/post.html" ctx
        >>= loadAndApplyTemplate "templates/base.html" ctx
        >>= relativizeUrls

  create ["index.html"] $ do
    route $ idRoute
    compile $ do
      posts <- recentFirst =<< loadAllSnapshots "posts/*" "content"
      let ctx = postListCtx tags archives (return posts)
      makeItem ""
              >>= loadAndApplyTemplate "templates/post-list.html" ctx
              >>= loadAndApplyTemplate "templates/base.html" ctx
              >>= relativizeUrls

  let tagRule _ pattern = do
        route idRoute
        compile $ do
          posts <- recentFirst =<< loadAll pattern
          let ctx = postListCtx tags archives (return posts)
          makeItem ""
            >>= loadAndApplyTemplate "templates/tags.html" ctx
            >>= loadAndApplyTemplate "templates/base.html" ctx
            >>= relativizeUrls

  tagsRules tags tagRule

  archiveRules archives tagRule

  create ["atom.xml"] $ do
    route idRoute
    compile $ do
        let feedCtx = postCtx <>
                      bodyField "description"
        posts <- fmap (take 10) . recentFirst =<<
                loadAllSnapshots "posts/*" "content"
        renderAtom myFeedConfiguration feedCtx posts

  create ["rss.xml"] $ do
    route idRoute
    compile $ do
        let feedCtx = postCtx <>
                      bodyField "description"
        posts <- fmap (take 10) . recentFirst =<<
                loadAllSnapshots "posts/*" "content"
        renderRss myFeedConfiguration feedCtx posts

  match "templates/*" $ compile templateCompiler


postListCtx :: Tags -> Archives -> Compiler [Item String] -> Context String
postListCtx tags archives posts = listField "posts" teaserCtx posts
                               <> defaultContext
                               <> tagCloudField "tag-cloud" 100.0 200.0 tags
                               <> archiveCloudField "archive-list" archives

postCtxWithTags :: Tags -> Archives -> Context String
postCtxWithTags tags archive = tagsField "tags" tags
                            <> postCtx
                            <> tagCloudField "tag-cloud" 100.0 200.0 tags
                            <> archiveCloudField "archive-list" archive

postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" <>
          defaultContext

teaserCtx :: Context String
teaserCtx = teaserField "teaser" "content" <>
            dateField "date" "%B %e, %Y"   <>
            defaultContext

customPandocCompiler :: Compiler (Item String)
customPandocCompiler =
    let customExtensions = [Ext_tex_math_dollars]
        defaultExtensions = writerExtensions defaultHakyllWriterOptions
        newExtensions = foldr S.insert defaultExtensions customExtensions
        writerOptions = defaultHakyllWriterOptions {
                          writerExtensions = newExtensions
                        }
    in pandocCompilerWith defaultHakyllReaderOptions writerOptions

myFeedConfiguration :: FeedConfiguration
myFeedConfiguration = FeedConfiguration
    { feedTitle       = "Cold Codes: Bruce's Coding Endeavor"
    , feedDescription = "This feed logs my random thoughts on technology"
    , feedAuthorName  = "Bruce Li"
    , feedAuthorEmail = "muyuanli@buffalo.edu"
    , feedRoot        = "http://coldcodes.info"
    }
