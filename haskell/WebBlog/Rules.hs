{-# LANGUAGE OverloadedStrings #-}
module WebBlog.Rules where

import qualified Control.Exception               as CE
import qualified GHC.IO.Exception                as G
import           Foreign.C.Error                 (Errno(Errno), ePIPE)
import           Data.String                     (fromString)
import           Data.String.Utils               (replace)
import           Data.Monoid
import qualified Data.Set                        as S
import qualified Data.ByteString.Char8           as C8
import           Data.IORef
import           System.IO.Unsafe                (unsafePerformIO)
import           Text.Highlighting.Kate          (styleToCss, espresso)
import           Text.Pandoc.Options
import           Text.Pandoc.Definition
import           Text.Pandoc.Walk
import           Text.Regex                      (mkRegex, matchRegex)
import           Text.Blaze.Html
import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as A
import           Text.Blaze.Html.Renderer.String (renderHtml)
import           Pipes
import           Pipes.Shell
import           Hakyll
import           Hakyll.Web.Archives
import           WebBlog.Emoji                   (covertCode)

compileRules :: Rules ()
compileRules = do
  tags <- buildTags "posts/*" (fromCapture "tags/*.html")
  archives <- buildArchives "%Y-%m" "posts/*" (fromCapture "archives/*.html")

  match "images/*" $ do
    route idRoute
    compile copyFileCompiler

  match "emoji/*" $ do
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
                               <> archiveField "archive-list" archives

postCtxWithTags :: Tags -> Archives -> Context String
postCtxWithTags tags archive = tagsField "tags" tags
                            <> postCtx
                            <> tagCloudField "tag-cloud" 100.0 200.0 tags
                            <> archiveField "archive-list" archive

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
    in pandocCompilerWithTransform
         defaultHakyllReaderOptions
         writerOptions
         (mkEmoji . pygmentize)


myFeedConfiguration :: FeedConfiguration
myFeedConfiguration = FeedConfiguration
    { feedTitle       = "Cold Codes: Bruce's Coding Endeavor"
    , feedDescription = "This feed logs my random thoughts on technology"
    , feedAuthorName  = "Bruce Li"
    , feedAuthorEmail = "muyuanli@buffalo.edu"
    , feedRoot        = "http://coldcodes.info"
    }

mkEmoji :: Pandoc -> Pandoc
mkEmoji = walk emojize

emojize :: Inline -> Inline
emojize (Str x) =
    case matchRegex (mkRegex "(:[a-zA-Z]+:)") x of
      Nothing -> Str x
      Just matches -> RawInline "html" $
        foldr (\c acc ->
                replace c ("<img class=\"emojione\" alt=\"" ++
                           c ++ "\" src=\"/emoji/" ++
                           covertCode c ++ ".png\" style=\"height: 1em\"/>") acc)
              x matches
emojize x = x

pygmentize :: Pandoc -> Pandoc
pygmentize (Pandoc meta bs) = Pandoc meta (map pygTrans bs)

pygTrans :: Block -> Block
pygTrans (CodeBlock (cls, [lang], _) code) =
    let composed = renderHtml $ H.figure !
                      (A.class_ . fromString) cls $ do
                      preEscapedToHtml (runPygment lang code)
    in RawBlock "html" composed
pygTrans x = x

runPygment :: String -> String -> String
runPygment lang txt = unsafePerformIO $ do
  mv <- newIORef ""
  runShell $ yield (C8.pack txt) >?>
           cmd ("pygmentize -l " ++ lang ++ " -f html") >->
           ignoreErr >-> go mv
  readIORef mv
      where go mvar = do
              bs <- await
              x  <- liftIO $ CE.try $ modifyIORef mvar (++(C8.unpack bs))
              case x of
                Left (G.IOError { G.ioe_type  = G.ResourceVanished
                                , G.ioe_errno = Just ioe })
                     | Errno ioe == ePIPE
                         -> return ()
                Left  e  -> liftIO (CE.throwIO e)
                Right () -> go mvar
