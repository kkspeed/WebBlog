{-# LANGUAGE OverloadedStrings #-}
module WebBlog.Rules where

import Data.Monoid
import Hakyll
import Text.Highlighting.Kate (styleToCss, tango)


compileRules :: Rules ()
compileRules = do
  compileImage
  compileCss
  createSyntaxCss
  compilePosts
  compileTemplates
  compilePostList

compileImage :: Rules ()
compileImage = match "images/*" $ do
                 route idRoute
                 compile copyFileCompiler

compileCss :: Rules ()
compileCss = match "css/*" $ do
               route idRoute
               compile copyFileCompiler

createSyntaxCss :: Rules ()
createSyntaxCss = create ["css/syntax.css"] $ do
               route idRoute
               compile $ makeItem (compressCss . styleToCss $ tango)

compilePosts :: Rules ()
compilePosts = match "posts/*" $ do
        route $ setExtension "html"
        compile $ do
          pandocCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/post.html" postCtx
            >>= loadAndApplyTemplate "templates/base.html" postCtx
            >>= relativizeUrls

compilePostList :: Rules ()
compilePostList = create ["index.html"] $ do
        route $ idRoute
        compile $ do
          posts <- recentFirst =<< loadAllSnapshots "posts/*" "content"
          let postListCtx = listField "posts" teaserCtx (return posts) <>
                            defaultContext
          makeItem ""
                  >>= loadAndApplyTemplate "templates/post-list.html" postListCtx
                  >>= loadAndApplyTemplate "templates/base.html" postListCtx
                  >>= relativizeUrls

compileTemplates :: Rules ()
compileTemplates = match "templates/*" $ compile templateCompiler

postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" <>
          defaultContext

teaserCtx :: Context String
teaserCtx = teaserField "teaser" "content" <>
            dateField "date" "%B %e, %Y"   <>
            defaultContext
