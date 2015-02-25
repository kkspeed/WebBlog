{-# LANGUAGE OverloadedStrings #-}
module WebBlog.Rules where

import Data.Monoid
import Hakyll

compileRules :: Rules ()
compileRules = do
  compileImage
  compileCss
  compilePosts
  compileTemplates

compileImage :: Rules ()
compileImage = match "images/*" $ do
                 route idRoute
                 compile copyFileCompiler

compileCss :: Rules ()
compileCss = match "css/*" $ do
               route idRoute
               compile copyFileCompiler

compilePosts :: Rules ()
compilePosts = match "posts/*" $ do
        route $ setExtension "html"
        compile $ do
          pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html" postCtx
            >>= loadAndApplyTemplate "templates/base.html" postCtx
            >>= relativizeUrls

compileTemplates :: Rules ()
compileTemplates = match "templates/*" $ compile templateCompiler

postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" <>
          defaultContext
