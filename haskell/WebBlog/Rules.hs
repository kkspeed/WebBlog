{-# LANGUAGE OverloadedStrings #-}
module WebBlog.Rules where

import Hakyll

compileRules :: Rules ()
compileRules = do
  compileImage
  compileCss

compileImage :: Rules ()
compileImage = match "images/*" $ do
                 route idRoute
                 compile copyFileCompiler

compileCss :: Rules ()
compileCss = match "css/*" $ do
               route idRoute
               compile copyFileCompiler
