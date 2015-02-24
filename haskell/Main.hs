module Main where

import Hakyll
import WebBlog.Rules

main :: IO ()
main = hakyll compileRules
