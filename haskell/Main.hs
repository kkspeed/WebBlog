module Main where

import Hakyll
import WebBlog.Rules

main :: IO ()
main = hakyllWith config compileRules

config :: Configuration
config = defaultConfiguration {
           deployCommand = "rsync -ave ssh _site/ bruce@bruce-blog:/var/www/html/"
         }
