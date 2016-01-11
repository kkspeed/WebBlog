---
title: Using Pygments for Code Highlighting in Hakyll
tags: Functional programming, Hakyll, Haskell
---

Hakyll uses Pandoc to process markdown and code snippets. It's very
easy to integrate Kate to Hakyll for code highlighting. But I choose
to switch to Pygments due to its support for many programming
languages.

<!--more-->
I found (this
blog)[http://www.blaenkdenum.com/posts/the-switch-to-hakyll/], which
briefly describes how Pandoc should be integrated with
Pygments. Roughly speaking, Pandoc is an abstract syntax tree. A code
highlighting process is a tree transformation. In Pandoc's AST, the
CodeBlock element is for code snippets. We match its patterns to
supply our transformations:

~~~ {.haskell}
-- The meta information:
-- * cls is the class you supplied in source block
-- * a list containing language declarations
-- * actual code block
pygTrans :: Block -> Block
pygTrans (CodeBlock (cls, [lang], _) code) =
    let composed = renderHtml $ H.figure !
                      (A.class_ . fromString) cls $ do
                      preEscapedToHtml (runPygment lang code)
    in RawBlock "html" composed
pygTrans x = x
~~~

The Pygment can be invoked with Pipe. Below is my code. It looks messy
but it works. It's by no means efficient as I did a bit list
concatenations over strings.

~~~~~~~~~~~~~ {.haskell}
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
~~~~~~~~~~~~~~~

As the last piece, Hakyll includes a functions
pandocCompilerWithTransform that allows you to plug in your
transformations.

~~~ {.haskell}
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
         pygmentize

pygmentize :: Pandoc -> Pandoc
pygmentize (Pandoc meta bs) = Pandoc meta (map pygTrans bs)
~~~

Now we can use Pygment to highlight the code. You'd probably want to
grab some code highlighting css files. Note that Pygments indeed runs
slower than Kate, but it's not a big deal for me.
