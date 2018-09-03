-------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
-- import qualified Data.Set as S
import           Text.Pandoc.Options
import Debug.Trace

-------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do

    tags <- buildTags "writing/*" (fromCapture "tags/*.html")

    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged \"" ++ tag ++ "\""
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let ctx = constField "title" title
                      `mappend` listField "posts" (postCtxWithTags tags) (return posts)
                      `mappend` defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/post-list.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    match ("images/*" .||. "css/*") $ do
        route   idRoute
        compile copyFileCompiler

    match "writing/*" $ do
        route $ setExtension "html"
        compile $ pandocMathCompiler
            >>= loadAndApplyTemplate "templates/post.html" (postCtxWithTags tags)
            >>= loadAndApplyTemplate "templates/default.html" (defaultContext)
            >>= relativizeUrls

    match "pages/writing.markdown" $ do
        route $ gsubRoute "pages/" (const "") `composeRoutes` setExtension "html"
        compile $ do
            posts <- recentFirst =<< loadAll "writing/*"
            let indexCtx = 
                    listField "posts" (postCtxWithTags tags) (return posts) `mappend`
                    defaultContext

            getResourceBody 
                >>= applyAsTemplate indexCtx
                >>= renderPandoc
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match ("pages/projects.markdown" .||. "pages/index.markdown" .||. "pages/work-in-progress.markdown") $ do
        route   $ gsubRoute "pages/" (const "") `composeRoutes` setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler


postCtxWithTags :: Tags -> Context String
postCtxWithTags tags = 
    tagsField "tags" tags `mappend` 
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

-------------------------------------------------------------------------------
-- Utils
-------------------------------------------------------------------------------

pandocMathCompiler :: Compiler (Item String)
pandocMathCompiler =
  let
    mathExtensions =
      [ Ext_tex_math_dollars
      , Ext_tex_math_double_backslash
      , Ext_latex_macros
      ]
    defaultExtensions = writerExtensions defaultHakyllWriterOptions
    newExtensions = foldr enableExtension defaultExtensions mathExtensions
    writerOptions =
      defaultHakyllWriterOptions
      { writerExtensions = newExtensions
      , writerHTMLMathMethod = MathJax ""
      }
    in pandocCompilerWith defaultHakyllReaderOptions writerOptions