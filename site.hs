-------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
-- import qualified Data.Set as S
import           Text.Pandoc.Options

-------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do

    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged \"" ++ tag ++ "\""
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let ctx = constField "title" title
                      `mappend` listField "posts" postCtx (return posts)
                      `mappend` defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/tag.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "longform/*" $ do
        route $ setExtension "html"
        compile $ pandocMathCompiler
            >>= loadAndApplyTemplate "templates/post.html"    (postCtxWithTags tags)
            >>= loadAndApplyTemplate "templates/default.html" (postCtxWithTags tags)
            >>= relativizeUrls

    match "unfinished/*" $ do
        route $ setExtension "html"
        compile $ pandocMathCompiler
            >>= loadAndApplyTemplate "templates/post.html"    (postCtxWithTags tags)
            >>= loadAndApplyTemplate "templates/default.html" (postCtxWithTags tags)
            >>= relativizeUrls
    
    match "pages/longform.markdown" $ do
        route $ gsubRoute "pages/" (const "") `composeRoutes` setExtension "html"
        compile $ do
            posts <- recentFirst =<< loadAll "longform/*"
            let indexCtx = 
                    listField "posts" postCtx (return posts) `mappend`
                    defaultContext

            getResourceBody 
                >>= applyAsTemplate indexCtx
                >>= renderPandoc
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "pages/unfinished.markdown" $ do
        route $ gsubRoute "pages/" (const "") `composeRoutes` setExtension "html"
        compile $ do
            posts <- recentFirst =<< loadAll "unfinished/*"
            let indexCtx = 
                    listField "posts" postCtx (return posts) `mappend`
                    defaultContext

            getResourceBody 
                >>= applyAsTemplate indexCtx
                >>= renderPandoc
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "pages/projects.markdown" $ do
        route   $ gsubRoute "pages/" (const "") `composeRoutes` setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "pages/about.markdown" $ do
        route   $ gsubRoute "pages/" (const "") `composeRoutes` setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "index.markdown" $ do
        route $ setExtension "html"
        compile $ do
            posts <- recentFirst 
                =<< (return . takeAtMost 5) 
                =<< loadAll "posts/*"
            let indexCtx = 
                    listField "posts" postCtx (return posts) `mappend`
                    defaultContext

            getResourceBody 
                >>= applyAsTemplate indexCtx
                >>= renderPandoc
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler


-------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

postCtxWithTags :: Tags -> Context String
postCtxWithTags tags = 
    tagsField "tags" tags `mappend` 
    postCtx

-------------------------------------------------------------------------------
-- Utils
-------------------------------------------------------------------------------

takeAtMost :: Int -> [a] -> [a]
takeAtMost _ [] = []
takeAtMost 0 _ = []
takeAtMost n (x:xs) = x:(takeAtMost (n-1) xs)

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