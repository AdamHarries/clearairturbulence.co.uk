--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls
 
    match "about.markdown" $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "archive.markdown" $ do
        route $ setExtension "html"
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx = 
                    listField "posts" postCtx (return posts) `mappend`
                    defaultContext

            getResourceBody 
                >>= applyAsTemplate indexCtx
                >>= renderPandoc
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
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


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

takeAtMost :: Int -> [a] -> [a]
takeAtMost _ [] = []
takeAtMost 0 _ = []
takeAtMost n (x:xs) = x:(takeAtMost (n-1) xs)