-------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Hakyll
import           Text.Pandoc.Options
import           System.Process
import           Control.Monad                   (foldM)
import           Data.Monoid                     (mappend)
import           Data.Maybe                      (fromMaybe)
import           Data.List                       (find) 

-------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    tags <- buildTags "writing/*" (fromCapture "tags/*.html")

    modifications <- buildModifications "writing/*"

    let context = postCtxWithTags tags modifications

    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged \"" ++ tag ++ "\""
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let ctx = constField "title" title
                      `mappend` listField "posts" (context) (return posts)
                      `mappend` defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/post-list.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    match ("images/**" .||. "css/*") $ do
        route   idRoute
        compile copyFileCompiler

    match "writing/*" $ do
        route $ setExtension "html"
        compile $ pandocMathCompiler
            >>= loadAndApplyTemplate "templates/post.html" (context)
            >>= loadAndApplyTemplate "templates/default.html" (defaultContext)
            >>= relativizeUrls

    match "pages/writing.markdown" $ do
        route $ gsubRoute "pages/" (const "") `composeRoutes` setExtension "html"
        compile $ do
            posts <- recentFirst =<< loadAll "writing/*"
            let indexCtx = 
                    listField "posts" (context) (return posts) `mappend`
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


postCtxWithTags :: Tags -> [(Identifier, String)] -> Context String
postCtxWithTags tags times = 
    modificationCtx times `mappend` 
    githubCtx `mappend`
    tagsField "tags" tags `mappend` 
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

githubCtx :: Context String 
githubCtx = field "githubLink" $ \item -> do 
    let path = (toFilePath . itemIdentifier) item
    return $ path

modificationCtx :: [(Identifier, String)] -> Context String 
modificationCtx modificationTimes = field "lastModified" $ \item -> do
    let time = find (\x -> (fst x) == (itemIdentifier item)) modificationTimes >>= return . snd 
    return $ fromMaybe "Post not in git" $ time

buildModifications ::  Pattern -> Rules [(Identifier, String)]
buildModifications pattern = do 
    ids <- getMatches pattern
    pairs <- preprocess $ foldM getLastModified [] ids
    return pairs
    where 
        getLastModified l id' = do
            lmodtime <- readProcess "git" 
                ["log", "-1", "--format=%ad", "--date=format:%b %d, %Y", 
                (toFilePath id')] 
                ""
            return $ (id', lmodtime) : l


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