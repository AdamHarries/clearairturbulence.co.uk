-------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Hakyll
import           Text.Pandoc
import           System.Process
import           Control.Monad                   (foldM)
import           Data.Monoid                     (mappend)
import           Data.Maybe                      (fromMaybe)
import           Data.List                       (find) 

-------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    -- get tags from all the pages in writing
    tags <- buildTags "writing/*" (fromCapture "tags/*.html")

    -- get the modification times (from git) for all the pages
    modifications <- runCommandOnItems gitModTime "**"

    -- get the git hashes from all the pages
    hashes <- runCommandOnItems gitShortHash "**"

    -- Create a context for static information/pages
    let staticContext = staticCtx hashes

    let context = postCtx tags modifications

    tagsRules tags $ \tag pt -> do
        let title = "Posts tagged \"" ++ tag ++ "\""
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pt
            let ctx = constField "title" title
                      `mappend` listField "posts" context (return posts)
                      `mappend` defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/post-list.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    match ("images/**" .||. "css/*") $ do
        route   idRoute
        compile copyFileCompiler

    match ("raw/*") $ do
        route $ gsubRoute "raw/" (const "")
        compile copyFileCompiler

    match "scratch/*" $ do 
        route $ setExtension "html" 
        compile $ pandocMathCompiler 
            >>= loadAndApplyTemplate "templates/post.html" (filepathCtx `mappend` context)
            >>= loadAndApplyTemplate "templates/default.html" staticContext
            >>= relativizeUrls

    match "writing/*" $ do
        route $ setExtension "html"
        compile $ pandocMathCompiler
            >>= loadAndApplyTemplate "templates/post.html" (filepathCtx `mappend` context)
            >>= loadAndApplyTemplate "templates/default.html" staticContext
            >>= relativizeUrls

    match "pages/writing.markdown" $ do
        route $ gsubRoute "pages/" (const "") `composeRoutes` setExtension "html"
        compile $ do
            posts <- recentFirst =<< loadAll "writing/*"
            let indexCtx = 
                    listField "posts" context (return posts) `mappend`
                    staticContext

            getResourceBody 
                >>= applyAsTemplate indexCtx
                >>= renderPandoc
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match ("pages/projects.markdown" .||. 
        "pages/index.markdown" .||. 
        "pages/work-in-progress.markdown" .||. 
        "pages/err404.markdown" .||. 
        "pages/err404.markdown") $ do
        route   $ gsubRoute "pages/" (const "") `composeRoutes` setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" staticContext
            >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

staticCtx :: [(Identifier, String)] -> Context String
staticCtx hashes = 
    filepathCtx `mappend`
    genericStringField "githash" hashes `mappend` 
    defaultContext

postCtx :: Tags -> [(Identifier, String)] -> Context String
postCtx tags times = 
    genericStringField "lastModified" times `mappend`
    tagsField "tags" tags `mappend` 
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

filepathCtx :: Context String 
filepathCtx = field "filepath" $ \item -> do 
    let path = (toFilePath . itemIdentifier) item
    return $ path

genericStringField :: String -> [(Identifier, String)] -> Context String
genericStringField name vs = field name $ \ident ->
    return $ fromMaybe ("No value found for key " ++ name) $ search ident
    where 
        search ident = 
            find (\p -> (fst p) == (itemIdentifier ident)) vs >>= return . snd

-- Generic tools to query information about posts using command line programs
data ShlCommand = ShlCommand { 
    cmname :: String, 
    cmargs :: [String]
}

gitModTime :: ShlCommand
gitModTime = ShlCommand { cmname = "git", cmargs = ["log", "-1", "--format=%ad", "--date=format:%b %d, %Y"]}

gitShortHash :: ShlCommand
gitShortHash = ShlCommand { cmname = "git", cmargs = ["log", "-1", "--pretty=format:%h"]}

runCommandOnItems :: ShlCommand -> Pattern -> Rules [(Identifier, String)]
runCommandOnItems cmd ptrn = do 
    ids <- getMatches ptrn
    preprocess $ foldM runOnItem [] ids
    where 
        runOnItem l id' = do 
            cmdRes <- readProcess (cmname cmd) (cmargs cmd ++ [toFilePath id']) ""
            return $ (id', cmdRes) : l

-- Compiler for pandoc math extensions
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
