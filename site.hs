--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Hakyll
import           Data.Monoid (mappend)
import           Data.List (nub, sort)
import           Data.Maybe (fromMaybe)
--import           Data.Char (toLower)
import qualified Data.Map as M
import           Data.String.Utils (split, join)


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "images/portfolio/*" $ do
        route idRoute
        compile copyFileCompiler

    match "js/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "pdf/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "dist/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "projects/personal/*" $ do
        route idRoute
        compile $ pandocCompiler

    match "projects/professional/*" $ do
        route idRoute
        compile $ pandocCompiler

    create ["portfolio.html"] $ do
        route idRoute
        compile $ do
            personal <- (loadAll "projects/personal/*" :: Compiler [Item String])
            professional <- (loadAll "projects/professional/*" :: Compiler [Item String])
            let projects = personal ++ professional
            rawTags <- mapM tags projects
            listedTags <- mapM makeItem (prepareTags rawTags)

            let prCtx = classContext `mappend` defaultContext
                flCtx = listField "tagslist" defaultContext (return listedTags) `mappend`
                        listField "personal" prCtx (return personal) `mappend`
                        listField "professional" prCtx (return professional) `mappend`
                        constField "title" "Portfolio" `mappend`
                        defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/portfolio.html" flCtx
                >>= loadAndApplyTemplate "templates/base.html" flCtx
                >>= relativizeUrls

    match (fromList ["index.md", "about.md"]) $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/base.html" defaultContext
            >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
tags item = getMetadataField (itemIdentifier item) "tags"

classContext :: Context a
classContext = field "tags-cat" $ \item -> do
    metadata <- getMetadata (itemIdentifier item)
    let tagsStr = fromMaybe "none" $ M.lookup "tags" metadata
    return $ join " " $ split ", " tagsStr

prepareTags :: [Maybe String] -> [String]
prepareTags = 
    sort . 
--    (map $ map toLower) .
    nub . 
    concat . 
    (map $ split ", ") . 
    (map $ fromMaybe "none")