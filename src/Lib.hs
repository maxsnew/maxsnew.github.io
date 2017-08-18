{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( site
    ) where

import Hakyll
import Control.Monad
import Data.Maybe (fromJust)
import System.Process (callProcess)

site :: IO ()
site = do
  hakyllWith conf $ do
    match "*.md" mdpost
    match "blog/*.md" mdpost

    match "js/*.js" $ do
      route   idRoute
      compile copyFileCompiler
  
    match "css/*.css" $ do
      route   idRoute
      compile compressCssCompiler

    forM_ ["docs/*", "img/*"] $ \p -> 
      match p rawOut
  
    match "templates/*" $ compile templateCompiler

    -- match "../src/elm/Main.elm" $ do
    --   route $ constRoute "hubway.html"
    --   compile $ do
    --     rt <- fromJust <$> (getRoute =<< getUnderlying)
    --     unsafeCompiler $ () <$ (callProcess "elm" ["make", rt, "--output", "hubway-tmp.html"])

    --     return _
  
    match "*.html" $ do
      route idRoute
      compile $ getResourceBody >>=
        loadAndApplyTemplate "templates/default.html" defaultContext

    -- create ["blog.html"] $ do
    --   route idRoute
    --   compile $ do
    --     posts <- loadAll "blog/*"

    --     let blogData =
    --           listField "posts" defaultContext (return posts) `mappend`
    --           constField "title" "Blog Archive"               `mappend`
    --           defaultContext

    --     makeItem ""
    --       >>= loadAndApplyTemplate "templates/blog.html" blogData
    --       >>= loadAndApplyTemplate "templates/default.html" blogData
    --       >>= relativizeUrls
-- Configuration
conf :: Configuration
conf = defaultConfiguration {
  providerDirectory = "content"
  , deployCommand   = "./src/deploy.sh"
  }

mdpost :: Rules ()
mdpost = do
  route   $ setExtension "html"
  compile $ pandocCompiler
    >>= loadAndApplyTemplate "templates/default.html" defaultContext
    >>= relativizeUrls

rawOut :: Rules ()
rawOut = do
  route   idRoute
  compile copyFileCompiler

-- elm :: 
