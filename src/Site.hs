{-# LANGUAGE OverloadedStrings #-}
module Main where

import Hakyll

main :: IO ()
main = do
  hakyllWith conf $ do
    match "*.md" mdpost
    match "blog/*.md" mdpost

    match "js/*.js" $ do
      route   idRoute
      compile copyFileCompiler
  
    match "css/*.css" $ do
      route   idRoute
      compile compressCssCompiler

    match "*.pdf" $ do
      route   idRoute
      compile copyFileCompiler
  
    match "templates/*" $ compile templateCompiler

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
