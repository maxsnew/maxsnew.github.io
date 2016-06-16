{-# LANGUAGE OverloadedStrings #-}
module Main where

import Hakyll

main :: IO ()
main = do
  hakyllWith conf $ do
    match "*.md" $ do
      route   $ setExtension "html"
      compile $ pandocCompiler
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls
  
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
  
-- Configuration
conf :: Configuration
conf = defaultConfiguration {
  providerDirectory = "content"
  , deployCommand   = "./src/deploy.sh"
  }
