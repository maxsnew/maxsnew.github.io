{-# LANGUAGE OverloadedStrings #-}
import Hakyll
import Hakyll.Web.Elm

main :: IO ()
main = hakyllWith conf $ do
  match "*.md" $ do
    route   $ setExtension "html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/default.html" defaultContext
      >>= relativizeUrls

  match "*.elm" $ do
    route   $ setExtension "html"
    compile $ elmStandaloneCompiler
      >>= loadAndApplyTemplate "templates/default.html" defaultContext
      -- >>= relativizeUrls

  match "js/*.js" $ do
    route   idRoute
    compile copyFileCompiler

  match "css/*.css" $ do
    route   idRoute
    compile compressCssCompiler

  match "templates/*" $ compile templateCompiler

-- Configuration
conf :: Configuration
conf = defaultConfiguration {
  providerDirectory = "content"
  , deployCommand   = "./src/deploy.sh"
  }
