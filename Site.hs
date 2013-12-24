{-# LANGUAGE OverloadedStrings #-}
import Hakyll

main :: IO ()
main = hakyll $ do
  match "index.md" $ do
    route $ setExtension "html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/default.html" defaultContext
      >>= relativizeUrls

  match "templates/*" $ compile templateCompiler
