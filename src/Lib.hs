{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( site
    ) where

import Hakyll

import Control.Monad
import Control.Monad.Except
import Data.Aeson.Types (typeMismatch)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import Data.Traversable
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import Data.Yaml ((.:), (.:?), FromJSON)
import System.FilePath
import System.Process

site :: IO ()
site = do
  hakyllWith conf $ do
    match "*.md"  $ textPost defaultTemplate
    match "blog.html" $ do
      route idRoute
      compile $
        getResourceBody
          >>= applyAsTemplate blogsCtx
          >>= loadAndApplyTemplate "templates/default.html" blogsCtx
          >>= relativizeUrls


    match "js/*.js" $ do
      route   idRoute
      compile copyFileCompiler
  
    match "css/*.css" $ do
      route   idRoute
      compile compressCssCompiler

    forM_ ["docs/*", "img/*", "teaching/*/docs/*"] $ \p -> 
      match p rawOut
  
    match "templates/*" $ compile templateCompiler

    match "blog/*" $ do
      route $ setExtension "html"
      compile $ pandocCompiler
        >>= loadAndApplyTemplate blogPostTemplate blogPostCtx
        >>= loadAndApplyTemplate defaultTemplate blogPostCtx
        >>= relativizeUrls

    match "*.html" $ do
      route idRoute
      compile $ getResourceBody >>=
        loadAndApplyTemplate "templates/default.html" defaultContext

    match "publications.yaml" $ do
      route $ setExtension "html"
      compile $ yamlCompiler
                >>= loadAndApplyTemplate pubsTemplate groupedPubsContext
                >>= loadAndApplyTemplate defaultTemplate defaultContext


    forM_ [ "teaching/eecs-483-fa21/*"
          , "teaching/eecs-483-fa22/*"
          , "teaching/eecs-483-fa23/*"
          , "teaching/eecs-483-wi24/**" ] $ \p ->
      match p $ do
        route idRoute
        compile copyFileCompiler
    -- match "teaching/eecs-483-fa22/*" $ do
    --   route idRoute
    --   compile copyFileCompiler
    -- match "teaching/eecs-483-fa23/*" $ do
    --   route idRoute
    --   compile copyFileCompiler

    -- Add this for winter...
    -- match "teaching/eecs-483-wi23/*" $ do
    --   route idRoute
    --   compile copyFileCompiler

    match "teaching/eecs-598-w22/index.md" $ textPost classTemplate
    match "teaching/eecs-598-w23/index.md" $ textPost classTemplate

    -- match "ps/src/Main.purs" $ do
    --   route   $ constRoute "js/hubway.js"
    --   compile $ psCompiler


blogPostTemplate = "templates/blog-post.html"
defaultTemplate = "templates/default.html"
pubsTemplate = "templates/publications.html"
classTemplate = "templates/class.html"

        
-- Configuration
conf :: Configuration
conf = defaultConfiguration {
  providerDirectory = "content"
  , deployCommand   = "./src/deploy.sh"
  }

textPost :: Identifier -> Rules ()
textPost templ = do
  route   $ setExtension "html"
  compile $ pandocCompiler
    >>= loadAndApplyTemplate templ defaultContext
    >>= relativizeUrls

rawOut :: Rules ()
rawOut = do
  route   idRoute
  compile copyFileCompiler

yamlCompiler :: (FromJSON a) => Compiler (Item a)
yamlCompiler = do
  path <- getResourceFilePath
  rawItem <- getResourceLBS
  for rawItem $ \raw ->
    case Yaml.decodeEither' . LBS.toStrict $ raw of
      Left err     -> throwError ["Failed to parse " <> path <> " : " <> show err]
      Right parsed -> return parsed

-- | Purescript

psCompiler :: Compiler (Item String)
psCompiler = do
  psFile <- getResourceFilePath
  let psBaseDir = takeDirectory . takeDirectory $ psFile
  js <- unsafeCompiler $ do
    _ <- flip readCreateProcess "" . shell . List.intercalate " && " $
      [ "cd " <> psBaseDir
      , "bower install"
      , "pulp build --to " <> tmpFile
      ]
    readFile $ psBaseDir </> tmpFile
  makeItem js
  where
    tmpFile = "tmp.js"

-- | Publications
data Publication = Publication { pTitle :: Text
                               , pAuthors :: Text
                               , pId :: Text
                               , pVenue :: Maybe Text
                               , pLinks :: Map Text Text
                               , pManuscript :: Bool
                               , pAbstract :: Bool
                               , pPreprint :: Bool
                               , pNote :: Maybe Text
                               }
instance FromJSON Publication where
  parseJSON (Yaml.Object v) = 
    Publication      <$>
    v .: "title"     <*>
    v .: "authors"   <*>
    v .: "id"        <*>
    v .:? "venue"    <*>
    (withDefault mempty $ v .:? "links")      <*>
    (withDefault False  $ v .:? "manuscript") <*>
    (withDefault False  $ v .:? "abstract")   <*>
    (withDefault False  $ v .:? "preprint")   <*>
    v .:? "note"
    where withDefault d = fmap (fromMaybe d)
  parseJSON invalid = typeMismatch "Publication" invalid

pField :: String -> (a -> Text) -> Context a
pField s m = field s (return . T.unpack . m . itemBody)

mField :: String -> (a -> Maybe Text) -> Context a
mField s m = field s (fromMay . m . itemBody)
  where fromMay Nothing  = fail ("missing field " ++ s)
        fromMay (Just t) = return . T.unpack $ t

linkContext :: Context (Text, Text)
linkContext =  pField "url" snd
            <> pField "text" fst

pubContext :: Context Publication
pubContext =  pField "title" pTitle
           <> pField "id" pId
           <> pField "authors" pAuthors
           <> mField "venue" pVenue
           <> listFieldWith "links" linkContext (return . sequenceA . fmap (Map.toList . pLinks))
           <> mField "note" pNote

pubsContext :: Context (Text, [Publication])
pubsContext =
  pField "group-name" fst
  <> listFieldWith "papers" pubContext (return . traverse snd)

groupedPubsContext :: Context [Publication]
groupedPubsContext = listFieldWith "groups" pubsContext (return . traverse separate)
  where -- foo :: Item [Publication] -> Compiler [Item (Text, [Publication])]
        separate pubs = let (manus, notManus) = List.partition pManuscript pubs
                            (absPubs, notAbsPubs) = List.partition pAbstract notManus
                            (prePubs, paperPubs)  = List.partition pPreprint notAbsPubs
                        in filter (not . null . snd)
                             [ ("Manuscripts", manus)
                             , ("Peer-Reviewed Papers", paperPubs)
                             , ("Drafts", prePubs)
                             , ("Abstracts", absPubs) ]

blogPostCtx :: Context String
blogPostCtx =
    dateField "date" "%B %e, %Y"
    <> defaultContext

blogsCtx =
  listField "posts" blogPostCtx mkPosts
  <> constField "title" "Blog"
  <> defaultContext

mkPosts = recentFirst =<< loadAll "blog/*"
