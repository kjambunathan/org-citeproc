{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           System.Environment
import           Text.CSL                         hiding (Citation, Cite (..))
import           Text.CSL.Pandoc
import           Text.JSON
import           Text.JSON.Generic
import           Text.JSON.Types                  (get_field)
import           Text.Pandoc.Definition           hiding (Cite)
import qualified Text.Pandoc.Definition           as PDD (Inline (Cite))
import           Text.Pandoc.Options
import           Text.Pandoc.Writers.HTML
import           Text.Pandoc.Writers.Markdown
import           Text.Pandoc.Writers.Native
import           Text.Pandoc.Writers.OpenDocument
import           Text.Pandoc.Writers.Org

import           Control.Applicative              ((<$>))
import           Control.Monad                    (foldM, unless, when)
import           Data.List                        (intersperse)
--import Text.Pandoc.Generic
import           Data.Set                         (empty)
import           System.Console.GetOpt
import           System.Exit
import           System.IO

--
-- INPUT PROCESSING
--
-- represents arrays of citeproc-js citation data JSON objects
data CitationsData =
  CitationsData [CitationData]
  deriving (Typeable, Data)

instance JSON CitationsData where
  showJSON (CitationsData cds) = JSArray $ map showJSON cds
  readJSON (JSArray cds) = Ok $ CitationsData cs
    where
      cs = reverse $ foldl getCitations [] cds
      getCitations acc obj =
        case readJSON obj of
          Ok cd@(CitationData _ _) -> cd : acc
          _                        -> acc -- TODO: error if non-citation in the array?
  readJSON x = fromJSON x

-- represents the citeproc-js citation data JSON object
data CitationData =
  CitationData
    { citationItems :: [Cite]
    , properties    :: DataProperties
    }
  deriving (Typeable, Data)

instance JSON CitationData where
  showJSON = toJSON
  readJSON (JSObject o) =
    case get_field o "citationItems" of
      Just (JSArray cs) ->
        Ok CitationData {citationItems = cis, properties = props}
        where getCites acc obj =
                case readJSON obj of
                  Ok c@(Cite {}) -> c : acc
                  _              -> acc -- TODO: error if non-citation in citationItems?
              cis = reverse $ foldl getCites [] cs
              props =
                case get_field o "properties" of
                  Just p ->
                    case readJSON p of
                      Ok ps@(DataProperties {}) -> ps
                      _                         -> defProps
                  _ -> defProps
              defProps =
                DataProperties
                  {noteIndex = 0, commonPrefix = [], commonSuffix = []}
      _ -> Error "Not a citations data cluster"
  readJSON x = fromJSON x

-- convenience accessors for CitationData properties:
getCPrefix :: CitationData -> [Inline]
getCPrefix = commonPrefix . properties

getCSuffix :: CitationData -> [Inline]
getCSuffix = commonSuffix . properties

-- represents properties of a citation data JSON object
data DataProperties =
  DataProperties
    { noteIndex    :: Int
                                     -- non-standard properties
                                     -- supported here for LaTeX-like
                                     -- multi-cite behavior:
    , commonPrefix :: [Inline]
    , commonSuffix :: [Inline]
    }
  deriving (Typeable, Data)

instance JSON DataProperties where
  showJSON = toJSON
  readJSON (JSObject o) =
    Ok DataProperties {noteIndex = n, commonPrefix = cpfx, commonSuffix = csfx}
    where
      n = 0 -- TODO
      asInlines field =
        case get_field o field of
          Just (JSString s) -> [Str $ fromJSString s]
          _                 -> []
      cpfx = asInlines "common-prefix"
      csfx = asInlines "common-suffix"
  readJSON x = fromJSON x

-- represents an individual work in a citation data JSON object
data Cite =
  Cite
    { citeId         :: String
    , citePrefix     :: [Inline]
    , citeSuffix     :: [Inline]
    , citeLabel      :: String
    , citeLocator    :: String
    , suppressAuthor :: Bool
    , authorInText   :: Bool
                 -- , itemData :: [ItemData] -- TODO
    , uris           :: [String]
    }
  deriving (Typeable, Data, Show)

instance JSON Cite where
  showJSON = toJSON
  readJSON (JSObject o) =
    case get_field o "id" of
      Just (JSString citeid) ->
        Ok
          Cite
            { citeId = fromJSString citeid
            , citePrefix =
                case get_field o "prefix" of
                  Just (JSString s) -> [Str $ fromJSString s]
                  _                 -> []
            , citeSuffix =
                case get_field o "suffix" of
                  Just (JSString s) -> [Str $ fromJSString s]
                  _                 -> []
            , citeLabel =
                case get_field o "label" of
                  Just (JSString x) -> fromJSString x
                  _                 -> ""
            , citeLocator =
                case get_field o "locator" of
                  Just (JSString x) -> fromJSString x
                  _                 -> ""
            , suppressAuthor =
                case get_field o "suppress-author" of
                  Just (JSBool True) -> True
                  _                  -> False
            , authorInText =
                case get_field o "author-in-text" of
                  Just (JSBool True) -> True
                  _                  -> False
            , uris =
                case get_field o "uris" of
                  Just (JSArray ss) -> us
                    where us = reverse $ foldl getUris [] ss
                          getUris acc obj =
                            case readJSON obj
                              -- TODO: convert to Links
                                  of
                              Ok (JSString s) -> fromJSString s : acc
                              _               -> acc -- TODO: error if non-strings in field?
                  _ -> []
                      -- TODO: itemData
                      -- See: https://raw.githubusercontent.com/citation-style-language/schema/master/csl-citation.json
                      -- https://raw.githubusercontent.com/citation-style-language/schema/master/csl-data.json
            }
      _ -> Error "Not a citation item"
  readJSON x = fromJSON x

-- global values needed by several functions
citeSep :: String
citeSep = "////\n" -- TODO: prefer something like "<!-- endCite -->"?

citeSepInline :: Inline
citeSepInline = Str citeSep

citeBibSep :: String
citeBibSep = "====\n" -- TODO: prefer something like "<!-- startBibliography -->"?

citeBibSepInline :: Inline
citeBibSepInline = Str citeBibSep

citeBibSepBlock :: Block
citeBibSepBlock = Plain [citeBibSepInline]

-- functions to transform input into a Pandoc
itemAsCitation :: Cite -> Citation
itemAsCitation i =
  Citation
    { citationId = citeId i
    , citationPrefix = citePrefix i
    , citationSuffix = citeSuffix i
    , citationMode =
        if authorInText i
          then AuthorInText
          else if suppressAuthor i
                 then SuppressAuthor
                 else NormalCitation
    , citationNoteNum = 0
    , citationHash = 0
    }

toPandocCite :: CitationData -> Inline
toPandocCite cd = PDD.Cite citas []
  where
    citas = map itemAsCitation $ citationItems cd

data MultiCite
  = InTextMulti CitationData
  | ParenMulti CitationData

multiCite :: MultiCite -> [Inline]
multiCite (InTextMulti cd) = group
  -- in the in-text case, we need to break up the CitationData into
  -- multiple individual records, one for each reference, and add the
  -- common prefix and suffix in the surrounding text
  where
    items = citationItems cd
    props = properties cd
    cpfx =
      case commonPrefix props of
        []   -> []
        inls -> inls ++ [Space]
    csfx =
      case commonSuffix props of
        []   -> []
        inls -> sep : inls
    asCd i = CitationData {citationItems = [i], properties = props}
    citas = map (toPandocCite . asCd) items
    sep = Str ", " -- TODO: grab separator from style?
    group = cpfx ++ intersperse sep citas ++ csfx
multiCite (ParenMulti cd) = [toPandocCite newCd]
  -- in the parenthetical case, we just need to prepend the common
  -- prefix to the prefix of the first item, and append the suffix to
  -- the suffix of the last item (which might be the same), then treat
  -- the resulting CitationData normally, as a single pandoc Citation
  where
    cpfx =
      case getCPrefix cd of
        []   -> []
        inls -> inls ++ [Space]
    csfx =
      case getCSuffix cd of
        []   -> []
        inls -> Str ", " : inls
    newItems =
      case citationItems cd of
        [] -> [] -- edge case: common prefix or suffix, but no items
                   -- (this case is prevented by Org's parser)
        [first] ->
          [ first
              { citePrefix = cpfx ++ citePrefix first
              , citeSuffix = citeSuffix first ++ csfx
              }
          ]
        (first:xs) ->
          [first {citePrefix = cpfx ++ citePrefix first}] ++
          init xs ++ [lst {citeSuffix = citeSuffix lst ++ csfx}]
          where lst = last xs
    newCd = cd {citationItems = newItems}

citationsAsPandoc :: [CitationData] -> Pandoc
citationsAsPandoc cds = Pandoc nullMeta citationBlocks
  where
    citationBlocks = foldr getBlocks [citeBibSepBlock] cds
        -- a citation is a `multi-cite' and needs to be handled
        -- specially when it has a common prefix or suffix, or when it
        -- contains only in-text references (and there are 2+).  In
        -- these cases, we behave like LaTeX:
    atLeastTwo xs = not (null xs) && not (null $ tail xs)
    multiInText cd =
      atLeastTwo (citationItems cd) && all authorInText (citationItems cd)
    hasCommons cd = not $ null $ getCPrefix cd ++ getCSuffix cd
    getBlocks cd acc
      | multiInText cd =
        (Plain $ multiCite (InTextMulti cd) ++ [citeSepInline]) : acc
      | hasCommons cd =
        (Plain $ multiCite (ParenMulti cd) ++ [citeSepInline]) : acc
      | otherwise = Plain [toPandocCite cd, citeSepInline] : acc

--
-- OUTPUT PROCESSING
--
-- output format selection
data OutputFormat
  = Ascii
  | Html
  | OpenDocument
  | Org
  | NativeBefore
  | NativeAfter
  deriving (Show, Ord, Eq)

chooseOutputFormat :: String -> OutputFormat
chooseOutputFormat s
  | s == "ascii" = Ascii
  | s == "html" = Html
  | s == "odt" = OpenDocument
  | s == "org" = Org
  | s == "native-before" = NativeBefore
  | s == "native" = NativeAfter
  | otherwise = error $ "Unknown output format: " ++ s

chooseRenderer :: OutputFormat -> Pandoc -> String
chooseRenderer fmt =
  case fmt of
    Ascii        -> renderPandocPlain
    Html         -> renderPandocHTML
    OpenDocument -> renderPandocODT
    Org          -> renderPandocOrg
    NativeBefore -> renderPandocNativeBefore
    NativeAfter  -> renderPandocNative

-- rendering functions:
-- plain text:
renderPandocPlain :: Pandoc -> String
renderPandocPlain = writePlain opts
  where
    opts =
      def
        { writerStandalone = False
        , writerTableOfContents = False
        , writerCiteMethod = Citeproc
        , writerWrapText = True -- TODO: don't break lines?
        , writerColumns = 80 -- TODO: adjustable?
        , writerExtensions = empty -- TODO: need any exts?
        }

-- HTML:
renderPandocHTML :: Pandoc -> String
renderPandocHTML = writeHtmlString opts
  where
    opts =
      def
        { writerStandalone = False
        , writerTableOfContents = False
        , writerCiteMethod = Citeproc
        , writerWrapText = False
        , writerSlideVariant = NoSlides
        }

-- ODT:
renderPandocODT :: Pandoc -> String
renderPandocODT (Pandoc _ blocks) = concatMap renderBlock blocks
  where
    asDoc inlines = Pandoc nullMeta [Plain inlines]
    transform i acc =
      case i of
        (PDD.Cite _ inls) -> inls ++ acc
        (Str s) ->
          if s == citeSep
            then acc -- remove cite separators inserted earlier
            else i : acc
        _ -> i : acc
    renderInlines inlines =
      writeOpenDocument opts $ asDoc $ foldr transform [] inlines
    renderBlock b =
      case b of
        (Div ("", ["references"], []) _) ->
          citeBibSep ++ writeOpenDocument opts (Pandoc nullMeta [b])
        (Plain inls) ->
          if inls == [citeBibSepInline]
            then "" -- remove bib separator inserted earlier
            else renderInlines inls ++ citeSep
        _ -> ""
    opts =
      def
        { writerStandalone = False
        , writerTableOfContents = False
        , writerCiteMethod = Citeproc
        , writerWrapText = False
        }

-- special case: we can't just use Pandoc's ODT writer directly here,
-- because it wraps Plain blocks in <text:p> tags.  This breaks our
-- extraction mechanism for individual citations and the bibliography
-- on the Org side.  We don't want to produce a complete ODT document,
-- but rather identifiable fragments that can be inserted into an ODT
-- document by Org; so here we take care to insert the separators
-- *outside* the ODT XML tags.
-- Org:
renderPandocOrg :: Pandoc -> String
renderPandocOrg (Pandoc m blocks) = writeOrg opts cleanDoc
  where
    opts =
      def
        { writerStandalone = False
        , writerTableOfContents = False
        , writerCiteMethod = Citeproc
        , writerSectionDivs = False
        , writerWrapText = False
        }
        -- the Org writer converts divs weirdly, wrapping them in
        -- BEGIN_HTML/END_HTML blocks...avoid this by extracting the
        -- list of bibliography blocks from a Div block
    cleanDoc = Pandoc m (foldr unwrapRefsBlock [] blocks)
    headline = Plain [Str "* References\n"]
    unwrapRefsBlock b acc =
      case b of
        (Div ("", ["references"], []) blks) -> (headline : blks) ++ acc
        _                                   -> b : acc

-- Native:
renderPandocNativeBefore :: Pandoc -> String
renderPandocNativeBefore = writeNative opts
  where
    opts = def {writerStandalone = False}

renderPandocNative :: Pandoc -> String
renderPandocNative = writeNative opts
  where
    opts =
      def
        { writerStandalone = False
        , writerTableOfContents = False
        , writerCiteMethod = Citeproc
        }

--
-- MAIN
--
main :: IO ()
main = do
  rawargs <- getArgs
  let (opts, args, errs) = getOpt Permute options rawargs
  unless (null errs) $ do
    mapM_ err errs
    exitWith $ ExitFailure 1
  opt <- foldM (flip ($)) defaultOpt opts

  when (optHelp opt) $ do
    putStr $ usageInfo "org-citeproc [OPTIONS] [BIB-FILES]" options
    exitSuccess
  when (optVersion opt) $ do
    putStrLn $ "citeproc version " ++ "0.1"
    exitSuccess

  let backend = id $ case optFormat opt of
                       Just "ascii"         ->  "ascii"
                       Just "html"          ->  "html"
                       Just "odt"           ->  "odt"
                       Just "native-before" ->  "native-before"
                       Just "native"        ->  "native"
                       Just "org"           ->  "org"
                       Just _               -> "html"
                       Nothing              ->  "html"

  bibfiles <- case args of
                   (x:_) -> mapM readBiblioFile args
                   _     -> err "No Bibfiles specified"

  jsonfile <- case optReferences opt of
                  Just fp ->  readFile fp
                  Nothing ->  err "No references"

  sty <- case optStyle opt of
                  Just fp -> readCSLFile Nothing fp
                  Nothing -> err "No style specified"

  let refs = concat bibfiles
  let Ok (CitationsData inputCitations) = decode jsonfile
  let doc = processCites sty refs $ citationsAsPandoc inputCitations
  putStrLn $ (chooseRenderer . chooseOutputFormat) backend doc

data Opt =
  Opt{ optStyle      :: Maybe String
     , optReferences :: Maybe String
     , optFormat     :: Maybe String
     , optHelp       :: Bool
     , optVersion    :: Bool
     } deriving Show

defaultOpt :: Opt
defaultOpt =
  Opt { optStyle = Nothing
      , optReferences = Nothing
      , optFormat = Nothing
      , optHelp = False
      , optVersion = False
      }

options :: [OptDescr (Opt -> IO Opt)]
options =
  [ Option ['s'] ["style"]
     (ReqArg (\fp opt -> return opt{ optStyle = Just fp }) "FILE")
     "CSL style file"
  , Option ['c'] ["citations"]
     (ReqArg (\fp opt -> return opt{ optReferences = Just fp }) "FILE")
     "Citations as JSON"
  , Option ['f'] ["format"]
     (ReqArg (\format opt -> return opt{ optFormat = Just format }) "ascii|html|native|native-before|odt|org")
     "Output format"
  , Option ['h'] ["help"]
     (NoArg (\opt -> return opt{ optHelp = True }))
     "Print usage information"
  , Option ['V'] ["version"]
     (NoArg (\opt -> return opt{ optVersion = True }))
     "Print version number"
  ]

err :: String -> IO a
err s = do
  hPutStrLn stderr s
  exitWith $ ExitFailure 1
