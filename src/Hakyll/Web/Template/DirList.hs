{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Hakyll.Web.Template.DirList
       (
         Configuration (..)
       , defaultConfiguration
       , metadataConfiguration
       , dirListField
       ) where

--import           Data.Monoid (mappend)
import           Control.Monad               (liftM)
import           Data.List                   (sortBy)
import           Data.Ord                    (comparing)
import           Hakyll          ( MonadMetadata, Identifier
                                 , Item, itemIdentifier, field
                                 , getMetadata, lookupString, listField
                                 , toFilePath, Metadata, Compiler, Context
                                 , splitAll)
import           System.FilePath ( dropExtensions
                                 , splitDirectories
                                 , takeBaseName)
import           Data.Maybe      ( fromMaybe)
import qualified Data.Map as M
import           Data.Default    (Default (..))

-- | Configuration for allowing variation of collection and
-- item tags in dependency of the hierarchical directory level.
-- The level count begins with zero. 
data Configuration = Configuration
  {
    -- | begin of item tag, default returns '<li>'
    beginItemTag :: Int -> String
    -- | end of item tag, default returns '</li>'
  , endItemTag :: Int -> String
    -- | begin of collection tag, default returns '<ul>' 
  , beginCollectionTag :: Int -> String
    -- | end of collection tag, default returns '</ul>' 
  , endCollectionTag :: Int -> String
  }

instance Default Configuration where
  def = defaultConfiguration

-- | Default configuration for dirListField
defaultConfiguration :: Configuration
defaultConfiguration = Configuration
  { beginItemTag = \_ -> "<li>"
  , endItemTag = \_ -> "</li>"
  , beginCollectionTag = \_ -> "<ul>"
  , endCollectionTag = \_ -> "</ul>"
  }

-- | Read Configuration from Metadata
--   the given default Configuration is used
--   if no coresponding metadata field is found.
--   The fields are: beginItemTag, endItemTag,
--   beginCollectionTag, endCollectionTag
--   These fields can hold a list of tags which are
--   separated by the value of the field tagDelimiter,
--   or if not given an ','. The list holds the
--   tags per level, if level is higher than the
--   tags in the list the last tag is used.
--   Use a double hyphen '--' to get an empty string as 'tag'.
metadataConfiguration :: Metadata -> Configuration -> Configuration
metadataConfiguration md default' =
  Configuration 
  ( f "beginItemTag" ( beginItemTag default' ) )
  ( f "endItemTag"   ( endItemTag default' ) )
  ( f "beginCollectionTag" ( beginCollectionTag default' ) )
  ( f "endCollectionTag" ( endCollectionTag default' ) )
  where
    del = fromMaybe "," (lookupString "tagDelimiter" md)
    f :: String -> ( Int -> String ) -> ( Int -> String )
    f tag df =
      case lookupString tag md of
        Nothing -> df
        Just s -> g ( splitAll del s )
    g :: [String] -> Int -> String
    g (x:[]) _ = t x
    g (x:_) 0 = t x
    g (_:xs) l = g xs (l - 1)
    g [] _ = "<???>" -- this should not happen
    t :: String -> String
    t x = if x == "--" then "" else x
  
-- | Sort pages alphabetically.
alphabetical :: MonadMetadata m => [Item a] -> m [Item a]
alphabetical =
    sortByM $ getItemPath . itemIdentifier
  where
    sortByM :: (Monad m, Ord k) => (a -> m k) -> [a] -> m [a]
    sortByM f xs = liftM (map fst . sortBy (comparing snd)) $
                   mapM (\x -> liftM (x,) (f x)) xs

-- | get the path of the item
getItemPath :: MonadMetadata m
           => Identifier        -- ^ Input page
           -> m FilePath        -- ^ Parsed UTCTime
getItemPath id' = return $ toFilePath id'
-- |                                             page-id order
data ItemTree a = ItemTree (Item a) [ItemTree a] String  String
type ItemPath a = ( Item a, [FilePath])

itemPath :: [ Item a] -> [ ItemPath a]
itemPath =
  map (\i -> ( i, splitDirectories
               . dropExtensions . toFilePath . itemIdentifier $ i) )
-- | get all files which belonging to one tree
--   this means the first file name (without extensions) is
--   equal to the base name of the following items.
--   return theses and the rest of the list

getTreeFiles :: [ ItemPath a] -> ( [ItemPath a], [ItemPath a])
getTreeFiles []   = ( [], [])
getTreeFiles (p:ps) 
  | (length $ snd p) > 1 = getTreeFiles ps  -- drop directories without
                                          -- an leading file of the same name
  | otherwise = getTreeFiles' ( head . snd $ p ) ([],p:ps)
    
-- |             baseName        TreeFiles Rest
getTreeFiles' :: FilePath -> ([ItemPath a], [ItemPath a])
                 -> ( [ItemPath a], [ItemPath a])
getTreeFiles' _ ( ts, [] ) = ( ts, [])
getTreeFiles' a ( ts, p:ps )
  | (head $ snd p ) == a = getTreeFiles' a ( ts ++ [p] , ps)
  | otherwise = ( ts, p:ps )

-- | build the tree, the input are only files belonging to this tree
-- | key is the order 
buildTree :: MonadMetadata m => String -> [ItemPath a] -> m (ItemTree a)
buildTree parentPid (p:ps)  = do
  pid <- getItemPageId id'
  ord <- getItemPageOrder id'
  tl <- buildOrderedTreeList (parentPid' ++ pid) ( map (\x->(fst x, tail $ snd x)) ps) 
  return $ ItemTree (fst p) tl (parentPid' ++ pid) ord
  where
    id' = itemIdentifier $ fst p
    parentPid' = if parentPid == "" then "" else parentPid ++ "-"   

buildTree _ [] = error "buildTree: empty file list"

-- -- | build the tree, the input are only files belonging to this tree
-- -- | key is the order 
-- buildTree :: MonadMetadata m => String -> [Iid <- getItemPageId id
--   ord <- getItemPageOrder id
--   tl <- buildOrderedTreeList (parentPid' ++ pid) ( map (\x->(fst x, tail $ snd x)) ps) 
--   return $ ItemTree (fst p) tl (parentPid' ++ pid) ord
--   where
--     id = itemIdentifier $ fst p
--     parentPid' = if parentPid == "" then "" else parentPid ++ "-"   
     
-- | build tree list
buildTreeList :: MonadMetadata m => String -> [ItemPath a] -> m [ItemTree a]
buildTreeList _ [] = return []
buildTreeList parentPid ps = do
  t <- buildTree parentPid ts
  tl <- buildTreeList parentPid rs
  return $ t : tl
  where ( ts, rs) = getTreeFiles ps

-- | sort the treeList
buildOrderedTreeList :: MonadMetadata m => String -> [ItemPath a] -> m [ItemTree a]
buildOrderedTreeList _ [] = return []
buildOrderedTreeList parentPid ps = do
  tl <- buildTreeList parentPid ps
  return $ sortBy (comparing (\( ItemTree _ _ _ o)-> o)) tl 

-- |                           pid    btags  etags
data TreeContext = TreeContext String String String deriving Show
  
getItemTreeAList :: Configuration -> ItemTree a -> Int
                 -> String -> String -> [(Item a, TreeContext)]
getItemTreeAList cfg ( ItemTree i [] pid _ ) level btags etags =
  [( i, TreeContext pid (btags ++ (beginItemTag cfg) level)
        ((endItemTag cfg) level ++ etags))]
getItemTreeAList cfg ( ItemTree i ts pid _ ) level btags etags =
  ( i, TreeContext pid (btags ++ (beginItemTag cfg) level) "")
  : (getItemTreeListAList cfg (level+1) ((endItemTag cfg) level ++ etags) ts)

--
callGetItemTreeListAList :: Configuration -> [ ItemTree a ]
                         -> [(Item a, TreeContext)]
callGetItemTreeListAList cfg = getItemTreeListAList cfg 0 ""
-- 
getItemTreeListAList :: Configuration -> Int -> String
                     -> [ ItemTree a ] -> [(Item a, TreeContext)]
getItemTreeListAList _ _ _ [] = []
getItemTreeListAList cfg level etags (t:[]) =    -- begin + end
    getItemTreeAList cfg t level
    ((beginCollectionTag cfg) level)
    ((endCollectionTag cfg) level ++ etags)  
getItemTreeListAList cfg level etags (t:ts) =    -- begin item
  (getItemTreeAList cfg t level ((beginCollectionTag cfg) level) "")
  ++ (getItemTreeListAList' cfg level
       ((endCollectionTag cfg) level ++ etags) ts) 

-- iteration until the end
getItemTreeListAList' :: Configuration -> Int -> String
                      -> [ ItemTree a ] -> [(Item a, TreeContext)]
getItemTreeListAList' _ _ _ [] = []
getItemTreeListAList' cfg level etags (t:[]) = -- the last item
  getItemTreeAList cfg t level "" etags 
getItemTreeListAList' cfg level etags (t:ts) = -- the items in the middle
  ( getItemTreeAList cfg t level "" "")
  ++ ( getItemTreeListAList' cfg level etags ts)
                          
-- metadata: page-id page-order
-- context:  full-page-id

getItemPageId :: MonadMetadata m => Identifier -> m String
getItemPageId id' = do
  metadata <- getMetadata id'
  return $ fromMaybe
    ( takeBaseName $ toFilePath id' )
    ( lookupString "page-id" metadata )

getItemPageOrder :: MonadMetadata m => Identifier -> m String
getItemPageOrder id' = do
  metadata <- getMetadata id'
  pageId <- getItemPageId id'
  return $ fromMaybe pageId ( lookupString "page-order" metadata)

{-|
  The exported 'dirListField' function is similar to the
  'Hakyll.Web.Template.listField' template function but creates
  additional context information which can be used in the template
  to create a hierarchical menu.

  == Context usable inside the template 

  [@$begin-tags$@]: injects @\<li\>@ and @\<ul\>@ tags if apropriate

  [@$end-tags$@]: contains the corresponding @\<\/li\>@ and @\<\/ul\>@ tags

  [@$full-page-id$@]: is the hyphen seperated path of the page

  == Metainformation in the source files
  For each subdirectory which should be processed one source file
  with the same base name should
  exist which can contain meta information:

  [@pages\/a.md@]:       top page for directory a

  [@pages\/a\/foo.md@]:   page foo within a

  The following meta information can be given

  [@page-id@]:          part of the generated id, if not given the base name of the file

  [@page-order@]:       give an ordering key for sorting in the current
                       directory level, if not given the @page-id@ will be used

-}
dirListField :: Configuration -> String -> Context a
             -> Compiler [Item a] -> Context b
dirListField cfg key c xs = listField key ( c' `mappend` c) pages' 
  where
    pages = alphabetical =<< xs
    treeList = (buildOrderedTreeList "") =<<
      map (\ip-> (fst ip, tail . snd $ ip)) <$> itemPath <$> pages
    aList = callGetItemTreeListAList cfg <$> treeList
    pages' = (map fst) <$> aList        -- the pages in the correct order
    aList' = map (\(item,ct)->(itemIdentifier item,ct)) <$> aList
    idMap  = M.fromList <$> aList'
    c' = -- ( field "test" ( \i -> M.showTree <$> idMap) ) `mappend`
      ( field "full-page-id"
        ( \i -> ( (\(Just (TreeContext pid _ _))->pid)
                  . ( M.lookup (itemIdentifier i) ) ) <$> idMap ) ) `mappend`
      ( field "begin-tags"
        ( \i -> ( (\(Just (TreeContext _ b _))->b)
                  . ( M.lookup (itemIdentifier i) ) ) <$> idMap ) ) `mappend`
      ( field "end-tags"
        ( \i -> ( (\(Just (TreeContext _ _ e))->e)
                  . ( M.lookup (itemIdentifier i) ) ) <$> idMap ) )


                      
                    
