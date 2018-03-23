--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
import           Hakyll
import           Hakyll.Web.Template.DirList as DL

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do

    -- Compile page elements
    match "pages/**.md" $ do
      compile pandocCompiler

    match ( "templates/*" .&&.
            complement "templates/index.html" ) $
      compile templateCompiler
    
    match "templates/index.html" $ do
      route $ constRoute "index.html"
      compile $ do
        md <- getMetadata =<< getUnderlying
        pages <- loadAll "pages/**"
        let indexCtx =
              DL.dirListField
                (DL.metadataConfiguration md DL.defaultConfiguration)
                "pages" defaultContext
                (return pages)
              `mappend` defaultContext
              
        getResourceBody
          >>= applyAsTemplate indexCtx
          >>= relativizeUrls

