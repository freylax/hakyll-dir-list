--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
import           Hakyll
import           Hakyll.Web.Template.DirList

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do

    -- Compile page elements
    match "pages/**.md" $ do
      compile pandocCompiler

    match "templates/index.html" $ compile templateCompiler
    
    create ["index.html"] $ do
      route idRoute
      compile $ do
        pages <- loadAll "pages/**" 
        let indexCtx =
              dirListField "pages" defaultContext (return pages)
              `mappend` defaultContext
              
        makeItem ""
          >>= loadAndApplyTemplate "templates/index.html" indexCtx
          >>= relativizeUrls

--------------------------------------------------------------------------------

