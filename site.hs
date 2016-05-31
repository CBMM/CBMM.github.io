--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "favicon.ico" $ do
      route idRoute
      compile copyFileCompiler

    match "js/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/**" $ do
        route   idRoute
        compile compressCssCompiler

    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    tagsRules tags $ \tag pattern -> do
      let title = "Posts tagged \"" ++ tag ++ "\""
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll pattern
        let ctx = constField "title" title `mappend`
                  listField "posts" (postCtx tags) (return posts) `mappend`
                  defaultContext
        makeItem ""
          >>= loadAndApplyTemplate "templates/tag.html" ctx
          >>= loadAndApplyTemplate "templates/default.html" ctx
          >>= relativizeUrls


    match "about.markdown" $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html"
                (defaultContext `mappend` constField "page-about" "")
            >>= relativizeUrls

    match "projects.markdown" $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html"
                (defaultContext `mappend` constField "page-projects" "")
            >>= relativizeUrls

    match "people.md" $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html"
                (defaultContext `mappend` constField "page-people" "")
            >>= relativizeUrls


    match "contact.markdown" $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html"
                (defaultContext `mappend` constField "page-contact" "")
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/post.html"    (postCtx tags)
            >>= saveSnapshot "feedcontent"
            >>= loadAndApplyTemplate "templates/default.html" (postCtx tags)
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAllSnapshots "posts/*" "content"
            let archiveCtx =
                    listField "posts" (teaserCtx tags) (return posts) `mappend`
                    constField "page-archive" ""             `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAllSnapshots "posts/*" "content"
            let indexCtx =
                    listField "posts" (teaserCtx tags) (return posts) `mappend`
                    constField "page-home" ""                  `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler

    create ["atom.xml"] $ do
      route idRoute
      compile $ do
        let feedCtx = postCtx tags `mappend` bodyField "description"
        posts <- fmap (take 10) . recentFirst =<<
          loadAllSnapshots "posts/*" "feedcontent"
        renderAtom cbmmFeedConfig feedCtx posts

    create ["rss.xml"] $ do
      route idRoute
      compile $ do
        let feedCtx = postCtx tags `mappend` bodyField "description"
        posts <- fmap (take 10) . recentFirst =<<
          loadAllSnapshots "posts/*" "feedcontent"
        renderRss cbmmFeedConfig feedCtx posts



--------------------------------------------------------------------------------
postCtx :: Tags -> Context String
postCtx tags =
    dateField "date" "%B %e, %Y" `mappend`
    tagsField "tags" tags `mappend`
    constField "page-blog" "" `mappend`
    defaultContext

teaserCtx tags = teaserField "teaser" "content" `mappend` postCtx tags

cbmmFeedConfig :: FeedConfiguration
cbmmFeedConfig = FeedConfiguration
  { feedTitle = "CBMM @ GitHub"
  , feedDescription = "The open-source going ons of CBMM"
  , feedAuthorName = "Greg Hale"
  , feedAuthorEmail = "imalsogreg@gmail.com"
  , feedRoot = "https://cbmm.github.io"
  }
