{-# LANGUAGE JavaScriptFFI #-}

module Github where

import qualified Data.Aeson as A

{- foreign import javascript unsafe "var g = new Github({user: 'imalsogreg', password: ''});\
                                 \var r = g.getRepo('imalsogreg', 'pipes-rt');\
                                 \r.getCommits('master', function(err,commits){ console.log(commits); $r = commits; });"
  getCommits :: A.Value
-}
