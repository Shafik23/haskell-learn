{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty (ScottyM, get, html, param, post, scotty)

main :: IO ()
main = scotty 3000 $ displayWord >> postWord

displayWord :: ScottyM ()
displayWord = get "/:word" $ do
  beam <- param "word"
  html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]

postWord :: ScottyM ()
postWord = post "/test" $ do
  html "POSTED"