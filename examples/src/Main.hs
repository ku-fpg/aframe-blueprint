{-# LANGUAGE  RankNTypes, FlexibleInstances, UndecidableInstances, OverloadedStrings #-}

module Main where

import Text.AFrame
import Text.AFrame.WebPage
import Text.AFrame.DSL 

import System.Environment

import qualified HelloWorld

main :: IO ()
main = do
        args <- getArgs
        main2 args

main2 :: [String] -> IO ()
main2 ["hello-world",arg] = webPage arg [] HelloWorld.theScene
main2 _ = error "usage: aframe-blueprint-example example-name output.html"
