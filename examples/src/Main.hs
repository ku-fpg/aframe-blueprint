{-# LANGUAGE  RankNTypes, FlexibleInstances, UndecidableInstances, OverloadedStrings #-}

module Main where

import Text.AFrame
import Text.AFrame.WebPage
import Text.AFrame.DSL 

import System.Environment

import qualified HelloWorld
import qualified LookAt

main :: IO ()
main = do
        HelloWorld.main
        LookAt.main
