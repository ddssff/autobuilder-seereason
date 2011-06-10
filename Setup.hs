#!/usr/bin/env runghc

module Main where

import Distribution.Simple
import Distribution.Simple.Program

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
