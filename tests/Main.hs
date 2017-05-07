{-# LANGUAGE TemplateHaskell #-}
module Main where

import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Test.QuickCheck.Monadic

import Language.TorqueScript.Run

main :: IO ()
main = $(defaultMainGenerator)

tsTest :: TS Bool -> Property
tsTest m = monadicIO $ do
  cond <- run $ runTS m
  assert cond

prop_eachTaggedStringGetsId :: String -> String -> Property
prop_eachTaggedStringGetsId first second = tsTest $ do
  firstId <- storeTaggedString first
  secondId <- storeTaggedString second
  return $ (firstId == secondId) == (first == second)

prop_taggedStringsDontChange :: String -> [String] -> Property
prop_taggedStringsDontChange first rest = tsTest $ do
  firstId <- storeTaggedString first
  sequence_ $ storeTaggedString <$> rest
  secondId <- storeTaggedString first
  return $ firstId == secondId
