{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Lib where

import Data.Dependent.Map
import Data.Dependent.Sum
import Data.Functor.Identity
import Data.GADT.Show
import Data.GenRelativeValidity
import Data.GenValidity
import Test.QuickCheck

type GGen t = Gen (GGenResult t)

newtype GGenResult t = GGenResult
    { getGGenResult :: forall b. (forall a. t a -> b) -> b
    }

class GADTGenUnchecked f where
    gadtGenUnchecked :: GGen f

class GenTagUnchecked tag f where
    genTagUnchecked :: tag a -> Gen (f a)

instance (GADTGenUnchecked tag, GenTagUnchecked tag f) =>
         GenUnchecked (DSum tag f) where
    genUnchecked = do
        genResult <- gadtGenUnchecked
        getGGenResult genResult $ \tag -> do
            funct <- genTagUnchecked tag
            pure $ tag :=> funct
    shrinkUnchecked _ = []

---------------
--- EXAMPLE ---
---------------
data Tag a where
    AString :: Tag String
    AnInt :: Tag Int

instance GADTGenUnchecked Tag where
    gadtGenUnchecked =
        elements [GGenResult (\x -> x AString), GGenResult (\x -> x AnInt)]

instance GenTagUnchecked Tag Identity where
    genTagUnchecked AString = Identity <$> genUnchecked
    genTagUnchecked AnInt = Identity <$> genUnchecked

instance GShow Tag where
    gshowsPrec _ AString = showString "AString"
    gshowsPrec _ AnInt = showString "AnInt"

instance ShowTag Tag Identity where
    showTaggedPrec AString _ (Identity s) = showString s
    showTaggedPrec AnInt i (Identity int) = showsPrec i int

someFunc :: IO ()
someFunc = sample (genUnchecked :: Gen (DSum Tag Identity))
-- instance (GADTGenUnchecked tag, GenTagUnchecked tag f) =>
--          GenUnchecked (DSum tag f) where
--     genUnchecked = do
--         tb <- gadtGenUnchecked @tag
--         let TagBox tag = tb
--         funct <- genTagUnchecked (tag :: tag a)
--         pure $ tag :=> (funct :: f a)
--     shrinkUnchecked _ = []
--
-- type ReadS a = String -> [(a , String)]
-- type GReadS t = String -> [(GReadResult t, String)]
-- newtype GReadResult t = forall b (forall a. t a -> b) - >b
