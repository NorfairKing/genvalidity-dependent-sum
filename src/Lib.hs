{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Lib
    ( someFunc
    ) where

import Data.Dependent.Map
import Data.Dependent.Sum
import Data.Functor.Identity
import Data.GenRelativeValidity
import Data.GenValidity
import Test.QuickCheck

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Tag a where
    AString :: Tag String
    AnInt :: Tag Int

data TagBox f where
    TagBox :: f a -> TagBox f

class GADTGenUnchecked f where
    gadtGenUnchecked :: Gen (TagBox f)

instance GADTGenUnchecked Tag where
    gadtGenUnchecked = elements [TagBox AString, TagBox AnInt]

class GenTagUnchecked tag f where
    genTagUnchecked :: tag a -> Gen (f a)

instance GenTagUnchecked Tag Identity where
    genTagUnchecked AString = Identity <$> genUnchecked
    genTagUnchecked AnInt = Identity <$> genUnchecked

instance (GADTGenUnchecked tag, GenTagUnchecked tag f) =>
         GenUnchecked (DSum tag f) where
    genUnchecked = do
        tb <- gadtGenUnchecked @tag
        let TagBox tag = tb
        funct <- genTagUnchecked (tag :: tag a)
        pure $ tag :=> (funct :: f a)
    shrinkUnchecked _ = []
-- class GenTagUnchecked tag f where
--     genTagUnchecked  :: TagBox f => Gen
--
-- instance (GenUncheckedTag tag f) => GenUnchecked (DSum tag f) where
--     genUnchecked = do
--         tag <- genUncheckedTag
--         funct <- genRelativeTag
--         pure $ tag :=> funct
--
-- class GenUncheckedTag tag where
--     genUncheckedTag :: Gen (tag a)
--     genRelativeTag :: tag a -> Gen (f a)
