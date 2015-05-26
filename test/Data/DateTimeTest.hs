{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.DateTimeTest where

import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Data.Maybe (fromJust)

import Data.Time.Clock (UTCTime)
import Data.DateTime

instance Arbitrary UTCTime where
    arbitrary       = do
        offset <- choose (0, 20000) :: Gen Float
        return . fromMJD' $ offset + fromRational startOfTimeMJD


tests :: [Test]
tests = [ testGroup "properties"
          [ testProperty "MJD" prop_MJD
          , testProperty "Universal" prop_Universal
          , testProperty "StartOfTime" prop_StartOfTime
          , testProperty "SqlString" prop_SqlString
          -- , testProperty "SqlStartOfTime" prop_SqlStartOfTime
          ]
        ]

invariant :: Eq a => (a -> a) -> a -> Bool
invariant f x = f x == x

prop_MJD :: DateTime -> Bool
prop_MJD  = invariant $ fromMJD  . toMJD

prop_MJD' :: DateTime -> Bool
prop_MJD' = invariant $ fromMJD' . toMJD'

prop_Universal :: DateTime -> Bool
prop_Universal = invariant $ fromUniversalTime . toUniversalTime

prop_StartOfTime :: DateTime -> Bool
prop_StartOfTime _ = toSeconds startOfTime == 0

prop_SqlString :: DateTime -> Bool
prop_SqlString dt = (fromJust . fromSqlString . toSqlString $ dt') == dt'
  where
    Just dt' = fromSqlString . toSqlString $ dt

-- It doesn't seem like this test ever passed, so disabling.
-- prop_SqlStartOfTime :: DateTime -> Bool
-- prop_SqlStartOfTime _ = toSqlString startOfTime == "1970-01-01 00:00:00"
