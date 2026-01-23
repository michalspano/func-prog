-- * Don't Touch
-- / Michal

module Secret (mayHaveFreeDrink,format) where

import Test.QuickCheck

format k str     = show k ++ ".) " ++ str
mayHaveFreeDrink = (==) expectedResult

data Activity = W -- | Working out
              | H -- | Hiking
              | R -- | Reading books
              | L -- | Learning a new language
              deriving (Show,Eq,Enum)

instance Arbitrary Activity where
    arbitrary = oneof $ map return [W .. L]

newtype Schedule = Schedule [Activity] deriving Eq

instance Show Schedule where
    show (Schedule as) = concatMap show as

makeSchedule = Schedule <$> vectorOf 24 arbitrary

-- | very secret
expectedResult = 46
