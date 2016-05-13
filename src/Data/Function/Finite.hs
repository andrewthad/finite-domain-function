module Data.Function.Finite 
  ( FiniteDomainFunction(..)
  , unsafeVectorToFiniteDomainFunction
  ) where

import Data.HashMap.Strict (HashMap)
import Data.Vector (Vector)
import Control.Monad
import Data.Aeson (FromJSON(..),ToJSON(..))
import Data.Text (Text)
import Control.Monad.ST
import Data.STRef

import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector.Mutable as MVector

-- | A type that wraps a function to provide instances so that it
--   can be compared and serialized.
newtype FiniteDomainFunction a b = FiniteDomainFunction { appFiniteDomainFunction :: a -> b }

instance (Bounded a, Enum a, Eq b) => Eq (FiniteDomainFunction a b) where
  FiniteDomainFunction f == FiniteDomainFunction g =
    all ((==) <$> f <*> g) [minBound..maxBound]
  
instance (Bounded a, Enum a, Ord b) => Ord (FiniteDomainFunction a b) where
  compare (FiniteDomainFunction f) (FiniteDomainFunction g) =
    foldMap (compare <$> f <*> g) [minBound..maxBound]

-- | Replace the @Show a@ constraint with a @ToJSONKey a@ constraint once @aeson-0.12@
--   is released.
instance (ToJSON b, Show a, Enum a, Bounded a) => ToJSON (FiniteDomainFunction a b) where
  toJSON (FiniteDomainFunction f) = toJSON $ HashMap.fromList 
    $ flip map [minBound..maxBound] $ \a -> 
      (Text.pack $ show a, f a)

-- | Replace the @Show a@ constraint with a @ToJSONKey a@ constraint once @aeson-0.12@
--   is released.
instance (Show a, Bounded a, Enum a, FromJSON b) => FromJSON (FiniteDomainFunction a b) where
  parseJSON v = do
    m <- parseJSON v
    case hashmapToFiniteDomainVector (Text.pack . show) m of
      Nothing -> fail "FiniteDomainFunction: a key is missing"
      Just f -> return f

hashmapToFiniteDomainVector :: (Bounded a, Enum a) => (a -> Text) -> HashMap Text b -> Maybe (FiniteDomainFunction a b)
hashmapToFiniteDomainVector render m = runST $ do
  v <- MVector.new (length allKeys)
  success <- newSTRef True
  forM_ allKeys $ \a -> 
    case HashMap.lookup (render a) m of
      Just b -> MVector.unsafeWrite v (fromEnum a) b
      Nothing -> writeSTRef success False
  didSucceed <- readSTRef success
  if didSucceed
     then fmap (Just . unsafeVectorToFiniteDomainFunction) (Vector.unsafeFreeze v)
     else return Nothing
  where allKeys = [minBound..maxBound]

unsafeVectorToFiniteDomainFunction :: Enum a => Vector b -> FiniteDomainFunction a b
unsafeVectorToFiniteDomainFunction v = 
  FiniteDomainFunction (Vector.unsafeIndex v . fromEnum)



