module HFish.Interpreter.Env (
  Env(..)
  ,asHashMap
  ,empty
  ,toList
  ,fromList
  ,fromTextList
  ,lookup
  ,lookupDefault
  ,union
  ,update
  ,alter
  ,adjust
  ,insert
  ,delete
  ,filter
  ,filterWithKey
  ,identifiers
) where

import Prelude hiding (lookup,filter)

import Data.NText

import Data.Bifunctor
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Control.Lens


-- | The type of an /environment/, mapping identifiers to
--   their values.
newtype Env a = Env
  { extractEnv :: HM.HashMap NText a }

isoHM :: Iso' (Env a) (HM.HashMap NText a)
isoHM = iso extractEnv Env

asHashMap :: Env a -> (HM.HashMap NText a -> b) -> b
asHashMap (Env env) k = k env

onHashMap :: (HM.HashMap NText a -> b) -> Env a -> b
onHashMap = flip asHashMap

liftHashMap :: (HM.HashMap NText a -> HM.HashMap NText b)
  -> Env a -> Env b
liftHashMap f = onHashMap (Env . f)

liftHashMap2 ::
  ( HM.HashMap NText a
  -> HM.HashMap NText b
  -> HM.HashMap NText c )
  -> Env a -> Env b -> Env c
liftHashMap2 f e1 e2 = Env
  $ asHashMap e2
  $ asHashMap e1 f

empty :: Env a
empty = Env (HM.empty)

toList :: Env a -> [(NText, a)]
toList = onHashMap HM.toList

fromList :: [(NText, a)] -> Env a
fromList = Env . HM.fromList

fromTextList :: [(T.Text, a)] -> Env a
fromTextList = fromList . map (first mkNText)

lookup :: Env a -> NText -> Maybe a
lookup env text = asHashMap env (HM.lookup text)

lookupDefault :: b -> NText -> Env b -> b
lookupDefault v key env = asHashMap env $ HM.lookupDefault v key

update :: (b -> Maybe b) -> NText -> Env b -> Env b
update f key = liftHashMap $ HM.update f key

alter :: (Maybe b -> Maybe b) -> NText -> Env b -> Env b
alter f key = liftHashMap $ HM.alter f key

insert :: NText -> b -> Env b -> Env b
insert key v = liftHashMap $ HM.insert key v

adjust :: (b -> b) -> NText -> Env b -> Env b
adjust f key = liftHashMap $ HM.adjust f key

delete :: NText -> Env a -> Env a
delete key = liftHashMap $ HM.delete key

filter :: (a -> Bool) -> Env a -> Env a
filter f = liftHashMap $ HM.filter f

filterWithKey :: (NText -> b -> Bool) -> Env b -> Env b
filterWithKey f = liftHashMap $ HM.filterWithKey f

identifiers :: Env a -> [NText]
identifiers = onHashMap HM.keys

union :: Env a -> Env a -> Env a
union = liftHashMap2 HM.union
