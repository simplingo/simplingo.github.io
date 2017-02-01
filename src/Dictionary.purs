module Dictionary
( readDictionary
, Dictionary(..)
, queryDict
) where

import Prelude (class Show, Unit, bind, pure, show, ($), (<>), (=<<))
import Data.Foreign (Foreign)
import Data.Foreign.Class (class IsForeign, read, readProp)

import Data.Generic (class Generic, gShow)
import Data.Maybe (Maybe(..))
import Data.Foreign.Undefined (readUndefined, unUndefined)
import Data.Array (filter, partition)
import Data.Function.Uncurried (Fn2, runFn2)
import Control.Monad.Aff (Aff, makeAff)
import Data.Either (Either(..))
import Data.String (Pattern(..), charAt, contains, indexOf)

import Control.Monad.Eff (Eff)

import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExcept)



foreign import readDictionaryImpl
  :: forall eff
  .  Fn2 String
         (Foreign -> Eff eff Unit)
         (Eff eff Unit)

readDictionary
  :: forall eff
  .  String
  -> Aff eff (Array Dictionary)
readDictionary filename = do
  raw <- makeAff (\err succ -> runFn2 readDictionaryImpl filename succ)
  case runExcept $ read raw of
    Left e -> throwError $ error $ show e
    Right result -> pure result

data Dictionary = Dictionary
  { spell :: String
  , root :: Maybe String
  , meanning :: String
  }

derive instance genericDictionary :: Generic Dictionary

instance dictionaryShow :: Show Dictionary where
  show = gShow

instance dictionaryForeig :: IsForeign Dictionary where
  read raw = do
    s <- readProp "Simplingo" raw
    -- r <- readUndefined (readProp "词根") raw
    r <- readUndefined read =<< readProp "Root" raw
    m <- readProp "Explanation" raw
    pure $ Dictionary
      { spell: s
      , root:  unUndefined r
      , meanning: m
      }

queryDict :: String -> Array Dictionary -> Array Dictionary
queryDict q dict = p1.yes <> p2.yes <> p3.yes <> p4 where
  inMeanning (Dictionary d) = contains (Pattern q) d.meanning
  inSpell (Dictionary d) = contains (Pattern q) d.spell
  inFirstSpell (Dictionary d) = case indexOf (Pattern q) d.spell of
    Nothing -> false
    Just 0 -> true
    _ -> case charAt 0 d.spell of
      Just '/' -> true
      _ -> false
  inRoot (Dictionary d)  = case d.root of
    Nothing -> false
    Just dd -> contains (Pattern q) dd
  p1 = partition inFirstSpell dict
  p2 = partition inRoot p1.no
  p3 = partition inMeanning p2.no
  p4 = filter inSpell p3.no
