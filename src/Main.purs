module Main where

import Control.Monad.Aff (launchAff, liftEff')
--import Control.Monad.Aff.Console (logShow)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM, randomInt)
import DOM (DOM)
--import Data.Array (take)
import Deco (setBackground)
import Dictionary (readDictionary)
import GUI (mainGui)
import Prelude (Unit, bind, void, ($), (>>=))

main :: forall e. Eff (console :: CONSOLE, err :: EXCEPTION, dom :: DOM, random :: RANDOM | e) Unit
main = void $ launchAff do
  d <- readDictionary "simplingo.xlsx" -- "http://127.0.0.1:888/stactic/Simplingo.xlsx"
  -- logShow $ take 100 d
  -- log "Hello sailor!"
  liftEff' $ mainGui d
  -- log "Hello sailor!2"
  liftEff' $ randomInt 0 150 >>= setBackground
  -- log "Hello sailor!3"
