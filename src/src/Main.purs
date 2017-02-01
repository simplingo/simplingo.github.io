module Main where

import Prelude (Unit, bind, void, ($), (>>=))
import Control.Monad.Aff (launchAff, liftEff')

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM, randomInt)
import DOM (DOM)

import Dictionary (readDictionary)
import Deco (setBackground)
import GUI (mainGui)

main :: forall e. Eff (console :: CONSOLE, err :: EXCEPTION, dom :: DOM, random :: RANDOM | e) Unit
main = void $ launchAff do
  d <- readDictionary "simplingo.xlsx" -- "http://127.0.0.1:888/stactic/Simplingo.xlsx"
  -- logShow $ take 10 d
  -- log "Hello sailor!"
  liftEff' $ mainGui d
  -- log "Hello sailor!2"
  liftEff' $ randomInt 0 150 >>= setBackground
  -- log "Hello sailor!3"
