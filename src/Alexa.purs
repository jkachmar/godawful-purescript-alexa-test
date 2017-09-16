module Alexa
  ( ALEXA
  , Alexa
  , Event
  , Context
  , This
  , handler
  ) where

import Prelude

import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Uncurried (EffFn1, EffFn2, EffFn3, mkEffFn1, runEffFn1, runEffFn2, runEffFn3)
import Data.Newtype (class Newtype, unwrap)

type AlexaEffects eff = (alexa :: ALEXA, console :: CONSOLE | eff)

-- | Type of effects performed by Alexa handlers, and associated types used
-- | by the SDK.
foreign import data ALEXA   :: Effect
foreign import data Alexa   :: Type
foreign import data Event   :: Type
foreign import data Context :: Type
foreign import data This    :: Type

-- | Newtype wrappers for common arguments

newtype IntentLabel = IntentLabel String
derive instance ntIntentLabel :: Newtype IntentLabel _

newtype Say = Say String
derive instance ntSay :: Newtype Say _

newtype Listen = Listen String
derive instance ntListen :: Newtype Listen _

-- | Given an `Event` and `Context` from AWS Lambda, return an initialized
-- | Alexa handler.
foreign import _init
  :: ∀ eff. EffFn2 (AlexaEffects eff) Event Context Alexa
init :: ∀ eff. Event -> Context -> Eff (AlexaEffects eff) Alexa
init = runEffFn2 _init

-- | Register an intent handler.
foreign import _registerHandler
  :: ∀ eff.
     EffFn3 (AlexaEffects eff)
       Alexa
       String
       (EffFn1 (AlexaEffects eff) This Unit)
       Unit
registerHandler
  :: ∀ eff
   . Alexa
  -> IntentLabel
  -> (This -> Eff (AlexaEffects eff) Unit)
  -> (Eff (AlexaEffects eff) Unit)
registerHandler alexa (IntentLabel label) fn =
  runEffFn3 _registerHandler alexa label (mkEffFn1 fn)

foreign import _speak
  :: ∀ eff.
    EffFn2 (AlexaEffects eff)
      String
      This
      Unit
speak :: ∀ eff. Say -> This -> Eff (AlexaEffects eff) Unit
speak = runEffFn2 _speak <<< unwrap

foreign import _listen
  :: ∀ eff.
     EffFn2 (AlexaEffects eff)
       String
       This
       Unit
listen :: ∀ eff. Listen -> This -> Eff (AlexaEffects eff) Unit
listen = runEffFn2 _listen <<< unwrap

-- | Execute the given Alexa handler.
foreign import _execute :: ∀ eff. EffFn1 (AlexaEffects eff) Alexa Unit
execute :: ∀ eff. Alexa -> (Eff (AlexaEffects eff) Unit)
execute = runEffFn1 _execute

registerHelpIntent :: ∀ eff. Alexa -> Eff (AlexaEffects eff) Unit
registerHelpIntent alexa =
  let label = IntentLabel "AMAZON.HelpIntent"
      fn = \this -> do
        speak (Say "Please refer to the readme for example invocations and try again.") this
        listen (Listen "Please try again.") this
  in do
    log "Registered help intent"
    registerHandler alexa label fn

registerCancelIntent :: ∀ eff. Alexa -> Eff (AlexaEffects eff) Unit
registerCancelIntent alexa =
  let label = IntentLabel "AMAZON.CancelIntent"
      fn    = speak $ Say "Goodbye."
  in do
    log "Registered cancel intent"
    registerHandler alexa label fn

registerStopIntent :: ∀ eff. Alexa -> Eff (AlexaEffects eff) Unit
registerStopIntent alexa =
  let label = IntentLabel "AMAZON.StopIntent"
      fn    = speak $ Say "Goodbye."
  in do
    log "Registered stop intent"
    registerHandler alexa label fn

registerSpeakIntent :: ∀ eff. Alexa -> Eff (AlexaEffects eff) Unit
registerSpeakIntent alexa =
  let label = IntentLabel "SpeakIntent"
      fn    = speak $ Say "Hello."
  in do
    log "Registered speak intent"
    registerHandler alexa label fn

handler :: ∀ eff. Event -> Context -> Eff (AlexaEffects eff) Unit
handler event ctx = do
  alexa <- init event ctx

  registerHelpIntent alexa
  registerCancelIntent alexa
  registerStopIntent alexa
  registerSpeakIntent alexa

  execute alexa
