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
import Control.Monad.Eff.Uncurried (EffFn1, EffFn2, EffFn3, mkEffFn1, runEffFn1, runEffFn2, runEffFn3)
import Data.Newtype (class Newtype, unwrap)

type AlexaEffects eff = (alexa :: ALEXA)

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
       (EffFn1 (AlexaEffects eff) This This)
       Alexa
registerHandler
  :: ∀ eff
   . Alexa
  -> IntentLabel
  -> (This -> Eff (AlexaEffects eff) This)
  -> (Eff (AlexaEffects eff) Alexa)
registerHandler alexa (IntentLabel label) fn =
  runEffFn3 _registerHandler alexa label (mkEffFn1 fn)

foreign import _speak
  :: ∀ eff.
    EffFn2 (AlexaEffects eff)
      String
      This
      This
speak :: ∀ eff. Say -> This -> Eff (AlexaEffects eff) This
speak = runEffFn2 _speak <<< unwrap

foreign import _listen
  :: ∀ eff.
     EffFn2 (AlexaEffects eff)
       String
       This
       This
listen :: ∀ eff. Listen -> This -> Eff (AlexaEffects eff) This
listen = runEffFn2 _listen <<< unwrap

-- | Execute the given Alexa handler.
foreign import _execute :: ∀ eff. EffFn1 (AlexaEffects eff) Alexa Unit
execute :: ∀ eff. Alexa -> (Eff (AlexaEffects eff) Unit)
execute = runEffFn1 _execute

registerHelpIntent :: ∀ eff. Alexa -> Eff (AlexaEffects eff) Alexa
registerHelpIntent alexa =
  let label   = IntentLabel "AMAZON.HelpIntent"
      fn =
        speak (Say "Please consult some example invocations in the readme and try again.")
        >=>
        listen (Listen "Please try again.")
  in registerHandler alexa label fn

registerCancelIntent :: ∀ eff. Alexa -> Eff (AlexaEffects eff) Alexa
registerCancelIntent alexa =
  let label = IntentLabel "AMAZON.CancelIntent"
      fn    = speak $ Say "Goodbye."
  in registerHandler alexa label fn

registerStopIntent :: ∀ eff. Alexa -> Eff (AlexaEffects eff) Alexa
registerStopIntent alexa =
  let label = IntentLabel "AMAZON.StopIntent"
      fn    = speak $ Say "Goodbye."
  in registerHandler alexa label fn

registerSpeakIntent :: ∀ eff. Alexa -> Eff (AlexaEffects eff) Alexa
registerSpeakIntent alexa =
  let label = IntentLabel "SpeakIntent"
      fn    = speak $ Say "Hello."
  in registerHandler alexa label fn

handler :: ∀ eff. Event -> Context -> Eff (AlexaEffects eff) Unit
handler event ctx = do
  alexa <- init event ctx

  _ <- registerHelpIntent
   >=> registerCancelIntent
   >=> registerStopIntent
   >=> registerSpeakIntent
   $ alexa

  execute alexa
