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

-- | Newtype wrapper for intent handler label.
newtype IntentLabel = IntentLabel String
derive instance ntIntentLabel :: Newtype IntentLabel _

-- | Newtype wrapper for a message to be said to the user.
newtype Say = Say String
derive instance ntSay :: Newtype Say _

-- | Newtype wrapper for a message to prompt the user with while listening for
-- | a response.
newtype Prompt = Prompt String
derive instance ntListen :: Newtype Prompt _

foreign import _init
  :: ∀ eff. EffFn2 (AlexaEffects eff) Event Context Alexa
-- | Initialize an intent handler from the `Event` and `Context` provided by an
-- | AWS Lambda handler.
init :: ∀ eff. Event -> Context -> Eff (AlexaEffects eff) Alexa
init = runEffFn2 _init

foreign import _registerHandler
  :: ∀ eff.
     EffFn3 (AlexaEffects eff)
       Alexa
       String
       (EffFn1 (AlexaEffects eff) This This)
       Alexa
-- | Register an Alexa intent handler.
registerHandler
  :: ∀ eff
   . IntentLabel
  -> (This -> Eff (AlexaEffects eff) This)
  -> Alexa
  -> (Eff (AlexaEffects eff) Alexa)
registerHandler (IntentLabel label) fn alexa =
  runEffFn3 _registerHandler alexa label (mkEffFn1 fn)

foreign import _speak
  :: ∀ eff.
    EffFn2 (AlexaEffects eff)
      String
      This
      This
-- | Register a message and instruct Alexa to end the intent.
speak :: ∀ eff. Say -> This -> Eff (AlexaEffects eff) This
speak = runEffFn2 _speak <<< unwrap

foreign import _listen
  :: ∀ eff.
     EffFn2 (AlexaEffects eff)
       String
       This
       This
-- | Register a prompt message and instruct Alexa to wait for a user response.
listen :: ∀ eff. Prompt -> This -> Eff (AlexaEffects eff) This
listen = runEffFn2 _listen <<< unwrap

foreign import _execute :: ∀ eff. EffFn1 (AlexaEffects eff) Alexa Unit
-- | Call the execution method for an Alexa handler.
execute :: ∀ eff. Alexa -> (Eff (AlexaEffects eff) Unit)
execute = runEffFn1 _execute

--------------------------------------------------------------------------------
{-
Some test code to make sure the FFI all works, this is going to be moved out
to a test suite or example if this becomes a real library.
-}
--------------------------------------------------------------------------------

registerHelpIntent :: ∀ eff. Alexa -> Eff (AlexaEffects eff) Alexa
registerHelpIntent =
  let label  = IntentLabel "AMAZON.HelpIntent"
      say    = Say "Please consult some example invocations in the readme and try again."
      prompt = Prompt "Please try again."
      fn     = speak say >=> listen prompt
  in registerHandler label fn

registerCancelIntent :: ∀ eff. Alexa -> Eff (AlexaEffects eff) Alexa
registerCancelIntent =
  let label = IntentLabel "AMAZON.CancelIntent"
      fn    = speak $ Say "Goodbye."
  in registerHandler label fn

registerStopIntent :: ∀ eff. Alexa -> Eff (AlexaEffects eff) Alexa
registerStopIntent =
  let label = IntentLabel "AMAZON.StopIntent"
      fn    = speak $ Say "Goodbye."
  in registerHandler label fn

registerSpeakIntent :: ∀ eff. Alexa -> Eff (AlexaEffects eff) Alexa
registerSpeakIntent =
  let label = IntentLabel "SpeakIntent"
      fn    = speak $ Say "Hello."
  in registerHandler label fn

handler :: ∀ eff. Event -> Context -> Eff (AlexaEffects eff) Unit
handler event ctx = do
  alexa <- init event ctx

  _ <- registerHelpIntent
   >=> registerCancelIntent
   >=> registerStopIntent
   >=> registerSpeakIntent
   $ alexa

  execute alexa
