module Storage.Storage
  ( getItem
  , setItem
  )
  where

import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, runEffectFn1, runEffectFn2)

foreign import getItemImpl :: EffectFn1 String String

getItem :: String -> Effect String
getItem = runEffectFn1 getItemImpl

foreign import setItemImpl :: EffectFn2 String String Unit

setItem :: String -> String -> Effect Unit
setItem = runEffectFn2 setItemImpl