module Shared.ScriptRefDatum where

import Contract.Prelude

import Contract.PlutusData (class HasPlutusSchema, type (:+), type (:=), type (@@), I, PNil, Z, genericToData)
import Ctl.Internal.FromData (class FromData, genericFromData)
import Ctl.Internal.ToData (class ToData)
import Data.Newtype (class Newtype)
import Prelude (class Eq, class Ord)

newtype PScriptRefDatum = PScriptRefDatum
  { scriptName :: String
  }

derive newtype instance Show PScriptRefDatum
derive instance Generic PScriptRefDatum _
derive instance Newtype PScriptRefDatum _

instance
  HasPlutusSchema
    PScriptRefDatum
    ( "PScriptRefDatum"
        :=
          ( "scriptName" := I String
              :+ PNil
          )
        @@ Z
        :+ PNil
    )

derive newtype instance Eq PScriptRefDatum
derive newtype instance Ord PScriptRefDatum

instance ToData PScriptRefDatum where
  toData = genericToData

instance FromData PScriptRefDatum where
  fromData = genericFromData
