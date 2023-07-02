module Info.UserData where

import Contract.Prelude

import Contract.Address (PaymentPubKeyHash, Bech32String)
import Contract.Chain (currentTime)
import Contract.Monad (Contract, liftContractM)
import Contract.Time (POSIXTime)
import Contract.Value as Value
import Ctl.Internal.Types.ByteArray (ByteArray(..))
import Ext.Data.Either (eitherContract)
import Data.Array as Array
import Data.BigInt (BigInt)
import Data.TextDecoder (decodeUtf8)
import Fundraising.Datum (PFundraisingDatum(..))
import Fundraising.FundraisingScript (getFundraisingTokenName)
import Protocol.UserData (ProtocolConfigParams)
import Shared.Utxo (UtxoTuple, extractDatumFromUTxO, extractValueFromUTxO)
import Ext.Contract.Value (getCurrencyByTokenName, currencySymbolToString)
import Shared.MinAda (minAdaValue)

newtype FundraisingInfo = FundraisingInfo
  { creator :: PaymentPubKeyHash
  , description :: String
  , goal :: BigInt -- Goal in lovelaces
  , raisedAmt :: BigInt -- Raised amount in lovelaces
  , deadline :: POSIXTime
  , threadTokenCurrency :: Value.CurrencySymbol
  , threadTokenName :: Value.TokenName
  , path :: String
  , isCompleted :: Boolean
  }

derive newtype instance Show FundraisingInfo
derive newtype instance Eq FundraisingInfo

mapToFundraisingInfo :: UtxoTuple -> Contract FundraisingInfo
mapToFundraisingInfo utxo = do
  PFundraisingDatum currentDatum <- liftContractM "Impossible to extract datum from UTxO" $ extractDatumFromUTxO utxo
  let frVal = extractValueFromUTxO utxo
  let currentFunds = Value.valueToCoin' frVal - Value.valueToCoin' minAdaValue - Value.valueToCoin' minAdaValue
  let ByteArray unwrappedDesc = currentDatum.frTitle
  desc <- eitherContract "Description decoding failed: " $ decodeUtf8 unwrappedDesc
  frTokenName <- getFundraisingTokenName
  cs <- liftContractM "Impossible to get currency by token name" $ getCurrencyByTokenName frVal frTokenName
  let pathStr = currencySymbolToString cs
  now <- currentTime
  pure $ FundraisingInfo
    { creator: currentDatum.creatorPkh
    , description: desc
    , goal: currentDatum.frAmount
    , raisedAmt: currentFunds
    , deadline: currentDatum.frDeadline
    , threadTokenCurrency: cs
    , threadTokenName: frTokenName
    , path: pathStr
    , isCompleted: now > currentDatum.frDeadline || currentFunds >= currentDatum.frAmount
    }

filterByPkh :: PaymentPubKeyHash -> Array FundraisingInfo -> Array FundraisingInfo
filterByPkh pkh = Array.filter belongsToUser
  where
  belongsToUser (FundraisingInfo frInfo) = frInfo.creator == pkh

newtype UserInfo = UserInfo
  { address :: Bech32String
  , isManager :: Boolean
  }

derive newtype instance Show UserInfo
derive newtype instance Eq UserInfo

newtype AppInfo = AppInfo
  { protocolConfig :: ProtocolConfigParams
  , userInfo :: UserInfo
  }

derive newtype instance Show AppInfo
derive newtype instance Eq AppInfo
