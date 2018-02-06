{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hours.Input where

import           Data.Aeson
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import           Data.Semigroup (Semigroup(sconcat), Max(..), Min(..))
import           Data.Text (pack, unpack)
import           Data.Time.Clock (UTCTime, NominalDiffTime)
import           Hours.Budget (Interval(..))
import           Hours.Time

data WorkDay
    = NotWorking
    | Holiday
    | Weekend
    | OffFriday
    | HalfFriday
    | RegularDay
    deriving (Eq, Show, Read)

instance FromJSON WorkDay where
    parseJSON = withText "Interval" (pure . read . unpack)

instance ToJSON WorkDay where
   toJSON x = String (pack (show x))

data IntervalFile = IntervalFile
    { start        :: UTCTime
    , finish       :: UTCTime
    , nowThere     :: UTCTime
    , loggedIn     :: Bool
    , intervals    :: [Interval UTCTime (WorkDay, NominalDiffTime)]
    }

instance FromJSON a => FromJSON (Interval UTCTime a) where
    parseJSON = withObject "Interval" $ \v -> Interval
        <$> (parseIso =<< v .: "begin")
        <*> (parseIso =<< v .: "end")
        <*> v .: "value"

instance ToJSON a => ToJSON (Interval UTCTime a) where
   toJSON (Interval b e v) =
        object [ "begin" .= formatIso b
               , "end"   .= formatIso e
               , "value" .= v ]

instance FromJSON IntervalFile where
    parseJSON = withObject "IntervalFile" $ \v -> IntervalFile
        <$> (parseIso =<< v .: "start")
        <*> (parseIso =<< v .: "finish")
        <*> (parseIso =<< v .: "now-there")
        <*> v .: "logged-in"
        <*> v .: "intervals"

instance ToJSON IntervalFile where
   toJSON (IntervalFile b e t l v) =
        object [ "start"        .= formatIso b
               , "finish"       .= formatIso e
               , "now-there"    .= formatIso t
               , "logged-in"    .= l
               , "intervals"    .= v ]

defaultFile :: UTCTime -> IntervalFile
defaultFile now = IntervalFile now now now False []

encodeIntervals :: UTCTime
                -> Bool
                -> [Interval UTCTime (WorkDay, NominalDiffTime)]
                -> ByteString
encodeIntervals _ _ [] = encode (Nothing :: Maybe IntervalFile)
encodeIntervals moment b xxs@(x:xs) = encode . Just $
    IntervalFile (getMin (sconcat (NE.map (Min . begin) (x :| xs))))
                 (getMax (sconcat (NE.map (Max . end)   (x :| xs))))
                 moment b xxs

decodeFile :: FilePath -> IO (Maybe IntervalFile)
decodeFile p = either error return . eitherDecode =<< case p of
    "-"  -> BL.getContents
    path -> BL.readFile path
