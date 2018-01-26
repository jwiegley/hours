{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hours.Input where

import Control.Exception
import Data.Aeson
import Data.Text (pack, unpack)
import Data.Time
import Data.Yaml
import Hours.Budget (Interval(..))

parseIso :: Monad m => String -> m ZonedTime
parseIso = parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S %z"

formatIso :: ZonedTime -> String
formatIso = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S %z"

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
    { start        :: ZonedTime
    , finish       :: ZonedTime
    , nowThere     :: ZonedTime
    , loggedIn     :: Bool
    , intervals    :: [Interval ZonedTime (WorkDay, NominalDiffTime)]
    }

instance FromJSON a => FromJSON (Interval ZonedTime a) where
    parseJSON = withObject "Interval" $ \v -> Interval
        <$> (parseIso =<< v .: "begin")
        <*> (parseIso =<< v .: "end")
        <*> v .: "value"

instance ToJSON a => ToJSON (Interval ZonedTime a) where
   toJSON (Interval b e v) =
        object [ "begin" .= formatIso b
               , "end"   .= formatIso e
               , "value" .= v ]

instance FromJSON IntervalFile where
    parseJSON = withObject "IntervalFile" $ \v -> IntervalFile
        <$> (parseIso =<< v .: "start")
        <*> (parseIso =<< v .: "finish")
        <*> (parseIso =<< v .: "nowThere")
        <*> v .: "loggedIn"
        <*> v .: "intervals"

instance ToJSON IntervalFile where
   toJSON (IntervalFile b e t l v) =
        object [ "start"        .= formatIso b
               , "finish"       .= formatIso e
               , "nowThere"     .= formatIso t
               , "loggedIn"     .= l
               , "intervals"    .= v ]

readFile :: FilePath -> IO IntervalFile
readFile path = do
    eres <- decodeFileEither path
    case eres of
        Left err  -> throw err
        Right res -> return res
