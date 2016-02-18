{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module GeoShit where

import            System.IO
import            Data.Aeson
import            Data.Aeson.Types
import            Network.HTTP.Client
import            Network.HTTP.Client.TLS
import            Text.Regex.Posix
import Data.List.Split

import            GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as B


data GeoshitReq = GeoshitReq { wifiAPs :: [AccessPoint] } deriving (Show, Generic)
data AccessPoint = AccessPoint { address :: String } deriving (Show, Generic)
data GeoshitResp = GeoshitResp { location :: Location
                               , accuracy :: Float
                               } deriving (Show, Generic)
data Location = Location { lat :: Float , lng :: Float} deriving (Show, Generic)


-- gonna just let the compiler try to get these
instance FromJSON Location
instance FromJSON GeoshitReq
instance FromJSON GeoshitResp
instance FromJSON AccessPoint
instance ToJSON Location
instance ToJSON GeoshitReq
instance ToJSON GeoshitResp
instance ToJSON AccessPoint


testReq :: IO (Maybe Location)
testReq = do
  manager <- newManager tlsManagerSettings

  -- Create the request
  locationKey <- readFile "/home/frank/bin_storage/GoogleLocationKey.txt"
  initialRequest <- parseUrl $ "https://www.googleapis.com/geolocation/v1/geolocate?key=" ++ (newLength locationKey) -- because readFile adds \n to end of string
  let request = initialRequest { method = "POST", requestBody = RequestBodyLBS $ encode req }
  response <- httpLbs request manager
--  putStrLn $ "The status code was: " ++ (show $ statusCode $ responseStatus response)

  let b = responseBody response
  -- print $ responseBody response
  let gsp = decode b :: Maybe GeoshitResp
  case gsp of
   Nothing -> return Nothing
   Just r -> return $ Just $ location r
  where body = B.unpack $ encode $ req
        req = GeoshitReq [AccessPoint addr]
        addr = "" -- "00:00:0c:07:ac:2c" -- why does this work??

lookupLatLong :: (Maybe Location) -> IO (String, String)
lookupLatLong l = case l of
  Nothing -> print "what" >> return ("","")
  Just loc -> do
    let lt = lat loc
        lg = lng loc
    manager <- newManager tlsManagerSettings
    latlongToCityKey <- readFile "/home/frank/bin_storage/GoogleLatLongToCityKey.txt"
    request <- parseUrl $
               "https://maps.googleapis.com/maps/api/geocode/json?latlng=" ++ show lt ++ "," ++ show lg ++ "&location_type=approximate&result_type=locality&key=" ++ (newLength latlongToCityKey)
    response <- httpLbs request manager
    let res = B.unpack $ responseBody response
    let res' = getAllTextMatches $ res =~ ("\"formatted_address\" : .*$" :: String) :: [String]
        res'' = head $ tail $ splitOn ":" $ head res'
        res''' = reverse $ dropWhile (\c -> c == '"' || c == ' ' || c == ',') $ reverse $ dropWhile (\c -> c == '"' || c == ' ' || c == ',') $ res''
        res'''' = (\(x:y:[]) -> (x,y)) $ take 2 $ splitOn ", " res'''

    -- print res''''
    return $ res''''

newLength :: [Char] -> [Char] -- necessary because readFile adds \n to end of string
newLength x = take ((length x) - 1) x
