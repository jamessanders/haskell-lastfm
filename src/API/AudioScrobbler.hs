{-# LANGUAGE NamedFieldPuns #-}
module API.AudioScrobbler where

import Text.XML.Expat.Tree
import Network.HTTP
import Text.Printf
import Data.List
import Control.Applicative
import Network.URI
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Internal (c2w)
import Debug.Trace

class PrettyPrint a where
    pretty :: a -> String

data Format = XMLF | JSONF
instance Show Format where
    show XMLF  = "xml"
    show JSONF = "json"

data ASAPI = ASAPI { getASKey :: String
                   , getASUri :: String
                   , getASFormat :: Format }
           deriving (Show)

makeASAPI k = ASAPI k "ws.audioscrobbler.com/2.0" XMLF

data TrackInfo  = TrackInfo { getTrackArtist :: String 
                            , getTrackName :: String
                            , getTrackPlaycount :: String }
                deriving (Show)

emptyTrackInfo = TrackInfo "" "" ""

instance PrettyPrint TrackInfo where
    pretty (TrackInfo{getTrackArtist,getTrackName}) = printf "%s - %s" getTrackArtist getTrackName

data ArtistInfo = Artist { getArtistName :: String
                         , getArtistBio :: String
                         , getArtistImages :: String
                         , getSimilarArtist :: [String] }
                deriving(Show) 
emptyArtist = Artist "" "" "" []

makeRequest asapi params = printf "http://%s/?api_key=%s&%s" 
                             (getASUri asapi)  (getASKey asapi) (format params)
    where format = intercalate "&" . map (\(a,b)-> escape a ++ "=" ++ escape b)


searchArtist asapi artist = do req <- httpGet $ makeRequest asapi [("method","artist.getinfo")
                                                                  ,("artist",artist)] 
                               return . extractArtist $ makeTree req
        where                                                                                                                           
        extractArtist xml = walkTree [("/lfm/artist/name",setName)                                                                
                                     ,("/lfm/artist/bio/summary", setBio)                                                         
                                     ,("/lfm/artist/similar/artist/name", setSimilar)                                             
                                     ,("/lfm/artist/image", setImage)] xml emptyArtist                                            
            where setName artist _ (x:xs) = artist { getArtistName = extractText x }                                              
                  setBio  artist _ (x:xs) = artist { getArtistBio  = extractText x }                                              
                  setBio  artist _ _      = artist                                                                                
                  setImage artist attrs (x:xs) = case lookup "size" attrs of                                                      
                                                   Just "large" -> artist { getArtistImages = extractText x }                     
                                                   _ -> artist                                                                    
                  setImage artist _ _ = artist                                                                                    
                  setSimilar (artist@Artist{ getSimilarArtist = sa }) _ (x:xs) = artist { getSimilarArtist = extractText x : sa } 

getArtistTopTracks asapi artist = do req <- httpGet $ makeRequest asapi [("method","artist.getTopTracks")
                                                                        ,("artist",artist)] 
                                     return . reverse . extractTracks $ makeTree req
    where extractTracks xml = walkTree [("/lfm/toptracks/track",getTrack)] xml []
          getTrack ls a c = walkTree [("/track/name",setName)
                                     ,("/track/playcount",setPlaycount)] (Element "track" a c) (emptyTrackInfo { getTrackArtist = artist }) : ls
          setName track attr (x:xs) = track { getTrackName = extractText x }
          setPlaycount track attr (x:xs) = track { getTrackPlaycount = extractText x } 

extractText (Text t) = t


walkTree a b c = walkTree a b "" c
    where
        walkTree _ (Text _) _ init = init                                               
        walkTree actions this@(Element name attr children) path init =                  
            let new_path = path ++ "/" ++ name                                          
                new_state = case lookup new_path actions of                             
                              Just act -> (act init attr children)                      
                              Nothing -> init                                           
            in foldl (\a b-> walkTree actions b new_path a) new_state children          



makeTree :: String -> Node String String
makeTree src = let (tree, mError) = parse defaultParseOptions (L.pack $ map c2w $ src)
               in tree

httpGet url =  do
  rsp <- simpleHTTP (getRequest url)
  -- fetch document and return it (as a 'String'.)
  getResponseBody rsp

escape = escapeURIString isAllowedInURI