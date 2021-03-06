{-# LANGUAGE QuasiQuotes #-}

module Http.Trace
       ( traceRedirects
       , traceRedirectsTLD  
       , isReachable
       ) where

import           Control.Monad.IO.Class
import qualified System.IO as IO
import           System.IO.Unsafe
import           Control.Applicative
import qualified Data.Text as T
import           Data.Maybe
import           Data.String.Utils
import           Network.Curl
import           Text.Regex.PCRE.Heavy
import           Safe

--------------------------------------------------------------------------------

-- | Regex for getting web links from text
-- 
fullUrlRegex :: Regex
fullUrlRegex = [re|(http|ftp|https)://([\w_-]+(?:(?:\.[\w_-]+)+))([\w.,@?^=%&:/~+#-]*[\w@?^=%&/~+#-])?|]

-- | Regex for getting top-level web link from text
-- 
shortUrlRegex :: Regex
shortUrlRegex = [re|(http|https)?:\/\/(www\.)?[-a-zA-Z0-9@:%._\+~#=]{2,256}\.[a-z]{2,6}|]

userAgent = ""

-- | Function to get all redirects for
--   provided url in form of full link
--   Search throught LOCATION meta, head
--   and body tags
traceRedirects :: String      -- ^ Starting url
               -> IO [String] -- ^ List of available redirects
traceRedirects start =
  traceAux start
     where
       traceAux :: String -> IO [String]
       traceAux ""   = return $ []
       traceAux curr = do
         newLink  <- getNewLink (strip curr)
         newTrace <- traceAux (strip newLink)
         return $ curr : newTrace  
           where
             getNewLink :: String -> IO String
             getNewLink curr = do
               hds <- curlHead curr []
               let headMetas = snd hds
                   newLinkM  = lookup "LOCATION" headMetas
               case newLinkM of
                 Just link -> return $ link
                 Nothing   ->
                   do 
                     str <- curlGetString curr [CurlTimeout 60]
                     --putStrLn $ curr
                     --putStrLn $ "--->" ++ (show $ fst str) ++ snd str
                     let  urlFromBodyM = headMay $ scan fullUrlRegex $ snd str      
                     case urlFromBodyM of
                       Nothing    -> return $ ""
                       Just link  -> return $ fst link

-- | Alternative to `traceRedrects` that returns top-level domain names
--   without full url path. With compress functionality can show only
--   distinct domains without repetition.                       
traceRedirectsTLD :: String       -- ^ Starting url
                  -> Bool         -- ^ Flag to remove duplicate domains
                  -> IO [String]  -- ^ List of available redirects
traceRedirectsTLD start compress = do
  rdrs <- traceRedirects start
  let rdrs' = map (\x -> fst $ head $ scan shortUrlRegex x) rdrs
  -- TODO: 1. apply shortUrlRegex
  --       2. remove same name
      rdrs'' = case compress of
                 True  -> removeDuplicates rdrs' 
                 False -> rdrs' 
  return $ rdrs''

-- traceRedirectsWithType -- usually click/jump/track/badlink

-- | Checks if we redirects paths lead to destination url
--                        
isReachable :: String  -- ^ Statring url
            -> String  -- ^ Destination url
            -> IO Bool -- ^ Flag whether url reached
isReachable start dest =
  do
    redirects <- traceRedirects start
    case null redirects of
      True  -> return $ False
      False -> return $ last redirects == dest

--------------------------------------------------------------------------------
      
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = rdHelper []
    where rdHelper seen [] = seen
          rdHelper seen (x:xs)
              | x `elem` seen = rdHelper seen xs
              | otherwise = rdHelper (seen ++ [x]) xs      
