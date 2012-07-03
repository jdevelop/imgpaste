module ImgPaste.Upload where

import ImgPaste.Entity
import Control.Monad
import Control.Applicative ((<$>))
import Network.Curl
import Data.ByteString.Char8
import System.Posix
import Text.Regex.PCRE
import qualified Text.XML.HXT.XPath as XP
import qualified Text.XML.HXT.Core as X
import qualified Data.List as DL
import Debug.Trace

extractResponse :: CurlResponse_ [(String,String)] String -> String
extractResponse = respBody

opts = [ CurlCookieJar "cookies" {- , CurlVerbose True -} ]

uploadFileWithCurl :: Curl -> String -> String -> IO String
uploadFileWithCurl curl fileName key = do
    reset curl
    setopts curl opts
    liftM extractResponse $ do_curl_ curl "http://img.leprosorium.com/new.js" 
        [
            CurlVerbose False, 
            CurlHttpHeaders [
              "User-Agent: Mozilla/5.0 (X11; FreeBSD amd64; rv:12.0) Gecko/20100101 Firefox/12.0",
              "Referer: http://img.leprosorium.com/",
              "Accept-Language: en-us,en;q=0.5",
              "Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"
                ],
            CurlHttpPost postData,
            CurlFreshConnect True
        ]
    where
        postData = [
            HttpPost "image[uploaded_data]" 
                (Just "image/png")
                ( ContentFile fileName ) 
                []
                Nothing,
            makeFormPost "commit" "Yarr!",
            makeFormPost "upload_type" "file",
            makeFormPost "image[watermark]" "0",
            makeFormPost "image[valid_referers]" "",
            makeFormPost "image[password]" "",
            makeFormPost "authenticity_token" key
            ]
        makeFormPost name value = HttpPost name Nothing
            (ContentString value)
            []
            Nothing

extractAuthToken content = liftM DL.head . X.runX $
  X.readString [X.withValidate X.no, X.withParseHTML X.yes, X.withWarnings X.no] content X.>>>
  XP.getXPathTrees "//input[@name='authenticity_token']" X.>>>
  X.deep 
      ( X.isElem X.>>> X.hasName "input" X.>>> X.getAttrValue "value" )

getFormKey curl = do
    reset curl
    setopts curl opts
    (extractAuthToken . extractResponse ) =<< do_curl_ curl "http://img.leprosorium.com" 
      [ CurlVerbose False, 
        CurlFreshConnect True]

extractUrl :: String -> UploadResult
extractUrl src = match (src =~ "<a href=\\\\\\\\\\\\\"(http://img.leprosorium.com/\\d+?)\\\\\\\\\\\\\">" :: (String, String, String, [String]))
    where 
        match (_,_,_,[x]) = Right x
        match _ = error "Can not parse data"

pasteImage :: Curl -> String -> IO UploadResult
pasteImage curl path = extractUrl <$> (getFormKey curl >>= uploadFileWithCurl curl path)

