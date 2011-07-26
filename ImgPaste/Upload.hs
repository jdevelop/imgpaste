module ImgPaste.Upload where

import ImgPaste.Entity
import Control.Monad
import Network.Curl
import Data.ByteString.Char8
import Text.Regex.PCRE
import Text.Regex.PCRE.ByteString
import System.Posix

extractResponse :: CurlResponse_ [(String,String)] ByteString -> ByteString
extractResponse = respBody

uploadFileWithCurl :: Curl -> String -> ByteString -> IO ByteString
uploadFileWithCurl curl fileName key =
    liftM extractResponse $ do_curl_ curl "http://imgpaste.com/upload/" 
        [
            CurlVerbose False, 
            CurlHttpHeaders [
                "Expect: "
                ],
            CurlHttpPost postData,
            CurlCookie ("csrftoken="++keyStr)
        ]
    where
        postData = [
            HttpPost "file" 
                (Just "image/jpeg")
                ( ContentFile fileName ) 
                []
                Nothing,
            makeFormPost "submit" "Send",
            makeFormPost "csrfmiddlewaretoken" keyStr
            ]
        makeFormPost name value = HttpPost name Nothing
            (ContentString value)
            []
            Nothing
        keyStr = unpack key

getFormKey :: Curl -> IO ByteString
getFormKey curl = (fKey . extractResponse ) `fmap` do_curl_ curl "http://imgpaste.com/" [CurlVerbose False]
    where
        fKey src = match (src =~ "<input type='hidden' name='csrfmiddlewaretoken' value='([^\"]+?)' />" :: LocalCtx)
        match (_,_,_,[key]) = key
        match _ = error "Can not parse data"

extractUrl :: ByteString -> UploadResult
extractUrl src = match (src =~ "<input type=\"text\" name=\"field\" value=\"([^\"]+?)\" />" :: LocalCtx)
    where 
        match (_,_,_,[x]) = Right x
        match _ = error "Can not parse data"

pasteImage :: Curl -> String -> IO UploadResult
pasteImage curl path = liftM extractUrl $ getFormKey curl >>= uploadFileWithCurl curl path 

