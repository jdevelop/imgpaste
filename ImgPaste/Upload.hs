module ImgPaste.Upload where

import Control.Monad
import Network.Curl
import Data.ByteString
import Text.Regex.PCRE
import Text.Regex.PCRE.ByteString
import System.Posix

type LocalCtx = ( ByteString, ByteString, ByteString, [ByteString] )
type UploadResult = Either UploadError ByteString
data UploadError = UploadError {
    message :: String,
    response :: ByteString 
} deriving (Show)

extractResponse :: CurlResponse_ [(String,String)] ByteString -> ByteString
extractResponse = respBody

uploadFile :: String -> IO ByteString
uploadFile fileName = initialize >>= withCurlDo . (`uploadFileWithCurl` fileName)

uploadFileWithCurl :: Curl -> String -> IO ByteString
uploadFileWithCurl curl fileName =
    liftM extractResponse $ do_curl_ curl "http://imgpaste.com/" 
        [
            CurlVerbose False, 
            CurlHttpHeaders [
                "Expect: "
                ],
            CurlHttpPost postData
        ]
    where
        postData = [
            HttpPost "uplfile" 
                (Just "image/jpeg")
                ( ContentFile fileName ) 
                []
                Nothing,
            makeFormPost "keep" "a"
            ]
        makeFormPost name value = HttpPost name Nothing
            (ContentString value)
            []
            Nothing

extractUrl :: ByteString -> UploadResult
extractUrl src = match (src =~ "<input type=\"text\" name=\"copyfield\" size=\"31\" value=\"([^\"]+?)\" />" :: LocalCtx)
    where 
        match (_,_,_,[x]) = Right x
        match _ = Left $ UploadError "Can not parse content." src

pasteImage :: Curl -> String -> IO UploadResult
pasteImage curl path = liftM extractUrl $ uploadFileWithCurl curl path

