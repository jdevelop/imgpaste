module ImgPaste.Upload where

import Control.Monad
import Network.Curl
import Data.ByteString
import Text.Regex.PCRE
import Text.Regex.PCRE.ByteString

type LocalCtx = ( ByteString, ByteString, ByteString, [ByteString] )
type UploadResult = Either UploadError ByteString
data UploadError = UploadError {
    message :: String,
    response :: ByteString 
} deriving (Show)

extractResponse :: CurlResponse_ [(String,String)] ByteString -> ByteString
extractResponse = respBody

uploadFile :: String -> IO (ByteString)
uploadFile fileName = initialize >>= withCurlDo . (flip uploadFileWithCurl fileName)

uploadFileWithCurl :: Curl -> String -> IO ( ByteString )
uploadFileWithCurl curl fileName = do
    liftM extractResponse $ do_curl_ curl "http://imgpaste.com/" 
        [CurlPost True, CurlHttpPost postData, CurlVerbose True ]
    where
        postData = [
--            HttpPost "upfile" 
--                Nothing
--                ( ContentFile fileName ) 
--                []
--                Nothing
            makeFormPost "submit" "Upload",
            makeFormPost "keep" "a"
            ]
        makeFormPost name value = HttpPost name Nothing 
            (ContentString value)
            []
            Nothing

-- <input type="text" name="copyfield" size="31" value="http://imgpaste.com/tmp/yvtul.png" />

extractUrl :: ByteString -> UploadResult
extractUrl src = match (src =~ "<input type=\"text\" name=\"copyfield\" size=\"31\" value=\"([^\"]+?)\" />" :: LocalCtx)
    where 
        match (_,_,_,[x]) = Right x
        match _ = Left $ UploadError "Can not parse content." src

pasteImage :: String -> IO ( UploadResult )
pasteImage = liftM extractUrl . uploadFile

