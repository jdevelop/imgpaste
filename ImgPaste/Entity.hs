module ImgPaste.Entity where

import Data.ByteString

type LocalCtx = ( ByteString, ByteString, ByteString, [ByteString] )
type UploadResult = Either UploadError ByteString
data UploadError = UploadError {
    message :: String,
    response :: ByteString 
} deriving (Show)
