module ImgPaste.Entity where

type UploadResult = Either UploadError String
data UploadError = UploadError {
    message :: String,
    response :: String 
} deriving (Show)
