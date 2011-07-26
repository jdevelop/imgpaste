module ImgPaste.Collect where

import ImgPaste.Entity
import ImgPaste.Batch
import Data.List as DL
import Data.ByteString.Char8 as C8

collectImageTags :: String -> IO String
collectImageTags path = filterAndConcat `fmap` 
                            uploadFilesBatch path wrapWithTag
    where
        wrapWithTag ( Right img ) = "<img src=\"http://" ++ C8.unpack img ++ "\"/>"
        wrapWithTag ( Left (UploadError a b)) = "Can not parse content " ++ (show b)
        filterAndConcat = DL.concatMap ( ++ "\n\n" ) . DL.filter ( /= "" )
