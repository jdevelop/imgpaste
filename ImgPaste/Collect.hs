module ImgPaste.Collect where

import ImgPaste.Batch
import Data.List as DL
import Data.ByteString.Char8 as C8

collectImageTags :: String -> IO String
collectImageTags path = filterAndConcat `fmap` 
                            uploadFilesBatch path wrapWithTag
    where
        wrapWithTag ( Right img ) = "<img src=\"" ++ C8.unpack img ++ "\"/>"
        wrapWithTag _ = ""
        filterAndConcat = DL.concatMap ( ++ "\n\n" ) . DL.filter ( /= "" )
