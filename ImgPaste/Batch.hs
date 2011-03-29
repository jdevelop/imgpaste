module ImgPaste.Batch where

import ImgPaste.Upload
import System.Directory
import Data.ByteString.Char8 as C8 hiding (putStrLn)
import Control.Monad
import Data.List as DL
import Data.Either
import System.IO as IO

type BatchUploadResult = Either String [ByteString]

type ResultsCollector a = UploadResult -> a

uploadFilesBatch :: String -> ResultsCollector a -> IO [a]
uploadFilesBatch path f =
        ( listFiles `fmap` getDirectoryContents path ) >>= 
                mapM uploadImageWithStatus
        where
            uploadImageWithStatus img = do
                IO.hPutStrLn stderr $ "Processing image " ++ img
                f `fmap` pasteImage img
            basePath = path ++ "/"
            listFiles = DL.map ( basePath ++ ) . DL.filter (\x -> x /= "." && x /= ".." )
