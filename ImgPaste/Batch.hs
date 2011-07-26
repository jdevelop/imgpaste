module ImgPaste.Batch where

import ImgPaste.Entity
import ImgPaste.Upload
import System.Directory
import Data.ByteString.Char8 as C8 hiding (putStrLn)
import Control.Monad
import Data.List as DL
import Data.Either
import System.IO as IO
import Network.Curl

type BatchUploadResult = Either String [ByteString]

type ResultsCollector a = UploadResult -> a

uploadFilesBatch :: String -> ResultsCollector a -> IO [a]
uploadFilesBatch path f = do
        curl <- initialize
        withCurlDo $ 
            ( listFiles `fmap` getDirectoryContents path ) >>= 
                mapM ( uploadImageWithStatus curl )
        where
            uploadImageWithStatus curl img = do
                IO.hPutStrLn stderr $ "Processing image " ++ img
                f `fmap` ( pasteImage curl img )
            basePath = path ++ "/"
            listFiles = DL.map ( basePath ++ ) . DL.filter (\x -> x /= "." && x /= ".." )
