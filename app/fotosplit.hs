-- ----------------------------------------------------------------------------
-- fotosplit.hs - split *.jpg pictures by capture date
-- ----------------------------------------------------------------------------
--
-- Goal: sort the fotos from the old olympic camera
--
-- Processing steps
-- Get input directory name from run parameter
-- Check existance of input directory
-- Read all jpg file in a directory
-- Per jpeg file:
--     read capture date
--     convert cature date to standardized date string
--     check where subdirectory with date string exists
--           if not, create subdirectory
--           copy *.jpg file to subdirectory
--
-- Test in GHCi: :main "/home/roland/fotosplit/"
--               readexif "/home/roland/fotosplit/DSC04945.JPG"
--
-- Open Problems:
-- exif fails when the file does not exist
-- ----------------------------------------------------------------------------
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.Posix.Files ( setFileTimes )

import Control.Monad
import Data.List (isSuffixOf)
import Data.Char (toUpper)
import Data.Maybe (fromMaybe, fromJust, isJust)
import Data.Time
import Data.Convertible (convert)

import System.Directory (doesDirectoryExist, getDirectoryContents, createDirectoryIfMissing, renamePath)
import System.FilePath (combine, pathSeparator)
import System.Posix.Types (EpochTime(..))
import Graphics.Hexif
import Graphics.Hexif.Api

main :: IO()
main = do
   -- get input arguments
   args <- getArgs
   if length args /= 1
      then do
         putStrLn "usage:\r fotosplit <filePath>\r <filePath> = directory with *.jpg files"
         exitFailure
      else do
         let dirPath = head args
         exists <- doesDirectoryExist dirPath
         if exists
            then do
               fotosplit dirPath
               putStrLn "Processing ended"
            else
               putStrLn $ "Dirctory " ++ dirPath ++ " notFound"

fotosplit :: FilePath -> IO()
fotosplit path = do
      files <- getDirectoryContents path
      mapM_ pf $ filter filterjpg files
         where
           pf = processFile path
           filterjpg = isSuffixOf ".JPG" . map toUpper

processFile :: String -> String -> IO()
processFile dir file = do
      let actualFile = combine dir file
      mbDate <- readExifDate actualFile
      let mbStrDay = show . utctDay <$> mbDate
      let strDate = fromMaybe "NoDate" mbStrDay
      -- print strDate
      let newDir = dir <> [pathSeparator] <> strDate <> [pathSeparator]
      createDirectoryIfMissing False newDir
      let newFile = combine newDir file
      renamePath actualFile newFile
      when (isJust mbDate) $ setNewFileDate (fromJust mbDate) newFile

-- Read the original date, when the picture was created
readExifDate :: FilePath -> IO (Maybe UTCTime)
readExifDate file = do
   exif <- fromFile file
   return $ getDateTime exif

-- Set the file creation date to the exif capture date
setNewFileDate :: UTCTime -> String -> IO()
setNewFileDate utcDateTime newfile = do
         let epochTime = utcTimeToEpochTime utcDateTime
         setFileTimes newfile epochTime epochTime

-- convert `UTCTime` to `EpochTime`
utcTimeToEpochTime :: UTCTime -> EpochTime
utcTimeToEpochTime = convert



