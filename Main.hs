{-# LANGUAGE PackageImports #-}

module Main where

import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import System.Posix.IO
import Control.Monad
import Data.Char
import Data.Word
import Data.List
import Data.Bits
import qualified Data.ByteString as BS
import qualified Options as O
import qualified "unix-bytestring" System.Posix.IO.ByteString as BSIO
import Debug.Trace


printIndentedListWithHeading :: String -> [ String ] -> IO ()
printIndentedListWithHeading _ [] = return ()
printIndentedListWithHeading h xs =
  do
    putStrLn ""
    putStrLn h
    -- Map over the strings, printing them out.
    mapM_ putStrLn $ zipWith (++) (map (\s -> "  " ++ s ++ " ") $ map show [1..] ) xs
    putStrLn ""

newValue :: Word8 -> O.Options -> Word8
newValue o ( O.Options _ b False ) = clearBit o ( fromIntegral b )
newValue o ( O.Options _ b True ) = setBit o ( fromIntegral b )

readPort :: Word32 -> IO Word8
readPort addr =
  do
    handle <- openFile "/dev/port" ReadWriteMode
    fd <- handleToFd handle
    BSIO.fdSeek fd AbsoluteSeek ( fromIntegral addr )
    v <- BSIO.fdRead fd 1
    closeFd fd
    return $ BS.head v

writePort :: Word32 -> Word8 -> IO ()
writePort addr val =
  do
    handle <- openFile "/dev/port" ReadWriteMode
    fd <- handleToFd handle
    BSIO.fdSeek fd AbsoluteSeek ( fromIntegral addr )
    c <- BSIO.fdWrite fd ( BS.singleton val )
    closeFd fd

main = 
  do
    args <- getArgs
 
    -- Parse options, getting a list of option actions
    let (actions, nonOptions, errors) = getOpt Permute O.optionDescriptions args
 
    -- Here we thread startOptions through all supplied option actions
    opts <- foldl (>>=) (return O.defaultOptions) actions

    -- Print out the errors.
    printIndentedListWithHeading "Errors" errors

    v <- readPort ( O.address opts )
    putStrLn $ "Initial value: " ++ ( show v )
    let nv = newValue v opts
    putStrLn $ "New value: " ++ ( show nv )
    writePort ( O.address opts ) nv

