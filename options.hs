module Options
( Options( .. )
, defaultOptions
, optionDescriptions
)
where

import System.Console.GetOpt
import System.Exit
import Control.Monad
import System.IO
import Data.Char
import Data.Word
import Data.List
import Safe
import Debug.Trace

version :: String
version = "0.1.0"

data Options = Options { address :: Word32
                       , bit     :: Word8
                       , value   :: Bool
                       }

-- The defaults for the parameters
defaultOptions :: Options
defaultOptions = Options { address = 0xEE0
                         , bit     = 2
                         , value   = False
                         }
optionDescriptions :: [ OptDescr ( Options ->IO Options ) ]
optionDescriptions =
  [ Option "a" [ "address" ]
      ( ReqArg readAddress "" )
      "The address to read or write to"

  , Option "b" [ "bit" ]
      ( ReqArg readBit "" )
      "The bit to write"

  , Option "v" [ "value" ]
      ( ReqArg readValue "" )
      "The value to write"

  , Option "V" [ "version" ]
      ( NoArg ( \_ -> ( putStrLn $ "This is portio v" ++ version ) >>  exitWith ExitSuccess ) )
      "Print version information"

  , Option "h" [ "help" ]
      ( NoArg ( \_ -> ( putStrLn $ usageInfo "portio -a <address> -b <bit number> -v <on|off>" optionDescriptions ) >>  exitWith ExitSuccess ) )
      "Print this usage information"
  ]

readAddress :: String -> Options -> IO Options
readAddress arg opts =
  case readMay arg of
    Just addr -> trace ( "IO Port address: " ++ ( show addr ) ) return $ opts { address = addr } 
    Nothing   -> do 
                   putStrLn $ "Invalid address arg: " ++ arg
                   exitWith $ ExitFailure (-1)

readBit :: String -> Options -> IO Options
readBit arg opts =
  case readMay arg of
    Just b -> return $ opts { bit = b } 
    Nothing   -> do 
                   putStrLn $ "Invalid address arg: " ++ arg
                   exitWith $ ExitFailure (-1)


readValue :: String -> Options -> IO Options
readValue arg opts
  | arg == "on" = return $ opts { value = True }
  | arg == "high" = return $ opts { value = True }
  | arg == "yes" = return $ opts { value = True }
  | arg == "true" = return $ opts { value = True }
  | otherwise = return $ opts { value = False }
    
