module Data.Serialization.BCD.Debug
    (debug
    ) where

import           System.IO        (hPutStrLn, stderr)
import           System.IO.Unsafe (unsafePerformIO)

debug :: Show a => String -> a -> a
debug msg x = unsafePerformIO $ hPutStrLn stderr (msg ++ show x) >> return x
