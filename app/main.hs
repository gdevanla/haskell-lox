{-# LANGUAGE BangPatterns#-}
{-# LANGUAGE FlexibleContexts #-}

import           Control.Monad.Primitive     (PrimMonad, PrimState)
import qualified Data.ByteString.Lazy        as L
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Unboxed         as U
-- import qualified Data.Vector.Mutable as M
-- import qualified Data.Vector.Unboxed.Mutable as MU
import           Data.Word                   (Word8)


main :: IO ()
main = do
    -- Get all of the contents from stdin
    lbs <- L.getContents

    -- Create a new 256-size mutable vector
    -- Fill the vector with zeros
    mutable <- M.replicate 5 0

    -- Add all of the bytes from stdin
    addBytes mutable lbs

    -- Freeze to get an immutable version
    vector <- U.unsafeFreeze mutable

    -- Print the frequency of each byte
    -- In newer vectors: we can use imapM_
    U.zipWithM_ printFreq (U.enumFromTo 0 255) vector

addBytes :: (PrimMonad m, M.MVector v Int)
         => v (PrimState m) Int
         -> L.ByteString
         -> m ()
addBytes v lbs = mapM_ (addByte v) (L.unpack lbs)

addByte :: (PrimMonad m, M.MVector v Int)
        => v (PrimState m) Int
        -> Word8
        -> m ()
addByte v w = do
    -- Read out the old count value
    oldCount <- M.read v index
    -- Write back the updated count value
    M.write v index (oldCount + 1)
  where
    -- Indices in vectors are always Ints. Our bytes come in as Word8, so we
    -- need to convert them.
    index :: Int
    index = fromIntegral w

printFreq :: Int -> Int -> IO ()
printFreq index count = putStrLn $ concat
    [ "Frequency of byte "
    , show index
    , ": "
    , show count
    ]












-- data RunningTotal = RunningTotal
--   { sum :: !Int
--   , count :: !Int
--   }

-- printAverage :: RunningTotal -> IO ()
-- printAverage (RunningTotal !sum !count)
--   | count == 0 = error "Need at least one value!"
--   | otherwise = print (fromIntegral sum / fromIntegral count :: Double)

-- -- | A fold would be nicer... we'll see that later
-- printListAverage :: [Int] -> IO ()
-- printListAverage =
--   go (RunningTotal 0 0)
--   where
--     go rt [] = printAverage rt
--     go (RunningTotal sum count) (x:xs) =
--       let
--         !sum' = sum + x
--         !count' = count + 1
--         rt = RunningTotal sum' count'
--        in go rt xs


-- data Foo = Foo Int
-- data Bar = Bar !Int
-- newtype Baz = Baz Int

-- main :: IO ()
-- main = do
--   putStrLn "inside main"
--   printListAverage [1..1000000]
--   Baz undefined `seq` putStrLn "Still alive!"

#!/usr/bin/env stack
-- stack --resolver lts-12.21 script

-- main :: IO ()
-- main =
--   let nums = [1..10000000 :: Int]
--    in print $ fromIntegral (sum nums) / fromIntegral (length nums)

-- main :: IO ()
-- main =
--   let nums1 = [1 .. 10000000 :: Int]
--       nums2 = [1 .. 10000000 :: Int]
--    in print $ fromIntegral (sum nums1) / fromIntegral (length nums2)

-- main :: IO ()
-- main = do
--   (options, ()) <- simpleOptions
--     $(simpleVersion Paths_haskell_lox.version)
--     "Header for command line arguments"
--     "Program description, also for command line arguments"
--     (Options
--        <$> switch ( long "verbose"
--                  <> short 'v'
--                  <> help "Verbose output?"
--                   )
--     )
--     empty
--   lo <- logOptionsHandle stderr (optionsVerbose options)
--   pc <- mkDefaultProcessContext
--   withLogFunc lo $ \lf ->
--     let app = App
--           { appLogFunc = lf
--           , appProcessContext = pc
--           , appOptions = options
--           }
--      in runRIO app run
