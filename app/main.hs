{-# LANGUAGE BangPatterns#-}

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

main :: IO ()
main =
  let nums1 = [1 .. 10000000 :: Int]
      nums2 = [1 .. 10000000 :: Int]
   in print $ fromIntegral (sum nums1) / fromIntegral (length nums2)

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
