module M1 where

-- | (total, failed)
result :: IO (Int, Int)
result = pure (1, 0)

resultFailing :: IO (Int, Int)
resultFailing = pure (2, 1)

resultUnit :: IO ()
resultUnit = pure ()

resultThrows :: IO ()
resultThrows = ioError (userError "boom")
