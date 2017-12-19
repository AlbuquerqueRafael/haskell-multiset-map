{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

module GenerateFile (main) where

import TestIntersection as In
import TestUnion as Un
import Test.HUnit
import Data.List.Split
import GHC.Generics
import System.IO as T
import Data.Text.Lazy (Text)
import Data.Text.Lazy.IO as I
import Data.Aeson.Text (encodeToLazyText)
import Data.Aeson (ToJSON)

data Output = Output { matricula :: String, falhas :: Int,
                    passaram :: Int, totalTestes :: Int, excecoes :: Int} deriving (Show, Generic, ToJSON)


main matricula = do
  testsResult <- runTestTT $ test $ mconcat [ Un.tests, In.tests]
  generateResult matricula testsResult

generateResult :: String -> Counts -> IO ()
generateResult matricula testsResult = do
  let fileName = "results/" ++ matricula ++ ".json"
  let totalTestes = cases testsResult
  let passaram = (tried testsResult) - (errors testsResult) - (failures testsResult)
  let excecoes = errors testsResult
  let falhas = failures testsResult
  let output = Output { matricula = matricula, falhas = falhas, passaram = passaram,
                          totalTestes = totalTestes,  excecoes = excecoes}
  I.writeFile fileName (encodeToLazyText output)
  T.putStrLn "Worked"
