{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

module GenerateFile (main) where

import TestIntersection as In
import TestUnion as Un
import TestRemove as Rm
import TestInsert as Insert
import TestInclusion as Inclusion
import TestMinus as Minus
import TestSize as Size
import TestSearch as Search
import TestSum as Sum
import Test.HUnit
import Data.List.Split
import GHC.Generics
import System.IO as T
import Data.Text.Lazy (Text)
import Data.Text.Lazy.IO as I
import Data.Aeson.Text (encodeToLazyText)
import Data.Aeson (ToJSON)

data Output = Output { matricula :: [Char], falhas :: Int,
                    passaram :: Int, totalTestes :: Int, excecoes :: Int} deriving (Show, Generic, ToJSON)


main matricula = do
  s <- runTestTT $ test $ mconcat [ Un.tests, In.tests, Rm.tests, Insert.tests, Inclusion.tests, Minus.tests, Size.tests, Search.tests, Sum.tests]
  generateResult matricula s

generateResult :: String -> Counts -> IO ()
generateResult fileName str = do
  let formattedMatricula = splitOn "." fileName
  let matricula = formattedMatricula !! 0
  let totalTestes = cases str
  let passaram = (tried str) - (errors str) - (failures str)
  let excecoes = errors str
  let falhas = failures str
  let output = Output { matricula = matricula, falhas = falhas, passaram = passaram,
                          totalTestes = totalTestes,  excecoes = excecoes}
  I.writeFile "result.json" (encodeToLazyText output)
  T.putStrLn "Worked"
