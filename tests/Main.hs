-- | Test copilot-bluespec.
module Main where

-- External imports
import Test.Framework (Test, defaultMain)

-- Internal library modules being tested
import qualified Test.Copilot.Compile.Bluespec

-- | Run all unit tests on copilot-bluespec.
main :: IO ()
main = defaultMain tests

-- | All unit tests in copilot-bluespec.
tests :: [Test.Framework.Test]
tests =
  [ Test.Copilot.Compile.Bluespec.tests
  ]
