module Helper where

import Control.Monad(guard, when)
import Control.Exception.Base
import Test.HUnit.Lang
import Test.HUnit.Base


assertError :: String -> t -> IO ()
assertError desiredErrorMessage action
    = handleJust isWanted (const $ return ()) $ do
        _ <- (evaluate action)
        assertFailure $ "Expected exception: " ++ desiredErrorMessage
  where isWanted (ErrorCall actualErrorMessage)
            = guard $ actualErrorMessage == desiredErrorMessage

assertFalse :: Bool -> IO ()
assertFalse res = when res $ assertFailure "Expected false but got true"

assertTrue :: Bool -> IO ()
assertTrue res = assertBool "Expected true but got false" res