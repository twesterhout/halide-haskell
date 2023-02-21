module Utils
  ( shouldContainText
  , shouldNotContainText
  , appearsBeforeText
  , testOnGpu
  , (&)
  , void
  , T.hPutStrLn
  , stderr
  )
where

import Control.Monad (void)
import Data.Function ((&))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Language.Halide.Target
import System.IO (stderr)
import Test.Hspec

shouldContainText :: Text -> Text -> Expectation
a `shouldContainText` b = T.unpack a `shouldContain` T.unpack b

shouldNotContainText :: Text -> Text -> Expectation
a `shouldNotContainText` b = T.unpack a `shouldNotContain` T.unpack b

appearsBeforeText :: Text -> Text -> Text -> Expectation
appearsBeforeText a b t = do
  t `shouldContainText` b
  fst (T.breakOn b t) `shouldContainText` a

testOnGpu :: (Target -> Expectation) -> Expectation
testOnGpu f =
  case gpuTarget of
    Just t -> f t
    Nothing -> pendingWith "no GPU target available"
