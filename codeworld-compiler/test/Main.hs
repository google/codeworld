import Data.Char
import Control.Monad
import System.Directory
import Test.HUnit             -- only import needed, others are optional

test1 = TestCase $ assertEqual "test upCase" "FOO" (map toUpper "foo")

