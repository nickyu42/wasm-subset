module ConcreteSpec where

import Test.Hspec
import Test.QuickCheck
import ConcreteInterpreter
import Syntax as S
import Data.Concrete.Error as E

spec :: Spec
spec = do
    ex_prog

ex_prog = do
    let prog = [S.Plain (S.ConstI32 5), S.Plain (S.ConstI32 10), S.Plain (S.Binary S.I32 S.Add)]
    it "Should return 15" $ run prog `shouldBe` (E.Success ([I32Val 15], []))

