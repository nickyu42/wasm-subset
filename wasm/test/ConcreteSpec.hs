module ConcreteSpec where

import Test.Hspec
import Test.QuickCheck
import ConcreteInterpreter as Concrete
import Syntax
import Data.Concrete.Error as E

spec :: Spec
spec = do
    ex_prog
    ex_control_1

ex_prog = do
    let prog = [ ConstI32 5
               , ConstI32 10
               , Binary I32 Add ]
    it "Should return 15" $ Concrete.run prog `shouldBe` (E.Success ([I32Val 15], ()))

ex_control_1 = do
    let prog = [ ConstI32 20
               , Block [I32] [ ConstI32 30
                             , Br 0 ]
               , Binary I32 Add ]
    it "Should return 50" $ Concrete.run prog `shouldBe` (E.Success ([I32Val 50], ()))

