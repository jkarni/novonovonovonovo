module NovoSpec where

import Test.Hspec
import Test.QuickCheck

import Novo


spec :: Spec
spec = describe "eval" $ do
    let isLit (Lit _) = True
        isLit _       = False

    it "does not change literals" $ property $ \x
      -> isLit x ==> eval x `shouldBe` x

instance Arbitrary Expr where
  arbitrary = frequency
    [ (1, Soma <$> arbitrary <*> arbitrary)
    , (1, Mult <$> arbitrary <*> arbitrary)
    , (1, Menos <$> arbitrary <*> arbitrary)
    , (10, Lit <$> arbitrary)
    ]
