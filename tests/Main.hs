{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}
module Main where
import Test.Framework 
import Test.Framework.Providers.Feat
import Test.Feat
import Data.Typeable
import Data.Monoid
import Control.Applicative
import Test.Framework.Options

data Expr = Add Expr Expr
          | I Int
          deriving(Show, Read, Eq, Typeable)

deriveEnumerable ''Expr

showReadRoundTrip :: Expr -> Bool
showReadRoundTrip a = a == (read . show) a

defOptions = mempty

-- This should fail 
showReadRoundTrip' :: Expr -> Bool
showReadRoundTrip' a = Add a a == (read . show) a

main = defaultMainWithOpts [
        testFeat "should work"    showReadRoundTrip
        --, testFeat "this shouldn't" showReadRoundTrip'
    ] (defOptions { ropt_test_options = (\x -> x { topt_maximum_generated_tests = Just 10000 }) <$> Just mempty })