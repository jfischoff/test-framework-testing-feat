-- | Allows Feat properties to be used with the test-framework package.
-- 
-- For an example of how to use test-framework, please see 
-- <http://github.com/batterseapower/test-framework/raw/master/example/Test/Framework/Example.lhs>
{-# LANGUAGE DeriveDataTypeable #-} 
{-# LANGUAGE TypeOperators #-} 
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
module Test.Framework.Providers.Feat (testFeat) where
import Test.Framework.Providers.API
import Test.Feat (values, Enumerable)
import Control.Arrow (second)
import Data.Typeable
import Debug.Trace

-- | Create a 'Test' for a Feat property
testFeat :: (Enumerable a, Show a) => TestName -> (a -> Bool) -> Test
testFeat name = Test name . Property

-- | Used to document numbers which we expect to be intermediate 
--   test counts from running properties
--   I don't think I am using this in any meaningful way
type PropertyTestCount = Int

instance TestResultlike PropertyTestCount PropertyStatus where
    testSucceeded = propertySucceeded

data PropertyStatus = PropertyOK                        
                    -- ^ The property is true as far as we could check it
                    | PropertyFalsifiable String
                    -- ^ The property was not true. The strings 
                    --   are the reason and the output.
                    deriving(Eq)
                    
instance Show PropertyStatus where
    show PropertyOK              = "Property OK"
    show (PropertyFalsifiable x) = "Property failed with " ++ x

propertySucceeded :: PropertyStatus -> Bool
propertySucceeded s = case s of
  PropertyOK -> True
  _          -> False

data Property = forall a. (Enumerable a, Show a) => Property { unProperty :: a -> Bool }
  deriving Typeable

instance Testlike PropertyTestCount PropertyStatus Property where
    runTest topts (Property testable) = runProperty topts testable
    testTypeName _ = "Properties"

traceIt x = trace (show x) x

runProperty :: (Enumerable a, Show a) 
            => CompleteTestOptions 
            -> (a -> Bool) 
            -> IO (PropertyTestCount :~> PropertyStatus, IO ())
runProperty topts test = do
    let K count = topt_maximum_generated_tests topts
        --This is just to help things type check
        toValues :: (Enumerable a, Show a) => (a -> Bool) -> [(Integer, [a])] -> [(Integer, [a])]
        toValues _ xs = xs 
        --There is probably a better way to get it to typecheck
        values' = toValues test values
        samples = take count . concatMap (\(i, xs) -> zip (repeat i) xs) . take count $ values' 
        results = map (second test) samples
        
    case map fst . filter ((==False) . snd . snd) . zip samples $ results of
            [] -> return (Finished PropertyOK, return ())
            -- I could easily return all the results that sort was overwhelming
            x:_ -> return . (, return ()) . Finished . PropertyFalsifiable . show $ x
        