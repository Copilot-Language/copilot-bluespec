{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Test copilot-bluespec:Copilot.Compile.Bluespec.
module Test.Copilot.Compile.Bluespec
    ( tests )
  where

-- External imports
import Control.Arrow                        ((&&&))
import Control.Exception                    (IOException, catch)
import Control.Monad                        (when)
import Data.Bits                            (Bits, complement)
import Data.Foldable                        (foldl')
import Data.List                            (intercalate)
import Data.Type.Equality                   (testEquality)
import Data.Typeable                        (Proxy (..), (:~:) (Refl))
import GHC.TypeLits                         (KnownNat, natVal)
import Numeric.IEEE                         (nan)
import System.Directory                     (doesFileExist,
                                             getTemporaryDirectory,
                                             removeDirectoryRecursive,
                                             setCurrentDirectory)
import System.IO                            (hPutStrLn, stderr)
import System.Posix.Temp                    (mkdtemp)
import System.Process                       (callProcess, readProcess)
import System.Random                        (Random)
import Test.Framework                       (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck                      (Arbitrary, Gen, Property,
                                             arbitrary, choose, elements,
                                             forAll, forAllBlind, frequency,
                                             getPositive, ioProperty, once,
                                             oneof, vectorOf, withMaxSuccess,
                                             (.&&.))
import Test.QuickCheck.Gen                  (chooseAny, chooseBoundedIntegral)

-- External imports: Copilot
import Copilot.Core hiding (Property)

-- External imports: Modules being tested
import Copilot.Compile.Bluespec          (bluespecSettingsOutputDirectory,
                                          compile, compileWith,
                                          mkDefaultBluespecSettings)
import Copilot.Compile.Bluespec.External (External (extName), gatherExts)

-- * Constants

-- | All tests for copilot-bluespec:Copilot.Compile.Bluespec.
tests :: Test.Framework.Test
tests =
  testGroup "Copilot.Compile.Bluespec"
    [ testGroup "Unit tests"
      [ testProperty "Compile specification"               testCompile
      , testProperty "Compile specification in custom dir" testCompileCustomDir
      , testProperty "Run specification"                   testRun
      , testProperty "Run and compare results"             testRunCompare
      ]
    , testGroup "Regression tests"
      [ test15
      ]
    ]

-- * Individual tests

-- | Test compiling a spec.
testCompile :: Property
testCompile = once $ ioProperty $ do
    tmpDir <- getTemporaryDirectory
    setCurrentDirectory tmpDir

    testDir <- mkdtemp "CopilotTest"
    setCurrentDirectory testDir

    compile "CopilotTest" spec
    r <- compileBluespec "CopilotTest" []

    setCurrentDirectory tmpDir
    removeDirectoryRecursive testDir

    return r

  where

    spec = Spec streams observers triggers properties

    streams    = []
    observers  = []
    triggers   = [ Trigger function guard args ]
    properties = []

    function = "func"

    guard = Const Bool True

    args = []

-- | Test compiling a spec in a custom directory.
testCompileCustomDir :: Property
testCompileCustomDir = once $ ioProperty $ do
    tmpDir <- getTemporaryDirectory
    setCurrentDirectory tmpDir

    testDir <- mkdtemp "CopilotTest"

    compileWith (mkDefaultBluespecSettings
                   { bluespecSettingsOutputDirectory = testDir })
                "CopilotTest"
                spec

    setCurrentDirectory testDir
    r <- compileBluespec "CopilotTest" []

    setCurrentDirectory tmpDir
    removeDirectoryRecursive testDir

    return r

  where

    spec = Spec streams observers triggers properties

    streams    = []
    observers  = []
    triggers   = [ Trigger function guard args ]
    properties = []

    function = "nop"

    guard = Const Bool True

    args = []

-- | Test compiling a spec and running the resulting program.
--
-- The actual behavior is ignored.
testRun :: Property
testRun = once $ ioProperty $ do
    tmpDir <- getTemporaryDirectory
    setCurrentDirectory tmpDir

    testDir <- mkdtemp "CopilotTest"
    setCurrentDirectory testDir

    let bluespecProgram = unlines
          [ "package Top where"
          , ""
          , "import CopilotTest"
          , "import CopilotTestIfc"
          , "import CopilotTestTypes"
          , ""
          , "copilotTestIfc :: Module CopilotTestIfc"
          , "copilotTestIfc ="
          , "  module"
          , "    interface"
          , "      nop :: Action"
          , "      nop = return ()"
          , ""
          , "mkTop :: Module Empty"
          , "mkTop = mkCopilotTest copilotTestIfc"
          ]

    writeFile "Top.bs" bluespecProgram

    compile "CopilotTest" spec
    r <- compileBluespec "Top" ["-g", "mkTop"]

    -- Compile a main program
    r2 <- compileExecutable "mkTop"
    callProcess "./mkTop" ["-m", "2"]

    setCurrentDirectory tmpDir
    removeDirectoryRecursive testDir

    return $ r && r2

  where

    spec = Spec streams observers triggers properties

    streams    = []
    observers  = []
    triggers   = [ Trigger function guard args ]
    properties = []

    function = "nop"

    guard = Const Bool True

    args = []

-- | Test running compiled spec and comparing the results to the
-- expectation.
testRunCompare :: Property
testRunCompare =
  -- It takes a pretty long time to run these tests, so we set the maximum
  -- number of successful tests to 5 instead of the default (100) for the sake
  -- of making the test suite complete in a more reasonable amount of time.
  withMaxSuccess 5 $
       testRunCompare1 (arbitraryOpIntegralBool :: Gen (TestCase1 Int8   Bool))
  .&&. testRunCompare1 (arbitraryOpIntegralBool :: Gen (TestCase1 Int16  Bool))
  .&&. testRunCompare1 (arbitraryOpIntegralBool :: Gen (TestCase1 Int32  Bool))
  .&&. testRunCompare1 (arbitraryOpIntegralBool :: Gen (TestCase1 Int64  Bool))
  .&&. testRunCompare1 (arbitraryOpIntegralBool :: Gen (TestCase1 Word8  Bool))
  .&&. testRunCompare1 (arbitraryOpIntegralBool :: Gen (TestCase1 Word16 Bool))
  .&&. testRunCompare1 (arbitraryOpIntegralBool :: Gen (TestCase1 Word32 Bool))
  .&&. testRunCompare1 (arbitraryOpIntegralBool :: Gen (TestCase1 Word64 Bool))
  .&&. testRunCompare1 (arbitraryOpFloatingBool :: Gen (TestCase1 Float  Bool))
  .&&. testRunCompare1 (arbitraryOpFloatingBool :: Gen (TestCase1 Double Bool))
  .&&. testRunCompare1 (arbitraryOpStruct       :: Gen (TestCase1 MyStruct Int8))
  .&&. testRunCompare2 (arbitraryArrayNum       :: Gen (TestCase2 (Array 2 Int8) Word32 Int8))
  .&&. testRunCompare2 (arbitraryArrayNum       :: Gen (TestCase2 (Array 2 Int16) Word32 Int16))

-- * Regression tests

-- | Regression tests for
-- https://github.com/Copilot-Language/copilot-bluespec/issues/15 which ensure
-- that @copilot-bluespec@ generates valid code for comparison operators (('<'),
-- ('<='), ('>'), and ('>=')) that are capable of handling NaN values.
test15 :: Test.Framework.Test
test15 =
    testGroup "#15"
    [ testProperty "Generates valid (<) code for NaNs" $
      mkRegressionTest2 (Lt Double) (zipWith (<)) vals
    , testProperty "Generates valid (<=) code for NaNs" $
      mkRegressionTest2 (Le Double) (zipWith (<=)) vals
    , testProperty "Generates valid (>) code for NaNs" $
      mkRegressionTest2 (Gt Double) (zipWith (>)) vals
    , testProperty "Generates valid (>=) code for NaNs" $
      mkRegressionTest2 (Ge Double) (zipWith (>=)) vals
    ]
  where
    vals :: [(Double, Double)]
    vals = [(0, nan), (nan, 0)]

-- | Test the behavior of a binary operation (an @'Op2' a b c@ value) against
-- its expected behavior (as a Haskell function of type @[a] -> [b] -> [c]@)
-- using the supplied inputs (of type @[(a, b)]@). This function is intended to
-- be used to construct regression tests.
mkRegressionTest2 :: (Typed a, Typed b, Typed c,
                      Eq c, ReadableFromBluespec c)
                  => Op2 a b c
                  -> ([a] -> [b] -> [c])
                  -> [(a, b)]
                  -> Property
mkRegressionTest2 op haskellFun vals =
    let spec = alwaysTriggerArg1 (UExpr t3 appliedOp)
        appliedOp = Op2 op (ExternVar t1 varName1 Nothing)
                           (ExternVar t2 varName2 Nothing)

        len = length vals
        (vals1, vals2) = unzip vals
        inputs  = filterOutUnusedExts
                    spec
                    [ (typeBluespec t1,
                       fmap (bluespecShow t1) vals1,
                       varName1)
                    , (typeBluespec t2,
                       fmap (bluespecShow t2) vals2,
                       varName2)
                    ]
        outputs = haskellFun vals1 vals2 in

    once $
    testRunCompareArg
      inputs len outputs spec (typeBluespec t3)

  where

    t1 = typeOf
    t2 = typeOf
    t3 = typeOf

    varName1 = "input1"
    varName2 = "input2"

-- * Random generators

-- ** Random function generators

-- | Generator of functions that produce booleans.
arbitraryOpBool :: Typed a => Gen (Fun a Bool, [a] -> [Bool])
arbitraryOpBool =
  frequency
    [ (5, arbitraryOp1Any)
    , (5, funCompose1 <$> arbitraryOp1Bool <*> arbitraryOpBool)
    , (2, funCompose2 <$> arbitraryOp2Bool <*> arbitraryOpBool <*> arbitraryOpBool)
    , (1, funCompose2 <$> arbitraryOp2Eq   <*> arbitraryOpBool <*> arbitraryOpBool)
    ]

-- | Generator of functions that take Bits and produce booleans.
arbitraryOpBoolBits :: (Typed a, Bits a) => Gen (Fun a Bool, [a] -> [Bool])
arbitraryOpBoolBits =
  frequency
    [ (1, funCompose2 <$> arbitraryOp2Eq <*> arbitraryOpBits <*> arbitraryOpBits)
    ]

-- | Generator of functions that take Nums and produce booleans.
arbitaryOpBoolOrdEqNum :: (Typed a, Eq a, Ord a, Num a)
                       => Gen (Fun a Bool, [a] -> [Bool])
arbitaryOpBoolOrdEqNum =
  frequency
    [ (1, funCompose2 <$> arbitraryOp2Eq  <*> arbitraryOpNum <*> arbitraryOpNum)
    , (1, funCompose2 <$> arbitraryOp2Ord <*> arbitraryOpNum <*> arbitraryOpNum)
    ]

-- | Generator of functions that take Floating point numbers and produce
-- booleans.
arbitraryOpBoolEqNumFloat :: (Typed t, Eq t, Num t, Floating t)
                          => Gen (Fun t Bool, [t] -> [Bool])
arbitraryOpBoolEqNumFloat =
  frequency
    [ (1, funCompose2 <$> arbitraryOp2Eq <*> arbitraryOpNum   <*> arbitraryOpFloat)
    , (1, funCompose2 <$> arbitraryOp2Eq <*> arbitraryOpFloat <*> arbitraryOpNum)
    ]

-- | Generator of functions that take and produce Bits.
arbitraryOpBits :: (Bits t, Typed t)
                => Gen (Fun t t, [t] -> [t])
arbitraryOpBits = elements
  [ (Op1 (BwNot typeOf), fmap complement)
  ]

-- | Generator of functions that take and produce Nums.
arbitraryOpNum :: (Typed t, Num t) => Gen (Fun t t, [t] -> [t])
arbitraryOpNum = elements
  [ (Op1 (Abs typeOf),  fmap abs)
  , (Op1 (Sign typeOf), fmap signum)
  ]

-- | Generator of functions that take an arrays and indicates and produce
-- elements from the array.
arbitraryArrayIx :: forall t n . (Typed t, KnownNat n, Num t)
                 => Gen ( Fun2 (Array n t) Word32 t
                        , [Array n t] -> [Word32] -> [t]
                        )
arbitraryArrayIx = return
  (Op2 (Index typeOf), zipWith (\x y -> arrayElems x !! fromIntegral y))

-- | Generator of functions that take structs produce fields of the struct.
arbitraryStructField :: Gen ( Fun MyStruct Int8
                            , [MyStruct] -> [Int8]
                            )
arbitraryStructField = elements
  [ (Op1 (GetField typeOf typeOf myStruct1), fmap (unField . myStruct1))
  , (Op1 (GetField typeOf typeOf myStruct2), fmap (unField . myStruct2))
  ]

-- | Generator of functions on Floating point numbers.
arbitraryOpFloat :: (Floating t, Typed t) => Gen (Fun t t, [t] -> [t])
arbitraryOpFloat = elements
  [ (Op1 (Sqrt typeOf),  fmap sqrt)
  , (Op1 (Recip typeOf), fmap recip)
  ]

-- | Generator of functions on that produce elements of any type.
arbitraryOp1Any :: forall a b
                .  (Arbitrary b, Typed a, Typed b)
                => Gen (Fun a b, [a] -> [b])
arbitraryOp1Any = oneof $
    [ (\v -> (\_ -> Const typeOf v, fmap (const v))) <$> arbitrary ]
    ++
    rest
  where
    rest | Just Refl <- testEquality t1 t2
         = [return (id, id)]
         | otherwise
         = []

    t1 :: Type a
    t1 = typeOf

    t2 :: Type b
    t2 = typeOf

-- | Generator of functions on Booleans.
arbitraryOp1Bool :: Gen (Fun Bool Bool, [Bool] -> [Bool])
arbitraryOp1Bool = elements
  [ (Op1 Not, fmap not)
  ]

-- | Generator of binary functions on Booleans.
arbitraryOp2Bool :: Gen (Fun2 Bool Bool Bool, [Bool] -> [Bool] -> [Bool])
arbitraryOp2Bool = elements
  [ (Op2 And, zipWith (&&))
  , (Op2 Or,  zipWith (||))
  ]

-- | Generator of binary functions that take two Eq elements of the same type
-- and return a Bool.
arbitraryOp2Eq :: (Typed t, Eq t)
               => Gen (Fun2 t t Bool, [t] -> [t] -> [Bool])
arbitraryOp2Eq = elements
  [ (Op2 (Eq typeOf), zipWith (==))
  , (Op2 (Ne typeOf), zipWith (/=))
  ]

-- | Generator of binary functions that take two Ord elements of the same type
-- and return a Bool.
arbitraryOp2Ord :: (Typed t, Ord t)
                => Gen (Fun2 t t Bool, [t] -> [t] -> [Bool])
arbitraryOp2Ord = elements
  [ (Op2 (Le typeOf), zipWith (<=))
  , (Op2 (Lt typeOf), zipWith (<))
  , (Op2 (Ge typeOf), zipWith (>=))
  , (Op2 (Gt typeOf), zipWith (>))
  ]

-- ** Random data generators

-- | Random array generator.
arbitraryArray :: forall n t . (KnownNat n, Random t) => Gen (Array n t)
arbitraryArray = array <$> vectorOf len chooseAny
  where
   len :: Int
   len = fromIntegral $ natVal (Proxy :: Proxy n)

-- | Random struct generator.
arbitraryStruct :: Gen MyStruct
arbitraryStruct = do
   fld1 <- Field <$> gen
   fld2 <- Field <$> gen
   return $ MyStruct fld1 fld2
  where
   gen :: Gen Int8
   gen = chooseBoundedIntegral (minBound, maxBound)

-- ** Random test case generators

-- | Generator for test cases on integral numbers that produce booleans.
arbitraryOpIntegralBool :: (Typed t, Bounded t, Integral t, Bits t)
                        => Gen (TestCase1 t Bool)
arbitraryOpIntegralBool = frequency
  [ (5, mkTestCase1
          arbitraryOpBool
          (chooseBoundedIntegral (minBound, maxBound)))

  , (2, mkTestCase1
          arbitraryOpBoolBits
          (chooseBoundedIntegral (minBound, maxBound)))

    -- we need to use +1 because certain operations overflow the number
  , (2, mkTestCase1
          arbitaryOpBoolOrdEqNum
          (chooseBoundedIntegral (minBound + 1, maxBound)))
  ]

-- | Generator for test cases on floating-point numbers that produce booleans.
arbitraryOpFloatingBool :: (Random t, Typed t, Floating t, Eq t)
                        => Gen (TestCase1 t Bool)
arbitraryOpFloatingBool = oneof
  [ mkTestCase1 arbitraryOpBoolEqNumFloat chooseAny
  ]

-- | Generator for test cases on Arrays selection producing values of the
-- array.
arbitraryArrayNum :: forall n a
                  .  (KnownNat n, Num a, Random a, Typed a)
                  => Gen (TestCase2 (Array n a) Word32 a)
arbitraryArrayNum = oneof
    [ mkTestCase2 arbitraryArrayIx arbitraryArray gen
    ]
  where
   gen :: Gen Word32
   gen = choose (0, len - 1)

   len :: Word32
   len = fromIntegral $ natVal (Proxy :: Proxy n)

-- | Generator for test cases on structs that produce fields of the struct.
arbitraryOpStruct :: Gen (TestCase1 MyStruct Int8)
arbitraryOpStruct = oneof
    [ mkTestCase1
        arbitraryStructField
        arbitraryStruct
    ]

-- * Semantics

-- ** Functions

-- | Unary Copilot function.
type Fun a b = Expr a -> Expr b

-- | Binary Copilot function.
type Fun2 a b c = Expr a -> Expr b -> Expr c

-- | Compose functions, paired with the Haskell functions that define their
-- idealized meaning.
funCompose1 :: (Fun b c, [b] -> [c])
            -> (Fun a b, [a] -> [b])
            -> (Fun a c, [a] -> [c])
funCompose1 (f1, g1) (f2, g2) = (f1 . f2, g1 . g2)

-- | Compose a binary function, with two functions, one for each argument.
funCompose2 :: (Fun2 b c d, [b] -> [c] -> [d])
            -> (Fun a b, [a] -> [b])
            -> (Fun a c, [a] -> [c])
            -> (Fun a d, [a] -> [d])
funCompose2 (f1, g1) (f2, g2) (f3, g3) =
  (uncurry f1 . (f2 &&& f3), uncurry g1 . (g2 &&& g3))

-- ** Test cases

-- | Test case specification for specs with one input variable and one output.
data TestCase1 a b = TestCase1
  { wrapTC1Expr :: Spec
    -- ^ Specification containing a trigger an extern of type 'a' and a trigger
    -- with an argument of type 'b'.

  , wrapTC1Fun :: [a] -> [b]
    -- ^ Function expected to function in the same way as the Spec being
    -- tested.

  , wrapTC1CopInp :: (Type a, String, Gen a)
    -- ^ Input specification.
    --
    -- - The first element contains the type of the input in Bluespec.
    --
    -- - The second contains the variable name in Bluespec.
    --
    -- - The latter contains a randomized generator for values of the given
    -- type.

  , wrapTC1CopOut :: Type b
    -- ^ The type of the output in Bluespec.
  }

-- | Test case specification for specs with two input variables and one output.
data TestCase2 a b c = TestCase2
  { wrapTC2Expr :: Spec
    -- ^ Specification containing a trigger an extern of type 'a' and a trigger
    -- with an argument of type 'b'.

  , wrapTC2Fun :: [a] -> [b] -> [c]
    -- ^ Function expected to function in the same way as the Spec being
    -- tested.

  , wrapTC2CopInp1 :: (Type a, String, Gen a)
    -- ^ Input specification for the first input.
    --
    -- - The first element contains the type of the input in Bluespec.
    --
    -- - The second contains the variable name in Bluespec.
    --
    -- - The latter contains a randomized generator for values of the given
    -- type.

  , wrapTC2CopInp2 :: (Type b, String, Gen b)
    -- ^ Input specification for the second input.
    --
    -- - The first element contains the type of the input in Bluespec.
    --
    -- - The second contains the variable name in Bluespec.
    --
    -- - The latter contains a randomized generator for values of the given
    -- type.

  , wrapTC2CopOut :: Type c
    -- ^ The type of the output in Bluespec.
  }

-- | Generate test cases for expressions that behave like unary functions.
mkTestCase1 :: (Typed a, Typed b)
            => Gen (Fun a b, [a] -> [b])
            -> Gen a
            -> Gen (TestCase1 a b)
mkTestCase1 genO gen = do
    (copilotF, semF) <- genO

    let spec = alwaysTriggerArg1 (UExpr t2 appliedOp)
        appliedOp = copilotF (ExternVar t1 varName Nothing)

    return $
      TestCase1
        { wrapTC1Expr = spec
        , wrapTC1Fun = semF
        , wrapTC1CopInp = ( t1, varName, gen )
        , wrapTC1CopOut = t2
        }

  where

    t1 = typeOf
    t2 = typeOf

    varName = "input"

-- | Generate test cases for expressions that behave like binary functions.
mkTestCase2 :: (Typed a, Typed b, Typed c)
            => Gen (Fun2 a b c, [a] -> [b] -> [c])
            -> Gen a
            -> Gen b
            -> Gen (TestCase2 a b c)
mkTestCase2 genO genA genB = do
    (copilotF, semF) <- genO

    let spec = alwaysTriggerArg1 (UExpr t3 appliedOp)
        appliedOp = copilotF (ExternVar t1 varName1 Nothing)
                             (ExternVar t2 varName2 Nothing)

    return $
      TestCase2
        { wrapTC2Expr = spec
        , wrapTC2Fun = semF
        , wrapTC2CopInp1 = ( t1, varName1, genA )
        , wrapTC2CopInp2 = ( t2, varName2, genB )
        , wrapTC2CopOut = t3
        }

  where

    t1 = typeOf
    t2 = typeOf
    t3 = typeOf

    varName1 = "input1"
    varName2 = "input2"

-- | Test running a compiled Bluespec program and comparing the results.
testRunCompare1 :: (Show a, Typed a, ReadableFromBluespec b, Eq b, Typed b)
                => Gen (TestCase1 a b) -> Property
testRunCompare1 ops =
  forAllBlind ops $ \testCase ->
    let (TestCase1
           { wrapTC1Expr = copilotSpec
           , wrapTC1Fun = haskellFun
           , wrapTC1CopInp = inputVar
           , wrapTC1CopOut = outputType
           }) = testCase
        (bluespecTypeInput, bluespecInputName, gen) = inputVar

    in forAll (getPositive <$> arbitrary) $ \len ->

         forAll (vectorOf len gen) $ \nums -> do

         let inputs  = filterOutUnusedExts
                         copilotSpec
                         [ (typeBluespec bluespecTypeInput,
                            fmap (bluespecShow bluespecTypeInput) nums,
                            bluespecInputName)
                         ]
             outputs = haskellFun nums

         testRunCompareArg
           inputs len outputs copilotSpec (typeBluespec outputType)

-- | Test running a compiled Bluespec program and comparing the results.
testRunCompare2 :: (Show a1, Typed a1, Show a2, Typed a2,
                    ReadableFromBluespec b, Eq b, Typed b)
                => Gen (TestCase2 a1 a2 b) -> Property
testRunCompare2 ops =
  forAllBlind ops $ \testCase ->
    let (TestCase2
           { wrapTC2Expr = copilotSpec
           , wrapTC2Fun = haskellFun
           , wrapTC2CopInp1 = inputVar1
           , wrapTC2CopInp2 = inputVar2
           , wrapTC2CopOut = outputType
           }) =
          testCase

        (bluespecTypeInput1, bluespecInputName1, gen1) = inputVar1
        (bluespecTypeInput2, bluespecInputName2, gen2) = inputVar2

    in forAll (getPositive <$> arbitrary) $ \len ->
       forAll (vectorOf len gen1) $ \nums1 ->
       forAll (vectorOf len gen2) $ \nums2 -> do
         let inputs  = filterOutUnusedExts
                         copilotSpec
                         [ (typeBluespec bluespecTypeInput1,
                            fmap (bluespecShow bluespecTypeInput1) nums1,
                            bluespecInputName1)
                         , (typeBluespec bluespecTypeInput2,
                            fmap (bluespecShow bluespecTypeInput2) nums2,
                            bluespecInputName2)
                         ]
             outputs = haskellFun nums1 nums2

         testRunCompareArg
           inputs len outputs copilotSpec (typeBluespec outputType)

-- | Test running a compiled Bluespec program and comparing the results, when
-- the program produces one output as an argument to a trigger that always
-- fires.
--
-- PRE: all lists (second argument) of inputs have the length given as second
-- argument.
--
-- PRE: the monitoring code this is linked against uses the function
-- @printBack@ with exactly one argument to pass the results.
testRunCompareArg :: (ReadableFromBluespec b, Eq b)
                  => [(String, [String], String)]
                  -> Int
                  -> [b]
                  -> Spec
                  -> String
                  -> Property
testRunCompareArg inputs numInputs nums spec outputType =
  ioProperty $ do
    tmpDir <- getTemporaryDirectory
    setCurrentDirectory tmpDir

    -- Operate in temporary directory
    testDir <- mkdtemp "CopilotTest"
    setCurrentDirectory testDir

    -- Produce wrapper program
    let bluespecProgram =
          testRunCompareArgBluespecProgram inputs outputType
    writeFile "Top.bs" bluespecProgram

    -- Produce copilot monitoring code
    compile "CopilotTest" spec
    r <- compileBluespec "Top" ["-g", "mkTop"]

    -- Compile main program
    r2 <- compileExecutable "mkTop"

    -- Print result so far (for debugging purposes only)
    {-
    print r2
    print testDir
    -}

    -- Run program and compare result
    out <- readProcess "./mkTop" ["-m", show (numInputs + 2)] ""
    let outNums = readFromBluespec <$> lines out
        comparison = outNums == nums

    -- Only clean up if the test succeeded; otherwise, we want to inspect it.
    when comparison $ do
      -- Remove temporary directory
      setCurrentDirectory tmpDir
      removeDirectoryRecursive testDir

    return $ r && r2 && comparison

-- | Return a wrapper Bluespec program that runs for a number of clock cycles,
-- updating external stream registers on every cycle, running the monitors, and
-- publishing the results of any outputs.
testRunCompareArgBluespecProgram
  :: [(String, [String], String)]
  -> String
  -> String
testRunCompareArgBluespecProgram inputs outputType = unlines $
    [ "package Top where"
    , ""
    , "import FloatingPoint"
    , "import Vector"
    , ""
    , "import CopilotTest"
    , "import CopilotTestIfc"
    , "import CopilotTestTypes"
    , ""
    ]
    ++ inputVecDecls ++
    [ ""
    , "copilotTestIfc :: Module CopilotTestIfc"
    , "copilotTestIfc ="
    , "  module"
    ]
    ++ inputRegs ++
    [ "    i :: Reg (Bit 64) <- mkReg 0"
    , "    ready :: Reg Bool <- mkReg False"
    , "    interface"
    , "      printBack :: " ++ outputType ++ " -> Action"
    , "      printBack num = $display (fshow num)"
    , "                      when ready"
    , ""
    ]
    ++ inputMethods ++
    [ ""
    , "    rules"
    , "      \"inputs\": when True ==> do"
    ]
    ++ inputUpdates ++
    [ "        i := i + 1"
    , "        ready := True"
    , ""
    , "mkTop :: Module Empty"
    , "mkTop = mkCopilotTest copilotTestIfc"
    ]
  where
    inputVecDecls :: [String]
    inputVecDecls =
      concatMap
        (\(bluespecType, _varName, _regName, inputVecName, inputVals) ->
          [ inputVecName ++ " :: Vector " ++ show (length inputVals) ++
            " (" ++ bluespecType ++ ")"
          , inputVecName ++ " = " ++ genVector inputVals
          ])
        vars

    inputRegs :: [String]
    inputRegs =
      map
        (\(bluespecType, _varName, regName, _inputVecName, _inputVals) ->
          "    " ++ regName ++ " :: Reg (" ++ bluespecType ++ ") <- mkRegU")
        vars

    inputMethods :: [String]
    inputMethods =
      concatMap
        (\(bluespecType, varName, regName, _inputVecName, _inputVals) ->
          [ "      " ++ varName ++ " :: Reg (" ++ bluespecType ++ ")"
          , "      " ++ varName ++ " = " ++ regName
          ])
        vars

    inputUpdates :: [String]
    inputUpdates =
      map
        (\(_bluespecType, _varName, regName, inputVecName, _inputVals) ->
          "        " ++ regName ++ " := select " ++ inputVecName ++ " i")
        vars

    vars = map oneInput inputs
    oneInput (bluespecTypeInput, inputVals, bluespecInputName) =
        (bluespecTypeInput, inputVarName, inputRegVarName, inputVecVarName,
         inputVals)
      where
        inputVarName    = bluespecInputName
        inputRegVarName = bluespecInputName ++ "Impl"
        inputVecVarName = bluespecInputName ++ "Inputs"

-- * Auxiliary functions

-- ** Specs handling

-- | Build a 'Spec' that triggers at every step, passing the given expression
-- as argument, and execution a function 'printBack'.
alwaysTriggerArg1 :: UExpr -> Spec
alwaysTriggerArg1 = triggerArg1 (Const Bool True)

  where

    -- | Build a 'Spec' that triggers based on a given boolean stream, passing
    -- the given expression as argument, and execution a function 'printBack'.
    triggerArg1 :: Expr Bool -> UExpr -> Spec
    triggerArg1 guard expr = Spec streams observers triggers properties

      where

        streams    = []
        observers  = []
        properties = []

        triggers = [ Trigger function guard args ]
        function = "printBack"
        args     = [ expr ]

-- | Filter out any elements in the input list (of type @[(a, b, String)]@)
-- whose first element (the name of an external stream) does not correspond to
-- the name of an external stream in the supplied 'Spec'. For example, a Copilot
-- source program may declare external streams, but if none of them are used in
-- the 'Spec', then the 'Spec' value itself will not contain any external stream
-- definitions. As a result, we want to ensure that the input list also does not
-- contain any external streams.
filterOutUnusedExts :: Spec -> [(a, b, String)] -> [(a, b, String)]
filterOutUnusedExts spec = filter (\(_, _, name) -> name `elem` extNames)
  where
    extNames = map extName $ gatherExts (specStreams spec) (specTriggers spec)

-- ** Compilation of Bluespec programs

-- | Compile a Bluespec file given its basename.
compileBluespec :: String -> [String] -> IO Bool
compileBluespec baseName extraArgs = do
  result <- catch (do callProcess "bsc" $ extraArgs ++
                                          [ "-sim", "-quiet", "-u",
                                            -- We suppress the G0023 warning,
                                            -- which arises due to the `nop`
                                            -- triggers defined above. See the
                                            -- DESIGN.md document for more
                                            -- details on what these warning
                                            -- codes mean.
                                            "-suppress-warnings", "G0023:S0080",
                                            baseName ++ ".bs" ]
                      return True
                  )
                  (\e -> do
                     hPutStrLn stderr $
                       "copilot-bluespec: error: compileBluespec: "
                         ++ "cannot compile " ++ baseName ++ ".bs with bsc"
                     hPutStrLn stderr $
                       "copilot-bluespec: exception: " ++ show (e :: IOException)
                     return False
                  )
  if result
    then doesFileExist $ baseName ++ ".bo"
    else return False

-- | Compile a Bluespec file into an executable given its basename.
compileExecutable :: String -> IO Bool
compileExecutable topExe = do
  result <- catch (do callProcess "bsc" $ [ "-sim", "-quiet" ]
                                          ++ [ "-e", topExe ]
                                          ++ [ "-o", topExe ]
                      return True
                  )
                  (\e -> do
                     hPutStrLn stderr $
                       "copilot-bluespec: error: compileExecutable: "
                         ++ "cannot compile " ++ topExe ++ " with bsc"
                     hPutStrLn stderr $
                       "copilot-bluespec: exception: "
                         ++ show (e :: IOException)
                     return False
                  )
  if result
    then doesFileExist topExe
    else return False

-- ** Interfacing between Haskell and Bluespec

-- | Bluespec type used to store values of a given type.
typeBluespec :: Typed a => Type a -> String
typeBluespec Bool         = "Bool"
typeBluespec Int8         = "Int 8"
typeBluespec Int16        = "Int 16"
typeBluespec Int32        = "Int 32"
typeBluespec Int64        = "Int 64"
typeBluespec Word8        = "UInt 8"
typeBluespec Word16       = "UInt 16"
typeBluespec Word32       = "UInt 32"
typeBluespec Word64       = "UInt 64"
typeBluespec Float        = "Float"
typeBluespec Double       = "Double"
typeBluespec t@(Array tE) =
  "Vector " ++ show (typeLength t)  ++ "(" ++ typeBluespec tE ++ ")"
typeBluespec (Struct s)   = typeName s

-- | Show a value of a given type in Bluespec.
bluespecShow :: Type a -> a -> String
bluespecShow Bool       x = show x
bluespecShow Int8       x = bluespecShowIntegral x
bluespecShow Int16      x = bluespecShowIntegral x
bluespecShow Int32      x = bluespecShowIntegral x
bluespecShow Int64      x = bluespecShowIntegral x
bluespecShow Word8      x = bluespecShowIntegral x
bluespecShow Word16     x = bluespecShowIntegral x
bluespecShow Word32     x = bluespecShowIntegral x
bluespecShow Word64     x = bluespecShowIntegral x
bluespecShow Float      x = bluespecShowRealFloat x
bluespecShow Double     x = bluespecShowRealFloat x
bluespecShow (Array tE) x = genVector $ map (bluespecShow tE) $ arrayElems x
bluespecShow (Struct s) x =
  typeName s
    ++ "{ "
    ++ intercalate ";"
         (map
           (\(Value fldTy fld@(Field val)) ->
             fieldName fld ++ " = " ++ bluespecShow fldTy val)
           (toValues x))
    ++ "}"

-- | Show a value of a integral type (e.g., 'Int8' or 'Word8').
bluespecShowIntegral :: (Integral a, Num a, Ord a, Show a) => a -> String
bluespecShowIntegral x
  | x >= 0    = show x
  -- Bluespec Haskell doesn't have negative integer literals, so something like
  -- "-42" won't parse. Instead, we must rely on Bluespec's `negate` function.
  --
  -- We must be careful to negate an Integer literal rather than than a
  -- fixed-precision literal. For instance, suppose we wanted to display
  -- (-128 :: Int8). We wouldn't want to do this by displaying `negate 128`,
  -- since 128 isn't a valid Int8 value—the maximum Int8 value is 127!
  -- Instead, we want to display `fromInteger (negate 128)`, where 128 is an
  -- Integer. This way, `negate` can turn `128` to `-128` without issues.
  | otherwise = "fromInteger (negate " ++ show (abs (toInteger x)) ++ ")"

-- | Show a value of a floating-point type (e.g., 'Float' or 'Double'). We make
-- sure to convert NaN and infinity values to the corresponding Bluespec
-- @FloatingPoint@ functions that construct these values.
bluespecShowRealFloat :: (Num a, Ord a, RealFloat a, Show a) => a -> String
bluespecShowRealFloat x
  | isNaN x      = "qnan"
  | isInfinite x = "infinity " ++ show isNeg
  | isNeg        = "negate " ++ show x
  | otherwise    = show x
  where
    isNeg = x < 0

-- | Given a list of elements as arguments, show a @Vector@ expression. For
-- example, @'genVector' [\"27\", \"42\"]@ will return
-- @\"updateVector (updateVector newVector 0 27) 1 42)\"@.
genVector :: [String] -> String
genVector vals =
  snd $
  foldl'
    (\(!i, !v) x ->
      (i+1, "update (" ++ v ++ ") " ++ show i ++ " (" ++ x ++ ")"))
    (0 :: Int, "newVector")
    vals

-- | Read a value of a given type in Bluespec.
class ReadableFromBluespec a where
  readFromBluespec :: String -> a

instance ReadableFromBluespec Bool where
  readFromBluespec = read

instance ReadableFromBluespec Int8 where
  readFromBluespec = read

instance ReadableFromBluespec Int16 where
  readFromBluespec = read

instance ReadableFromBluespec Int32 where
  readFromBluespec = read

instance ReadableFromBluespec Int64 where
  readFromBluespec = read

instance ReadableFromBluespec Word8 where
  readFromBluespec = read

instance ReadableFromBluespec Word16 where
  readFromBluespec = read

instance ReadableFromBluespec Word32 where
  readFromBluespec = read

instance ReadableFromBluespec Word64 where
  readFromBluespec = read

-- ** A simple struct definition for unit testing purposes

data MyStruct = MyStruct
  { myStruct1 :: Field "myStruct1" Int8
  , myStruct2 :: Field "myStruct2" Int8
  }

instance Struct MyStruct where
  typeName _ = "MyStruct"
  toValues ms = [ Value Int8 (myStruct1 ms)
                , Value Int8 (myStruct2 ms)
                ]

instance Typed MyStruct where
  typeOf = Struct (MyStruct (Field 0) (Field 0))

-- | Unwrap a 'Field' to obtain the underlying value.
unField :: Field s t -> t
unField (Field val) = val
