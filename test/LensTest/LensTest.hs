module LensTest.LensTest (lensTest) where

import           Control.Lens
import           Data.Char
import           Data.Data.Lens
import           Data.Default
import           Data.Text.Lens
import           Test.HUnit

testList :: Test
testList = TestList [
  -- Lenses, Folds, and Traversals : https://www.youtube.com/watch?v=cefnmjtAolY
  TestCase $ assertEqual "fmap . fmap" ((fmap . fmap) (+1) [[1, 2, 3]]) [[2, 3, 4]],
  TestCase $ assertEqual "fmap . fmap . fmap" ((fmap . fmap . fmap) (+1) [[[1, 2, 3]]]) [[[2, 3, 4]]],
  TestCase $ assertEqual "fmap . fmap . fmap" ((fmap . fmap . fmap) (+1) [[[1, 2, 3]], [[4, 5, 6], [7, 8, 9]]]) [[[2, 3, 4]], [[5, 6, 7], [8, 9, 10]]],
  -- fmap law : fmap (f.g) = (fmap g) . (fmap f)
  TestCase $ assertEqual "fmap law" (fmap ((+1) . (+2)) [1, 2, 3]) (fmap (+2) $ fmap (+1) [1, 2, 3]),

  TestCase $ assertEqual "get : ^." (("hello", "world") ^._2) "world",
  TestCase $ assertEqual "get : ^." (("hello", ("world", "!!!")) ^._2^._1) "world",
  TestCase $ assertEqual "get'" (("hello", ("world", "!!!")) ^._2 . _1) "world",
  TestCase $ assertEqual "view" (view _1 ("hello", "world")) "hello",

  TestCase $ assertEqual "length" (("hello", ("world", "!!!")) ^. _2 ^. _1 . to length) 5,

  TestCase $ assertEqual "mapped" (over mapped succ [1, 2, 3]) [2, 3, 4],
  TestCase $ assertEqual "mapped.mapped" (over (mapped . mapped) length [["hello", "world"], ["!!!"]]) [[5, 5], [3]],
  TestCase $ assertEqual "mapped._2" (over (mapped . _2) succ [(1, 2), (3, 4)]) [(1, 3), (3, 5)],

  TestCase $ assertEqual "both" (over both (+1) (1, 2)) (2, 3),
  TestCase $ assertEqual "*~" (both *~ 2 $ (1, 2)) (2, 4),
  TestCase $ assertEqual "over not work in tuple" (over mapped (*2) (1, 2, 3)) (1, 2, 6),

  -- Control.Lens.Fold
  TestCase $ assertEqual "(^..)" ([[1, 2], [3]] ^.. id) [[[1, 2], [3]]],
  TestCase $ assertEqual "(^..)" ([[1, 2], [3]] ^.. traverse) [[1, 2], [3]],
  TestCase $ assertEqual "(^..)" ([[1, 2], [3]] ^.. traverse . traverse) [1, 2, 3],
  TestCase $ assertEqual "(^?)" (Left 4 ^? _Left) (Just 4),
  TestCase $ assertEqual "(^?)" (Right 4 ^? _Left :: Maybe Int) Nothing,
  TestCase $ assertEqual "(^?)" ("world" ^? ix 3) (Just 'l'),
  TestCase $ assertEqual "(^?)" ("world" ^? ix 10) Nothing,
  TestCase $ assertEqual "(^?!)" (Left 4 ^?! _Left) 4,
  TestCase $ assertEqual "(^?!)" ("world" ^?! ix 3) 'l',
  TestCase $ assertEqual "has" (has (element 0) []) False,
  TestCase $ assertEqual "has" (has _Left (Left 12)) True,
  TestCase $ assertEqual "has" (has _Right (Left 12)) False,
  TestCase $ assertEqual "hasn't" (hasn't _Left (Left 12)) False,
  TestCase $ assertEqual "hasn't" (hasn't _Left (Right 12)) True,
  TestCase $ assertEqual "folding" ([1, 2, 3, 4] ^.. folding reverse) [4, 3, 2, 1],
  TestCase $ assertEqual "foldring" ([1, 2, 3, 4] ^.. foldring foldr) [1, 2, 3, 4],
  TestCase $ assertEqual "folded" (Just 3 ^.. folded) [3],
  TestCase $ assertEqual "folded" ((Nothing :: Maybe Int) ^.. folded) [],
  TestCase $ assertEqual "folded" ([(1, 2), (3, 4)] ^.. folded . both) [1, 2, 3, 4],
  -- TestCase $ assertEqual "foldMapOf" (foldMapOf  sum [1, 2, 3] :: Int) 18,

  --Control.Lens.Getter
  TestCase $ assertEqual "(^..)" ((1, 2, 3, 4, 5) ^.. (_1 <> _3 <> _5)) [1, 3, 5],

  TestCase $ assertEqual "allOf" (allOf (folded . folded) isLower ["hello", "world"]) True,
  TestCase $ assertEqual "anyOf" (anyOf biplate (=="world") ("hello", (), ("hello", ("world", 1::Int)))) True,

  TestCase $ assertEqual "from to" ("hello" ^. from packed . to length) 5,

  -- Control.Lens.Setter
  TestCase $ assertEqual "over traverse" (over traverse length ["hello", "world"]) [5, 5],
  TestCase $ assertEqual "over _1" (over _1 length ("hello", "world")) (5, "world"),
  TestCase $ assertEqual "over traverse._1" (over (traverse . _1) length [("hello", "hello"), ("world", "world")]) [(5, "hello"), (5, "world")],
  TestCase $ assertEqual "over both" (over both length ("hello", "world")) (5, 5),
  TestCase $ assertEqual "over traverse.both" (over (traverse . both) length [("hello", "hello"), ("world", "world")]) [(5, 5), (5, 5)],
  TestCase $ assertEqual "over mapped" (over mapped length ["hello", "world"]) [5, 5],
  TestCase $ assertEqual "set mapped" (set mapped 1 ["hello", "world"]) [1, 1],
  TestCase $ assertEqual "+~" ((mapped +~ 1) [1, 2, 3]) [2, 3, 4],
  TestCase $ assertEqual "over Lifted" (over lifted Just [1, 2, 3]) [Just 1, Just 2, Just 3],
  TestCase $ assertEqual "set" (set _2 42 ("hello", "world")) ("hello", 42),
  TestCase $ assertEqual "set" (set (_2 . _1) 42 ("hello", ("world", "!!!"))) ("hello", (42, "!!!")),
  TestCase $ assertEqual ".~" ((1, 2) & _1 .~ "hello" & _2 .~ "world") ("hello", "world"),
  TestCase $ assertEqual ".~" (_1 . mapped .~ 'a' $ ("hello", "world")) ("aaaaa", "world"),
  TestCase $ assertEqual "%~" ((1, 2) & _1 %~ show & _2 %~ show) ("1", "2"),
  TestCase $ assertEqual "%~" (_1 . mapped %~ succ $ ("hello", "world")) ("ifmmp", "world"),
  TestCase $ assertEqual "both .~" ((1, 2) & both .~ "hello") ("hello", "hello")
  -- TODO others

  -- Control.Lens.Traversal
  ]

lensTest :: IO Counts
lensTest = runTestTT testList