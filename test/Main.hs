import TypeTest (typeTest)
import LensTest.LensTest (lensTest)
import LensTest.LensExerciseTest (lensExerciseTest)
import Test.HUnit

main :: IO ()
main = do
  -- typeTest
  lensTest
  lensExerciseTest
  print "Finish test"