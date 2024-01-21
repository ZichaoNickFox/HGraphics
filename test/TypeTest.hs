module TypeTest (typeTest) where

import HGraphics.Type
import Test.HUnit

vec2Test = TestCase $ assertEqual "Vec2 Add" (Vec2 1 2 + Vec2 3 4) (Vec2 4 6)
vec3Test = TestCase $ assertEqual "Vec3 Add" (Vec3 1 2 3 + Vec3 4 5 6) (Vec3 5 7 9)
vec4Test = TestCase $ assertEqual "Vec4 Add" (Vec4 1 2 3 4 + Vec4 5 6 7 8) (Vec4 6 8 10 12)

typeTest :: IO Counts
typeTest = runTestTT $ TestList [vec2Test, vec3Test, vec4Test]