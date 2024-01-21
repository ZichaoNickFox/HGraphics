{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module LensTest.LensExerciseTest (lensExerciseTest) where

import Test.HUnit

import Control.Lens
import Data.Text
import Control.Lens.TH

-- https://williamyaoh.com/posts/2019-04-25-lens-exercises.html

data User = User
  { _name     :: Text
  , _userid   :: Int
  , _metadata :: UserInfo
  }
  deriving (Show, Eq)

data UserInfo = UserInfo
  { _numLogins     :: Int
  , _associatedIPs :: [Text]
  }
  deriving (Show, Eq)

makeLenses ''User
makeLenses ''UserInfo

user1 = User
  { _name = "qiao.yifan"
  , _userid = 103
  , _metadata = UserInfo
    { _numLogins = 20
    , _associatedIPs =
      [ "52.39.193.61"
      , "52.39.193.75"
      ]
    }
  }

testList :: Test
testList = TestList [
  TestCase $ assertEqual "I.1" (user1 ^. name) "qiao.yifan",
  TestCase $ assertEqual "I.2" (user1 ^. metadata . numLogins) 20,
  TestCase $ assertEqual "I.3" (user1 & metadata . numLogins .~ 0) 
    (User
      { _name = "qiao.yifan"
      , _userid = 103
      , _metadata = UserInfo
        { _numLogins = 0
        , _associatedIPs =
          [ "52.39.193.61"
          , "52.39.193.75"
          ]
        }
      }
    ),
  TestCase $ assertEqual "I.4" (user1 & metadata . associatedIPs %~ ("192.168.0.2" :)) 
    (User
      { _name = "qiao.yifan"
      , _userid = 103
      , _metadata = UserInfo
        { _numLogins = 20
        , _associatedIPs =
          [ "192.168.0.2"
          , "52.39.193.61"
          , "52.39.193.75"
          ]
        }
      }
    ),
  TestCase $ assertEqual "I.5" (metadata . numLogins %~ (+1) $ user1)
    (User
      { _name = "qiao.yifan"
      , _userid = 103
      , _metadata = UserInfo
        { _numLogins = 21
        , _associatedIPs =
          [ "52.39.193.61"
          , "52.39.193.75"
          ]
        }
      }
    ),
  TestCase $ assertEqual "II.1" (user1 & name .~ "qyifan@xingxin.com")
    (User
      { _name = "qyifan@xingxin.com"
      , _userid = 103
      , _metadata = UserInfo
        { _numLogins = 21
        , _associatedIPs =
          [ "52.39.193.61"
          , "52.39.193.75"
          ]
        }
      }
    ),
  ]

lensExerciseTest :: IO Counts
lensExerciseTest = runTestTT testList