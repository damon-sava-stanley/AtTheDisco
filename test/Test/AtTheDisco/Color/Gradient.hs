module Test.AtTheDisco.Color.Gradient
  ( gradientTests
  ) where

import           AtTheDisco.Color.Gradient
import           Data.Functor.Identity     (Identity (Identity))
import           Test.HUnit                (Test, (~:), (~=?), (~?=), (~?))

gradientTests =
  [ newGradientTest
  , insertKeypointTest
  , gradientMidpoint
  , halfGradient
  , gradientBeforeIsFirst
  , gradientAfterIsLast
  ]

newGradientTest =
  "Gradient Construction" ~: Just (newGradient 's' 'e') ~?=
  gradientFromList [('s', 0), ('e', 1)]

insertKeypointTest =
  "Gradient Construction" ~: Just (insertKeypoint 'm' 0.5 $ newGradient 's' 'e') ~?=
  gradientFromList [('s', 0), ('m', 0.5), ('e', 1)]

gradientMidpoint =
  "Gradient Midpoint" ~:
  colorGradientValue 0.7 (newGradient (Identity 0) (Identity 1)) ~?=
  Identity 0.7

gradientBeforeIsFirst =
  "Gradient Before" ~:
  colorGradientValue (-1) (newGradient (Identity 0) (Identity 1)) ~?=
  Identity 0

gradientAfterIsLast =
  "Gradient After" ~:
  colorGradientValue 2 (newGradient (Identity 0) (Identity 1)) ~?=
  Identity 1

halfGradient =
  "Half Gradient Midpoint" ~:
  fmap
    (colorGradientValue 0.25)
    (gradientFromList [(Identity 0, 0), (Identity 1, 0.5)]) ~?=
  Just (Identity 0.5)
