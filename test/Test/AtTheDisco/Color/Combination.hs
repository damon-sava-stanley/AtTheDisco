{-# LANGUAGE DataKinds #-}

module Test.AtTheDisco.Color.Combination where

import           AtTheDisco.Color.Combination (AlphaOver (atop),
                                               Premultiplied (Premultiplied),
                                               blendColors, multiplyColors,
                                               premultiply, screenColors)
import           Data.Functor.Identity        (Identity (Identity))
import           Graphics.Color.Model         (Color, RGB, addAlpha)
import           Graphics.Color.Space         (Linearity (NonLinear),
                                               RedGreenBlue (unColorRGB), SRGB,
                                               SRGB)
import           Graphics.Color.Standard      (SVG (SVG), StandardColor (color))
import           Test.HUnit                   (Test, (~:), (~=?), (~?))

combinationTests :: [Test]
combinationTests =
  [ blend0and1Midway
  , blend0LeavesRight
  , blend1LeavesLeft
  , multiplyColorsAB
  , multiplyColors0B
  , multiplyColors1B
  , screenColorsAB
  , screenColors0B
  , screenColors1B
  , premultiply0
  , premultiply1
  , atop0
  , atop1
  ]

-- TODO: could easily be a property based test
-- Properties:
-- + blendColors x (`elem` [0..1]) black white == Identity x
-- + blendColors 0 a b == b
-- + blendColors 1 a b == a
blend0and1Midway :: Test
blend0and1Midway =
  let black = Identity 0
      white = Identity 1
   in "50 % blend" ~: blendColors 0.5 black white ~=? Identity 0.5

blend0LeavesRight =
  let a = Identity 0.3
      b = Identity 0.7
   in "0% blend" ~: blendColors 0 a b ~=? b

blend1LeavesLeft =
  let a = Identity 0.3
      b = Identity 0.7
   in "100% blend" ~: blendColors 1 a b ~=? a

-- Properties
-- + multiplyColors 0 b = 0
-- + multiplyColors 1 b = b
multiplyColorsAB =
  let a = 0.3
      b = 0.7
   in "multiplyTest" ~: multiplyColors (Identity a) (Identity b) ~=?
      Identity (a * b)

multiplyColors0B =
  "0% multiply" ~: multiplyColors (Identity 0) (Identity 0.7) ~=? Identity 0

multiplyColors1B =
  "100% multiply" ~: multiplyColors (Identity 1) (Identity 0.7) ~=? Identity 0.7

screenColorsAB =
  let a = 0.4
      b = 0.7
   in "screen test" ~: screenColors (Identity a) (Identity b) ~=?
      Identity (1 - ((1 - a) * (1 - b)))

screenColors0B = "0% screen" ~: screenColors (Identity 0) (Identity 0.7) ~=? Identity 0.7

screenColors1B = "100% screen" ~: screenColors (Identity 1) (Identity 0.7) ~=? Identity 1

-- Properties
-- (unPremultiply . premultiply) == id
-- (premultiply . unPremultiply) == id
-- premultiply a == a iff alpha a == 1
-- premultiply a == 0 iff alpha a == 0
white = color (SVG :: SVG "white") :: Color (SRGB 'NonLinear) Double

black = color (SVG :: SVG "black") :: Color (SRGB 'NonLinear) Double

premultiply1 =
  "100% premultiply" ~: premultiply (addAlpha white 1) ~=? Premultiplied (addAlpha white 1)

premultiply0 =
  "0% premultiply" ~: premultiply (addAlpha white 0) ~=? Premultiplied (addAlpha black 0)

-- Properties
-- atop a b == a iff alpha a == 1
-- atop a b == b iff alpha a == 0
atop1 = "100% atop" ~: atop (addAlpha white 1) (addAlpha black 1) ~=? addAlpha white 1

atop0 = "0% atop" ~: atop (addAlpha white 0) (addAlpha black 1) ~=? addAlpha black 1
