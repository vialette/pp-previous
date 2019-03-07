
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

import qualified Data.Algorithm.PP.Perm as PP.Perm

main = defaultMain unitTests

unitTests =
  testGroup
    "Unit tests"
    [increasing, decreasing]

increasing =
  testCase "Increasing" $ assertEqual [] [1..9] (PP.Perm.getList $ PP.Perm.mkPerm [1..9])

decreasing =
  testCase "Decreasing" $ assertEqual [] [9,8..1] (PP.Perm.getList $ PP.Perm.mkPerm [9,8..1])
