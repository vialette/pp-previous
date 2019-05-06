module Main where

  import qualified Data.Foldable      as F
  import qualified Data.List          as L
  import qualified System.Environment as Environment
  import qualified System.IO          as IO

  import Data.Algorithm.PP.Perm              as PP.Perm
  import Data.Algorithm.PP.Perm.Features     as PP.Perm.Features
  import Data.Algorithm.PP.Perm.Organization as PP.Perm.Organization
  import Data.Algorithm.PP.Perm.Generator    as PP.Perm.Generator

  stringToInt :: String -> Int
  stringToInt s = read s :: Int

  collect :: PP.Perm.Perm -> String
  collect p = show p        ++ ";"  ++
              (show $ xo)   ++ ";"  ++
              xPalindromic  ++ ";"  ++
              (show $ yo)   ++ ";"  ++
              yPalindromic  ++ ";"  ++
              xyPalindromic ++ ";"  ++
              (show $ xon)  ++ ";"  ++
              (show $ yon)  ++ ";"  ++
              org           ++ ";"  ++
              orgn          ++ "\n"
    where
      xo = PP.Perm.Organization.xOrganization p
      yo = PP.Perm.Organization.yOrganization p

      xPalindromic
        | xo == L.reverse xo = "Yes"
        | otherwise          = "No"

      yPalindromic
        | yo == L.reverse yo = "Yes"
        | otherwise          = "No"

      xyPalindromic
        | xo == L.reverse xo && yo == L.reverse yo = "Yes"
        | otherwise                                = "No"

      xon = PP.Perm.Organization.xOrganizationNumber p
      yon = PP.Perm.Organization.yOrganizationNumber p

      org
        | xo == yo  = "strongly equally organized"
        | otherwise = "non-strongly equally organized"

      orgn
        | xon == yon = "equally organized"
        | xon < yon  = "preferably x-organized"
        | otherwise  = "preferably y-organized"

  names :: String
  names = "permutation"           ++ ";" ++
          "x-organization"        ++ ";" ++
          "x-palindromic"         ++ ";" ++
          "y-organization"        ++ ";" ++
          "y-palindromic"         ++ ";" ++
          "xy-palindromic"        ++ ";" ++
          "x-organization number" ++ ";" ++
          "y-organization number" ++ ";" ++
          "strong organization"   ++ ";" ++
          "organization"          ++ "\n"


  main :: IO ()
  main = do
    args <- Environment.getArgs
    let n = stringToInt( L.head args)
    let res = [collect p| p <- PP.Perm.Generator.perms n]
    IO.putStr names
    mapM_ (IO.putStr) res
