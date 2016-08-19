-- -------------------------------------------------------------- [ Errmsg.idr ]
-- Module      : Lightyear.Errmsg
-- Description : Error message formatting.
--
-- This code is distributed under the BSD 2-clause license.
-- See the file LICENSE in the root directory for its full text.
-- --------------------------------------------------------------------- [ EOH ]

||| Error message formatting.
module Lightyear.Errmsg

import Lightyear.Core

%access export
-- ------------------------------------------------------------------- [ Begin ]

||| TODO: write docstring
interface Layout str where
  ||| TODO: write docstring
  lineLengths : str -> List Int

private
nat2int : Nat -> Int
nat2int  Z    = 0
nat2int (S x) = 1 + nat2int x

private
rowcol : List Int -> List Int -> (Int, Int)
rowcol       ws        []  = (0,0)  -- should not happen
rowcol (w :: ws) (x :: []) = (1 + (nat2int $ length ws), 1 + w-x)
rowcol (w :: ws) (x :: xs) = rowcol ws xs
rowcol       []        xs  = (0,0)  -- should not happen

private
formatItem : Layout str => str -> (str, String) -> String
formatItem whole (rest, msg) =
  let (row, col) = rowcol (reverse $ lineLengths whole)
                          (reverse $ lineLengths rest)
  in  "at " ++ show row ++ ":" ++ show col ++ " expected:\n  " ++ msg

||| Given a `whole` stream and a `stacktrace`,
||| pretty print the errors, returning a string.
||| @whole      A whole stream (string).
||| @stacktrace A stack trace of errors.
formatError : Layout str => (whole      : str)
                         -> (stacktrace : List (str, String))
                         -> String
formatError whole = unlines . map (formatItem whole)
-- --------------------------------------------------------------------- [ EOF ]
