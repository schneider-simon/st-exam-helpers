module Helpers where

import Data.List
import Test.QuickCheck
import System.Random

{------------------------------------------------------------------------------
                                General

-------------------------------------------------------------------------------}

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

infix 1 -->

(-->) :: Bool -> Bool -> Bool
p --> q = not p || q

{------------------------------------------------------------------------------
                    Compare conditions (weaker / stronger)

-------------------------------------------------------------------------------}

stronger, weaker :: [a] ->
       (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p

compar :: [a] -> (a -> Bool) -> (a -> Bool) -> String
compar xs p q = let pq = stronger xs p q
                    qp = stronger xs q p
                in
                  if pq && qp then "equivalent"
                  else if pq  then "stronger"
                  else if qp  then "weaker"
                  else             "incomparable"
