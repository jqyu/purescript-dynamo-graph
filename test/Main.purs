module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Array ((..))
import Data.Array as Array

import Test.Unit
import Test.Unit.Main
import Test.Unit.Assert as Assert

import JS.Control.Promise.Env (PromiseEnv(..))
import JS.Control.Promise.Env (run) as PromiseEnv
import Dynamo.Graph.Iterator (Iterator(..), Meta(..), take, filter, unionAsc, intersectionAsc)

main = runTest do

  suite "iterators" do

    let divisibleBy b x = x `mod` b == 0
        nats =
          let gen n s =
                Next
                  { vals: n .. (n + s - 1)
                  , meta: pure Indeterminate
                  , next: PromiseEnv \_ -> pure $ gen (n + s) s
                  }
           in gen 0 10
        mult2 = (2 * _) <$> nats
        mult3 = filter (divisibleBy 3) nats

    test "takes work" do
      hunna <- PromiseEnv.run unit $ take 101 nats
      Assert.equal (0..100) hunna

    test "unions work" do
      actual <- PromiseEnv.run unit
              $ take 100
              $ unionAsc mult2 mult3
      let expected = Array.take 100
                   $ Array.filter ( \x -> divisibleBy 3 x
                                       || divisibleBy 2 x)
                   $ 0..300
      Assert.equal expected actual

    test "intersections work" do
      actual <- PromiseEnv.run unit
              $ take 100
              $ intersectionAsc mult2 mult3
      let expected = Array.take 100
                   $ Array.filter (\x -> divisibleBy 3 x
                                      && divisibleBy 2 x)
                   $ 0..600
      Assert.equal expected actual
