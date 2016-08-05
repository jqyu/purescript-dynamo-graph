module Dynamo.Graph.Iterator where

import Prelude (($), (+), (-), (<=), (<<<))
import Control.Alt (class Alt, (<|>))
import Control.Alternative (class Alternative, (<*>))
import Control.Apply (class Apply)
import Control.Applicative (class Applicative, pure)
import Control.Bind (class Bind, (>>=))
import Control.Monad (class Monad, ap)
import Control.MonadPlus (class MonadZero, class MonadPlus)
import Control.Parallel.Class (par)
import Control.Plus (class Plus)
import Data.Array (length, take, filter, toUnfoldable, fromFoldable, reverse) as Array
import Data.Foldable (fold)
import Data.Functor (class Functor, map, (<$>), (<#>))
import Data.List (List(Nil), (:))
import Data.Monoid (class Monoid, mempty)
import Data.Ord (class Ord, compare)
import Data.Ordering (Ordering(..))
import Data.Semigroup (class Semigroup, append, (<>))
import JS.Control.Promise.Env (PromiseEnv)

-- | We define a batching, asynchronous iterator to handle graph cursors
-- | The downside here is that we can have a non-terminating thunk chain:
-- |   fuck = Next { vals: [], next: fuck }
-- | However in practical use, we greatly reduce the number of promises
-- | and function applications

data Iterator e env a = Done | Next (IteratorNext e env a)

type IteratorNext e env a =
  { vals :: Array a
  , meta :: PromiseEnv e env Meta
  , next :: IteratorCont e env a
  }

type IteratorCont e env a = PromiseEnv e env (Iterator e env a)


-- | We define a `Meta` type which allows introspection into the state of the iterator

data Meta = Meta { count :: Int }
          | Indeterminate -- n.b. atm i'm too dumb to figure a lot of these out

getMeta :: forall e env a. Iterator e env a -> PromiseEnv e env Meta
getMeta Done = pure mempty
getMeta (Next { meta }) = meta

instance semigroupIteratorMeta :: Semigroup Meta where
  append Indeterminate _ = Indeterminate
  append _ Indeterminate = Indeterminate
  append (Meta x) (Meta y) =
    Meta { count: x.count + y.count
         }

instance monoidIteratorMeta :: Monoid Meta where
  mempty = Meta { count: 0 }

-- | We define a `take` method which evaluates the first n elements of an iterator

take :: forall e env a. Int -> Iterator e env a -> PromiseEnv e env (Array a)
take n Done = pure []
take n (Next { vals, next }) =
    if n <= len then pure (Array.take n vals)
                else next >>= take (n - len)
                          <#> (vals <> _)
  where len = Array.length vals
-- in haskell we would write:
-- take n (Next { vals, next })
--     | n <= len  = pure $ Array.take n vals
--     | otherwise = next >>= take (n - len)
--                        <#> (vals <> _)
--   where len = Array.length vals

-- | We define a monoid instance which distributes over `take`
-- |   i.e. take n (a <> b) = take (count a) a <> take (n - count a) a
-- |        adjusted so that the monads work out on the RHS

instance semigroupIterator :: Semigroup (Iterator e env a) where
  append Done y = y
  append x Done = x
  append (Next { vals, meta, next }) y =
    Next { vals
         , meta: par append meta (getMeta y)
         , next: next <#> (_ <> y)
         }

instance monoidIterator :: Monoid (Iterator e env a) where
  mempty = Done

-- | Since an iterator is just a monad + array, it has all the expected instances

instance functorIterator :: Functor (Iterator e env) where
  map f Done = Done
  map f (Next { vals, meta, next }) =
    Next { vals: map f vals
         , meta: meta
         , next: map f <$> next
         }

instance applyIterator :: Apply (Iterator e env) where
  apply = ap

instance altIterator :: Alt (Iterator e env) where
  alt Done y = y
  alt x Done = x
  alt x@(Next { vals: [], meta, next }) y = Next
      { vals: []
      , meta: deferred >>= getMeta
      , next: deferred
      }
    where deferred = next <#> (_ <|> y)
  alt x _ = x

instance plusIterator :: Plus (Iterator e env) where
  empty = Done

instance alternativeIterator :: Alternative (Iterator e env)

instance applicativeIterator :: Applicative (Iterator e env) where
  pure a = Next { vals: [a]
                , meta: pure $ Meta { count: 1 }
                , next: pure Done
                }

instance bindIterator :: Bind (Iterator e env) where
  bind Done _ = Done
  bind (Next { vals, meta, next }) k = Next
      { vals: []
      , meta: getMeta batch
      , next: pure (batch <> deferred)
      } -- we could do batch <> bind next f, but that would eagerly load the next batch
    where batch = fold (map k vals)
          deferred = Next
            { vals: []
            , meta: next >>= getMeta
            , next: next <#> (_ >>= k)
            }

instance monadIterator :: Monad (Iterator e env)

instance monadPlusIterator :: MonadPlus (Iterator e env)
instance monadZeroIterator :: MonadZero (Iterator e env)

-- | Iterators are asynchronous, lazy arrays, so naturally we should be able to filter them

filter :: forall e env a. (a -> Boolean) -> Iterator e env a -> Iterator e env a
filter f Done = Done
filter f (Next { vals, next }) = Next
  { vals: Array.filter f vals
  , meta: pure Indeterminate -- TODO: see if there's a way to determine this
  , next: filter f <$> next
  }

-- | If we have sorted iterators, it becomes very easy to join multiple iterators
-- | in a sensible way.

type Accumulator a =
  { acc :: List a
  , lhs :: List a
  , rhs :: List a
  }

type Accumulated e env a =
  { vals :: Array a
  , lcont :: IteratorCont e env a
  , rcont :: IteratorCont e env a
  }

accumulateIterators :: forall e env a. (Accumulator a -> Accumulator a)
                    -> IteratorNext e env a
                    -> IteratorNext e env a
                    -> Accumulated e env a
accumulateIterators merge leftIt rightIt =
    { vals: Array.reverse $ Array.fromFoldable merged.acc
    , lcont: makeCont leftIt merged.lhs
    , rcont: makeCont rightIt merged.rhs
    }

  where merged =
          merge
            { acc: Nil
            , lhs: Array.toUnfoldable leftIt.vals
            , rhs: Array.toUnfoldable rightIt.vals
            }

        makeCont iterator Nil =
          iterator.next
        makeCont iterator xs =
          pure $ Next
            { vals: Array.fromFoldable xs
            , meta: pure Indeterminate
            , next: iterator.next
            }

-- | The following methods will join two iterators that are assumed to be in descending order
-- | based on the given comparator

union :: forall e env a
       . (a -> a -> Ordering)
      -> Iterator e env a
      -> Iterator e env a
      -> Iterator e env a
union _ Done y = y
union _ x Done = x
union cmp (Next left) (Next right) =
  let merge acc@{ lhs, rhs: Nil } = acc
      merge acc@{ lhs: Nil, rhs } = acc
      merge { acc, lhs: lhs@(l:lhs'), rhs: rhs@(r:rhs') } =
        case cmp l r of
             GT -> merge { acc: (l:acc), lhs: lhs', rhs }
             EQ -> merge { acc: (l:acc), lhs: lhs', rhs: rhs' }
             LT -> merge { acc: (r:acc), lhs, rhs: rhs' }

      merged = accumulateIterators merge left right

   in Next { vals: merged.vals
           , meta: pure Indeterminate
           , next: union cmp <$> merged.lcont <*> merged.rcont
           }

intersection :: forall e env a
              . (a -> a -> Ordering)
             -> Iterator e env a
             -> Iterator e env a
             -> Iterator e env a
intersection _ Done y = Done
intersection _ x Done = Done
intersection cmp (Next left) (Next right) =
  let merge acc@{ lhs, rhs: Nil } = acc
      merge acc@{ lhs: Nil, rhs } = acc
      merge { acc, lhs: lhs@(l:lhs'), rhs: rhs@(r:rhs') } =
        case cmp l r of
             GT -> merge { acc, lhs: lhs', rhs }
             EQ -> merge { acc: (l:acc), lhs: lhs', rhs: rhs' }
             LT -> merge { acc, lhs, rhs: rhs' }

      merged = accumulateIterators merge left right

   in Next { vals: merged.vals
           , meta: pure Indeterminate
           , next: intersection cmp <$> merged.lcont <*> merged.rcont
           }

unionDesc :: forall e env a. (Ord a)
          => Iterator e env a
          -> Iterator e env a
          -> Iterator e env a
unionDesc = union compare

unionAsc :: forall e env a. (Ord a)
         => Iterator e env a
         -> Iterator e env a
         -> Iterator e env a
unionAsc = union (flipOrdering compare)


intersectionDesc :: forall e env a. (Ord a)
                 => Iterator e env a
                 -> Iterator e env a
                 -> Iterator e env a
intersectionDesc = intersection compare

intersectionAsc :: forall e env a. (Ord a)
                => Iterator e env a
                -> Iterator e env a
                -> Iterator e env a
intersectionAsc = intersection (flipOrdering compare)

flipOrdering :: forall a. (a -> a -> Ordering) -> (a -> a -> Ordering)
flipOrdering cmp x y =
  case cmp x y of
       LT -> GT
       EQ -> EQ
       GT -> LT

