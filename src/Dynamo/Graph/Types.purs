module Dynamo.Graph.Types where

import JS.Control.Promise.Env (PromiseEnv)

foreign import data DYNAMO_READ  :: !
foreign import data DYNAMO_WRITE :: !

foreign import data Graph :: *

type DynamoGraph e env a =
  PromiseEnv
    ( dynamo_read :: DYNAMO_READ, dynamo_write :: DYNAMO_WRITE | e )
    { g :: Graph | env }
    a

type DynamoGraphRead e env a =
  PromiseEnv
    ( dynamo_read :: DYNAMO_READ | e )
    { g :: Graph | env }
    a
