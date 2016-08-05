module Dynamo.Graph where

import Dynamo.Graph.Types (Graph)

import JS.Control.Promise (Promise)
import JS.Control.Promise.Env (PromiseEnv(..))

foreign import data DYNAMO_CREATE_TABLE :: !

foreign import define :: String -> Graph
foreign import _generate :: Graph -> Promise Graph

generate :: forall e env. PromiseEnv ( dynamo_create_table :: DYNAMO_CREATE_TABLE | e ) { g :: Graph | env } Graph
generate = PromiseEnv \{ g } -> _generate g
