module Data.FSA.Register.Hashed(
    HashedRegister
  , empty
  , replaceOrRegister
  ) where

import           Data.FSA.Types

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

data HashedRegister = HashedRegister {
    regArcs    :: !(HashMap [Arc] StateRef)
  , regNextRef :: !StateRef
  } deriving (Eq, Show)

empty :: HashedRegister
empty = HashedRegister HashMap.empty 1

automaton :: HashedRegister -> HashMap [Arc] StateRef
automaton = regArcs

replaceOrRegister :: Compiler HashedRegister
replaceOrRegister arcs (HashedRegister arcMap nextRef) =
  case HashMap.lookup arcs arcMap of
    Just stateRef -> (stateRef, HashedRegister arcMap nextRef)
    Nothing       ->
      let
        register' = HashedRegister {
                      regArcs    = HashMap.insert arcs nextRef arcMap
                    , regNextRef = nextRef + 1
                    }
      in (nextRef, register')