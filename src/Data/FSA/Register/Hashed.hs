module Data.FSA.Register.Hashed(
    HashedRegister -- | A simple register which uses a strict `HashMap` to aid minimization proocess
  , empty          -- | Creates an empty register for use with `mkFST`
  , replaceOrRegister -- | Use this as compiling function for for `mkFST`
  , automaton       -- | Can be used to extract the constructed automaton as a `HashMap`
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