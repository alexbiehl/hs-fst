
{-# LANGUAGE OverloadedStrings #-}

import           Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import           Data.Word (Word8, Word64)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Hashable
import qualified Data.List as List
import           Data.Monoid
import           Data.Function (on)

type StateRef = Word64

data State = State [Arc]

data Arc = Arc {
   arcByte :: !Word8
 , arcTo   :: !StateRef
} deriving (Eq, Show)

instance Hashable Arc where
  hashWithSalt s (Arc b t) = hashWithSalt (hashWithSalt s b) t

data Register = Register {
    regArcs    :: !(HashMap [Arc] StateRef)
  , regNextRef :: !StateRef
} deriving (Eq, Show)

data UncompiledState = UncompiledState {
    ucByte :: !Word8
  , ucArcs :: ![Arc]
  } deriving (Eq, Show)

newtype UncompiledStateByte = UncompiledStateByte {
    toUncompiledState :: UncompiledState
  }

instance Eq UncompiledStateByte where
  (==) = (==) `on` (ucByte . toUncompiledState)

test1'Uncompiled :: [UncompiledState]
test1'Uncompiled = map (\b -> UncompiledState b []) (ByteString.unpack test1)

test1 :: ByteString
test1 = "alex"

test2'Uncompiled :: [UncompiledState]
test2'Uncompiled = map (\b -> UncompiledState b []) (ByteString.unpack test2)

test2 :: ByteString
test2 = "alien"


{-
["a", "l", "e", "x"]
["a", "l", "i", "e", "n"]
["s", "e", "e"]

activePath = map (\b -> UncompiledState b []) term

Sobald ein Suffix kompiliert ist, wird dem letzten unkompilierten Knoten der letzte resultierende Arc hinzugefügt


Kompilierung der Suffixe
------------------------

Gegeben sei eine Funktion `replaceOrRegister :: [Arcs] -> Register -> (StateRef, Register)`,
welche überprüft, ob eine Knotenmenge schon im Register vorhanden ist, d.h. kompiliert wurde
oder diese ansonsten in das Register einfügt.

ReplaceOrRegister wird das Compilerbackend, bis jetzt nur Dummy

-}

type ReplaceOrRegister = [Arc] -> Register -> (StateRef, Register)

emptyRegister = Register mempty 1

replaceOrRegister :: [Arc] -> Register -> (StateRef, Register)
replaceOrRegister arcs (Register arcMap nextRef) =
  case HashMap.lookup arcs arcMap of
    Just stateRef -> (stateRef, Register arcMap nextRef)
    Nothing       ->
      let
        register' = Register {
                      regArcs    = HashMap.insert arcs nextRef arcMap
                    , regNextRef = nextRef + 1
                    }
      in (nextRef, register')

compileSuffix :: ReplaceOrRegister -> Register -> [UncompiledState] -> (Arc, Register)
compileSuffix replaceOrRegister register sx = go register sx
  where
    go register ((UncompiledState byte arcs):sx) =
      let
        (arcs', register') = case sx of
          -- ^ We are really at the end of the word
          [] | List.null arcs -> ((Arc byte finalStateRef):arcs, register)
          -- ^ Nope, this is just the prefix of an already merged word
          --   (this works since the words need to be inserted in sorted order)
             | otherwise      -> (arcs, register)
          -- ^ Traverse till the end
          _                   -> let (a, r) = go register sx in (a:arcs, r)
        (ref, register'') = replaceOrRegister arcs' register'
      in (Arc byte ref, register'')

decompile :: ByteString -> [UncompiledState]
decompile = fmap (\b -> UncompiledState b []) . ByteString.unpack

compileList :: [ByteString] -> StateRef
compileList bs = go bs [] emptyRegister
  where
    go (b:bx) path register =
      let
        decompiled = decompile b
        prefix = commonPrefix
                  (map UncompiledStateByte path)
                  (map UncompiledStateByte decompiled)
        suffix = map toUncompiledState $
                  stripPrefix prefix (map UncompiledStateByte path)
      in undefined

-- | Find the common prefix
commonPrefix :: (Eq e) => [e] -> [e] -> [e]
commonPrefix _ [] = []
commonPrefix [] _ = []
commonPrefix (x:xs) (y:ys)
  | x == y    = x : commonPrefix xs ys
  | otherwise = []

stripPrefix p xs = let Just rest = List.stripPrefix p xs in rest

{-
Der resultierende `Arc` wird dem vorhergehenden `UncompiledState` hinzugefügt




1. Gibt es schon einen kompilierten Zustand, mit der Abbildung 'x -> final'
  -> Wenn ja, verwende dessen StateRef
  -> Nein, dann lege diesen im Register an (Kompiliere ihn -> StateRef)

2. Gibt es schon einen kompilierten Zustand, mit der Abbildung 'e -> x'
  -> Wenn ja, verwende dessen StateRef
  -> Nein, dann lege diesen im Register an

3. Gibt es schon einen kompilierten Zustand, mit der Abbildung 'n -> final'
  -> Wenn ja, verwende dessen StateRef
  -> Nein, dann lege diesen im Register an

4. Gibt es schon einen kompilierten Zustand, mit der Abbildung 'i -> n'
  -> Wenn ja, verwende dessen StateRef
  -> Nein, dann lege diesem im Register an

5. ... 'e -> i'

6. ... 'l -> e'

7. ... 'l -> l'

8. ... 'a -> l'

-}

errorStateRef :: StateRef
errorStateRef = 1

finalStateRef :: StateRef
finalStateRef = 0




main :: IO ()
main = putStrLn "Hallo welt"
