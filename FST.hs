
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
import           Data.Char (ord)

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
test1'Uncompiled = uncompiledBS test1

test1 :: ByteString
test1 = "alex"

test2'Uncompiled :: [UncompiledState]
test2'Uncompiled = uncompiledBS test2

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
          [] -> (arcs, register)
          -- ^ We are really at the end of the word
          _  -> let (a, r) = go register sx in (a:arcs, r)
          -- ^ Traverse till the end
        (ref, register'') = replaceOrRegister arcs' register'
      in (Arc byte ref, register'')

uncompiled :: [Word8] -> [UncompiledState]
uncompiled []     = []
uncompiled (w:wx) = (UncompiledState w arcs):(uncompiled wx)
  where
    arcs = if List.null wx then [finalArc] else []

uncompiledBS :: ByteString -> [UncompiledState]
uncompiledBS = uncompiled . ByteString.unpack

compileList :: [ByteString] -> StateRef
compileList bs = go bs [] emptyRegister
  where
    go (b:bx) path register =
      let
        decompiled = uncompiledBS b
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

finalArc :: Arc
finalArc = Arc 0 finalStateRef

main :: IO ()
main = do
  let compile      = compileSuffix replaceOrRegister
      ord'         = fromIntegral . ord
      r0           = emptyRegister
      (exArc, r1)  = compile r0 (uncompiledBS "ex")
      (ienArc, r2) = compile r1 (uncompiledBS "ien")
      (alArc, r3)  = compile r2 [UncompiledState (ord' 'a') [], UncompiledState (ord' 'l') [ienArc, exArc]]

  putStrLn (show exArc)
  putStrLn (show r1)

  putStrLn (show ienArc)
  putStrLn (show r2)

  putStrLn (show alArc)
  putStrLn (show r3)

