
{-# LANGUAGE OverloadedStrings #-}

import           Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import           Data.Word (Word16, Word64)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Hashable
import qualified Data.List as List
import           Data.Monoid
import           Data.Function (on)
import           Data.Char (chr, ord)

type StateRef = Word64

data State = State [Arc]

data Arc = Arc {
   arcByte :: !Word16
 , arcTo   :: !StateRef
} deriving (Eq, Show)

type RootArcs = [Arc]

instance Hashable Arc where
  hashWithSalt s (Arc b t) = hashWithSalt (hashWithSalt s b) t

data Register = Register {
    regArcs    :: !(HashMap [Arc] StateRef)
  , regNextRef :: !StateRef
} deriving (Eq, Show)

type Compiler a = [Arc] -> a -> (StateRef, a)

type ReplaceOrRegister = Compiler Register

data UncompiledState = UncompiledState {
    ucByte :: !Word16
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

emptyRegister = Register mempty 1

replaceOrRegister :: [Arc]
                  -> Register
                  -> (StateRef, Register)
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

compileSuffix :: ReplaceOrRegister
              -> Register
              -> [UncompiledState]
              -> (Arc, Register)
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

uncompiled :: [Word16]
           -> [UncompiledState]
uncompiled []     = []
uncompiled (w:wx) = (UncompiledState w arcs):(uncompiled wx)
  where
    arcs = if List.null wx then [finalArc] else []

uncompiledBS :: ByteString -> [UncompiledState]
uncompiledBS = uncompiled . map fromIntegral . ByteString.unpack

compile :: ReplaceOrRegister
        -> [UncompiledState]
        -> [UncompiledState]
        -> Register
        -> (RootArcs, [UncompiledState], Register)
compile ror new old register = (rootArcs, path, register')
  where
    dummyRoot                    = UncompiledState 0 []
    (root:path, register')       = compile' ror dummyRoot new old register
    (UncompiledState _ rootArcs) = root

compile' :: ReplaceOrRegister
         -> UncompiledState
         -> [UncompiledState]
         -> [UncompiledState]
         -> Register
         -> ([UncompiledState], Register)
compile' ror prev new@(n:nx) old@(o:ox) register
  | ucByte n == ucByte o =
    let
      (path, register') = compile' ror o nx ox register
    in (prev:path, register')
  | otherwise =
    let
      (arc, register') = compileSuffix ror register old
      path = (prev { ucArcs = arc:(ucArcs prev) }):new
    in (path, register')

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

root :: UncompiledState
root = UncompiledState 0 []

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

