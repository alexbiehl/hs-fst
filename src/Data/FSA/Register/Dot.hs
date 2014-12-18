module FSA.Register.Dot where

data Dotted a = Dotted {
    baseCompiler :: Compiler a
  , base         :: a
  }

mkDotted :: Compiler a -> a -> Dotted a
mkDotted compiler initial = Dotted compiler base

showDot :: Dotted a -> String
showDot = undefined

replaceOrRegister :: Compiler (Dotted a)
replaceOrRegister arcs dotted =
  dotted {
    base = baseCompiler arcs (base dotted)
  }