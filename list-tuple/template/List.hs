-- <length>

type instance Cons a <tail> = <tuple>
type instance Head <tuple> = a
type instance Tail <tuple> = <tail>
type instance Init <tuple> = <init>
type instance Last <tuple> = <last>
type instance Length <tuple> = <length>

instance HasHead' <tuple> a where
  head' <tuple-head> = a

instance HasTail' <tuple> <tail> where
  tail' <tuple-tail> = <tail>

instance HasInit' <tuple> <init> where
  init' <tuple-init> = <init>

instance HasLast' <tuple> <last> where
  last' <tuple-last> = <last>

instance HasCons' <tuple> a <tail> where
  cons' a <tail> = <tuple>

instance HasUncons' <tuple> a <tail> where
  uncons' <tuple> = (a, <tail>)

instance HasHead <tuple>

instance HasTail <tuple>

instance HasInit <tuple>

instance HasLast <tuple>

instance HasCons a <tail>

instance HasUncons <tuple>

instance HasLength <tuple>

{-# COMPLETE Cons' :: <cons> #-}
{-# COMPLETE Cons :: <cons> #-}

type instance Reverse <tuple> = <reverse>

instance HasReverse' <tuple> <reverse> where
  reverse' <tuple> = <reverse>

instance HasReverse <tuple>

---- has-at
