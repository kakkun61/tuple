-- <length>

type instance Cons i0 <tail> = <tuple>
type instance Head <tuple> = i0
type instance Tail <tuple> = <tail>
type instance Init <tuple> = <init>
type instance Last <tuple> = <last>
type instance Length <tuple> = <length>

instance HasHead' <tuple> i0 where
  head' <tuple-head> = i0

instance HasTail' <tuple> <tail> where
  tail' <tuple-tail> = <tail>

instance HasInit' <tuple> <init> where
  init' <tuple-init> = <init>

instance HasLast' <tuple> <last> where
  last' <tuple-last> = <last>

instance HasCons' <tuple> i0 <tail> where
  cons' i0 <tail> = <tuple>

instance HasUncons' <tuple> i0 <tail> where
  uncons' <tuple> = (i0, <tail>)

instance HasHead <tuple>

instance HasTail <tuple>

instance HasInit <tuple>

instance HasLast <tuple>

instance HasCons i0 <tail>

instance HasUncons <tuple>

instance HasLength <tuple>

{-# COMPLETE Cons' :: <cons> #-}
{-# COMPLETE Cons :: <cons> #-}

type instance Reverse <tuple> = <reverse>

instance HasReverse' <tuple> <reverse> where
  reverse' <tuple> = <reverse>

instance HasReverse <tuple>

---- has-at
