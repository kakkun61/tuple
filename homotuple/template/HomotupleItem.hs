-- <length>

type instance Homotuple <length> a = <homotuple>

instance IsList <homotuple> where
  type Item <homotuple> = a
  fromList <list> = <tuple>
  fromList _ = errorLengthMismatch
  toList <tuple> = <list>
