type instance <tuple> !! <at> = <item>

instance HasAt' <tuple> <at> <item> where
  <tuple-at> !!! _ = <item>

instance HasAt <tuple> <at>
