merge :: Ord a => LeftistTree a -> LeftistTree a -> LeftistTree a
merge a b = case (a, b) of
  (Empty,            y)                -> y
  (x,                Empty)            -> x
  (Node x' xr lx rx, Node y' yr ly ry)
    | x' < y'   -> mkNode x' lx $ merge rx b
    | otherwise -> mkNode y' ly $ merge ry a
