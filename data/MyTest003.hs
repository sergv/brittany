mkNode :: a -> LeftistTree a -> LeftistTree a -> LeftistTree a
mkNode x left right
  | rl < rr   = Node x (rl + 1) right left
  | otherwise = Node x (rr + 1) left right
  where
    rl = rank left
    rr = rank right


