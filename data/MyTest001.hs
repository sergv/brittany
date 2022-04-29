deleteMin :: Ord a => LeftistTree a -> LeftistTree a
deleteMin = \case
  Empty        -> Empty
  Node _ _ l r -> merge l r
