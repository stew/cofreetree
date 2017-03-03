import Data.Fix

data TreeF a t = TreeF Int a (Maybe t) (Maybe t)

type Tree a = Fix (TreeF a)

data TreeLocF a f = TreeLocF (Tree a) (Maybe (Either f f))

type TreeLoc a = Fix (TreeLocF a)

leaf a = Fix $ Leaf a
node a l r = Fix $ Node h a l r
  where
    h = max (height l) (height r)

extract :: (Tree a) -> a
extract (Fix (TreeF _ a _ _)) = a

height :: Maybe (Tree a) -> Int
height (Just (Fix (TreeF h _ _ _))) = h
height _ = 0

rootLoc :: Tree a -> TreeLoc a
rootLoc t = Fix $ TreeLocF t Nothing

data Rot = RotL | RotX | RotR

-- given an allowance, determine what kind of rotation is needed
isBalanced :: Int -> Int -> Int -> Rot
isBalanced allow lh rh | (lh - rh) > allow = RotR
                       | (rh - lh) > allow = RotL
                       | otherwise = RotX

rotate :: rot -> Tree a -> Tree a
rotate RotX t = t
rotate RotR (Fix (TreeF _ a Nothing Nothing)) = t
rotate RotR (Fix (TreeF _ a (Just (Fix TreeF lv ll lr)) r)) =
  case isBalanced 0 (height ll) (height lr) of
    RotL -> case lr of
      (Just (Fix (TreeF lrv lrl lrr))) -> node lrv (node lv l lrl) (node a lrr right)
      Nothing -> 
    otherwise -> node lv ll (node a lr r)




balance :: Tree a -> Tree a

