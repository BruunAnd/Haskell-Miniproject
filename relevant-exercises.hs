-- Exercise 1

test = [(1,False),(2,True),(3,False),(4,True)]

keyEq :: (Eq a) => (a, b) -> (a, b) -> Bool
keyEq a b = fst a == fst b

none :: Foldable t => (a -> Bool) -> t a -> Bool
none func list = not (any func list)

-- Check if the association list is valid
valid :: (Eq a) => [(a,b)] -> Bool
valid [] = True
valid (pair:rest) = none (keyEq pair) rest && valid rest

-- Lookup a key from the association list
lookup' :: (Eq a) => a -> [(a, b)] -> Maybe b
lookup' key aList = let
                      matches = filter (\other -> (fst other) == key) aList
                    in
                      if null matches then Nothing else Just (snd (head matches))

-- Make the function associated with an association list
findFun :: (Eq a) => [(a, b)] -> (a -> Maybe b)
findFun aList = (\key -> (lookup' key aList))

-- Exercise 2
data LTree = Leaf String | LBranch LTree LTree
type RLTree = Double -> LTree -> LTree

l = RLTree 1.0 (Leaf "Hej")
