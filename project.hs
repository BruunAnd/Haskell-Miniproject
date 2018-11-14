import Data.List (sort)

-- Helper function to insert an element into a sorted list
sortedInsert :: Ord a => a -> [a] -> [a]
sortedInsert elem [] = [elem]
sortedInsert elem (x:xs) = if elem <= x
                           then elem:x:xs
                           else x : sortedInsert elem xs

-- Given a char and a string, compute how often the char appears in the string
charFreq :: Char -> String -> Int
charFreq char str = length (filter (==char) str)

-- Create an association list from characters to their frequencies
-- TODO: Change this
freqList :: String -> [(Char, Int)]
freqList str = [(char, freq) | char <- ['A'..'z'], let freq = charFreq char str, freq > 0]

-- Data type for bits and type synonym for a list of bits
data Bit = Zero | One deriving Show
type Bits = [Bit]

-- Data type definition for the (Huffman binary tree)
-- At leaf nodes, we store chars with their associated frequency
-- The value of a branch is the sum of its descedants' values
data BTree = Leaf Char Int | Branch BTree BTree Int deriving Show

-- Define equality on the BTree type. Two nodes are considered equal if they have the same frequency
instance Eq BTree where
    x == y = treeFreq x == treeFreq y

-- Define ordering on the BTree type. The ordering is based on the frequency of the node
instance Ord BTree where
    compare x y = compare (treeFreq x) (treeFreq y)

-- Given a BTree, treeFreq returns the frequency associated with the root node of the tree
treeFreq :: BTree -> Int
treeFreq (Leaf c f) = f
treeFreq (Branch l r f) = f

-- BTrees can be merged such that, given two trees, the resulting tree branches to its two children and uses the sum of its frequencies as its frequency
mergeTree :: BTree -> BTree -> BTree
mergeTree l r = Branch l r ((treeFreq l) + (treeFreq r))

-- Maps an association list of char frequencies to corresponding leaves
freqToLeaves :: [(Char, Int)] -> [BTree]
freqToLeaves freqs = map (\(a, b) -> Leaf a b) freqs

-- Construct a Huffman binary tree given a list of char-frequency pairs
buildTree :: [(Char, Int)] -> BTree
buildTree list = buildTree' (sort (freqToLeaves list))
                 where
                    buildTree' [x] = x
                    buildTree' (x:y:r) = buildTree'(sortedInsert (mergeTree x y) r)

-- Recursively construct encoding map given a BTree
buildMap :: BTree -> [(Char, Bits)]
buildMap tree = buildMap' tree []
                where
                    buildMap' (Branch l r _) list = (buildMap' l list:Zero) ++ (buildMap' r list:One)
                    buildMap' (Leaf char _) list = [(char, list)]
        

test = freqList "aadasasoijdasoasodasojidoijaodjadas"
tree = buildTree test