import Data.List (sort)
import Debug.Trace

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
freqList str = [(char, freq) | char <- ['A'..'z']++[' '], let freq = charFreq char str, freq > 0]

-- Data type for bits and type synonym for a list of bits
data Bit = Zero | One deriving Show
type Bits = [Bit]

-- Define equality on the bit datatype
instance Eq Bit where
    Zero == Zero = True
    Zero == One = False
    One == One = True
    One == Zero = False

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
                    buildTree' (x:y:r) = buildTree' (sortedInsert (mergeTree x y) r)

-- Recursively construct encoding map given a BTree
buildMap :: BTree -> [(Char, Bits)]
buildMap tree = buildMap' tree []
                where
                    -- From a branch, we will explore both the left and right branch
                    buildMap' (Branch l r _) list = buildMap' l (Zero:list) ++ buildMap' r (One:list)
                    -- When we encounter a leaf, we return the reverse of the path needed to get to the associated char
                    buildMap' (Leaf char _) list = [(char, reverse list)]

-- Composite function to build a tree from a string
mapFromString :: String -> [(Char, Bits)]
mapFromString = buildMap . buildTree . freqList

-- Encode a character using a mapping
-- Using Prelude's lookup here, which returns a Maybe type
-- Nothing should never be returned unless there is a bug in the encoder
encodeChar :: Char -> [(Char, Bits)] -> Bits
encodeChar char mapping = let result = lookup char mapping in
                              case result of
                                  Just value -> value
                                  Nothing -> error "Error during lookup"

-- Encode a string given a mapping
encode' :: String -> [(Char, Bits)] -> Bits
encode' str mapping = concatMap (\char -> encodeChar char mapping) str

-- Decode a string given a Huffman tree and bits
-- The approach here is to traverse the tree until we encounter a leaf
-- When we encounter a leaf, we return the char of that leaf and then start from the root again
decode :: BTree -> Bits -> String
decode tree bits = decode' tree bits
                   where
                       -- In the case there are no more bits to consume, consume the last leaf
                       decode' (Leaf char _) [] = [char]
                       -- Unlike when choosing a bit, we do not consume a bit when we consume a leaf
                       decode' (Leaf char _) all = char : decode' tree all
                       -- When we encounter a branch, we consume a bit and follow the path
                       decode' (Branch l r _) (bit:rest) = if bit == Zero then decode' l rest else decode' r rest

strengen = "test"
test = freqList strengen
tree = buildTree test
emap = buildMap(tree)
encoded = encode' strengen emap
decoded = decode tree encoded