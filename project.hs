-- Author: Anders Langballe Jakobsen <alja15@student.aau.dk>, stud. no. 20154059
import Data.List (sort)
import Data.Set (toList, fromList)

-- Helper function to insert an element into a sorted list
sortedInsert :: Ord a => a -> [a] -> [a]
sortedInsert elem [] = [elem]
sortedInsert elem (x:xs) = if elem <= x
                           then elem:x:xs
                           else x : sortedInsert elem xs

-- Given a char and a string, compute how often the char appears in the string
elemFreq :: Ord a => a -> [a] -> Int
elemFreq elem str = length (filter (==elem) str)

-- Get unique elements in a list (conversion from list -> set -> list)
uniqueList :: Ord a => [a] -> [a]
uniqueList = toList . fromList

-- Create an association list from characters to their frequencies
buildFreqList :: Ord a =>[a] -> FrequencyList a
buildFreqList lst = let uniques = uniqueList lst
                    in
                      map (\elem -> (elem, elemFreq elem lst)) uniques

-- Type synonym for encoding map and frequency lists
type FrequencyList a = [(a, Int)]
type EncodingMap a = [(a, Bits)]

-- Data type for bits and type synonym for a list of bits
data Bit = Zero | One deriving (Show, Eq)
type Bits = [Bit]

-- Data type definition for the (Huffman binary tree)
data BTree a = Leaf a Int | Branch (BTree a) (BTree a) Int deriving Show

-- Define equality on the BTree type. Two nodes are considered equal if they have the same frequency
instance Eq (BTree a) where
    x == y = treeFreq x == treeFreq y

-- Define ordering on the BTree type. The ordering is based on the frequency of the node
instance Ord (BTree a) where
    compare x y = compare (treeFreq x) (treeFreq y)

-- Given a BTree, treeFreq returns the frequency associated with the root node of the tree
treeFreq :: BTree a -> Int
treeFreq (Leaf e f) = f
treeFreq (Branch l r f) = f

-- BTrees can be merged such that, given two trees, the resulting tree branches to its two children and uses the sum of its frequencies as its frequency
mergeTree :: BTree a -> BTree a -> BTree a 
mergeTree l r = Branch l r ((treeFreq l) + (treeFreq r))

-- Maps an association list of char frequencies to corresponding leaves
freqToLeaves :: FrequencyList a -> [BTree a]
freqToLeaves freqs = map (\(a, b) -> Leaf a b) freqs

-- Construct a Huffman binary tree given a list of char-frequency pairs
buildTree :: Ord a => FrequencyList a -> BTree a
buildTree list = buildTree' (sort (freqToLeaves list))
                 where
                    buildTree' [x] = x
                    buildTree' (x:y:r) = buildTree' (sortedInsert (mergeTree x y) r)

-- Recursively construct encoding map given a BTree
buildMap :: Ord a => BTree a -> EncodingMap a
buildMap tree = buildMap' tree []
                where
                    -- From a branch, we will explore both the left and right branch
                    buildMap' (Branch l r _) list = buildMap' l (Zero:list) ++ buildMap' r (One:list)
                    -- When we encounter a leaf, we return the reverse of the path needed to get to the associated char
                    buildMap' (Leaf char _) list = [(char, reverse list)]

-- Encode a character using an encodingmap
encodeElement :: Ord a => a -> EncodingMap a -> Bits
encodeElement elem mapping = let result = lookup elem mapping
                             in
                                 case result of
                                   Just value -> value
                                   Nothing -> error "Error during lookup"

-- Encode a string given an encoding map
-- ConcatMap is used because each element is mapped to a list of bits
encode' :: Ord a => [a] -> EncodingMap a -> Bits
encode' lst mapping = concatMap (\elem -> encodeElement elem mapping) lst

-- Encode a list of a totally ordered type and return an encoding and its Huffmann tree
-- Instead of throwing an error in case of empty lists, I use the Maybe type
encode :: Ord a => [a] -> Maybe (BTree a, Bits)
encode lst = case lst of 
                [] -> Nothing
                _  -> let tree = buildTree (buildFreqList lst)
                      in
                        Just (tree, encode' lst (buildMap tree))

-- Decode a string given a Huffman tree and bits
decode :: Ord a => BTree a -> Bits -> [a]
decode tree bits = decode' tree bits
                   where
                       -- In the case there are no more bits to consume, consume the last leaf
                       decode' (Leaf elem _) [] = [elem]
                       -- Unlike when choosing a bit, we do not consume a bit when we consume a leaf
                       decode' (Leaf elem _) all = elem : decode' tree all
                       -- When we encounter a branch, we consume a bit and follow the path
                       decode' (Branch l r _) (bit:rest) = if bit == Zero then decode' l rest else decode' r rest

main = do
    putStrLn "Enter a string to decode: "
    toEncode <- getLine
    let encoded = encode toEncode
    case encoded of
        Nothing -> putStrLn "Nothing was returned by the encoder, did you input an empty string?"
        Just value -> let tree = fst value
                          bits = snd value
                      in
                        do
                          putStrLn ("Huffmann tree: " ++ show tree)
                          putStrLn ("Bit encoding: " ++ show bits)
                          putStrLn ("Decoded: " ++ (decode tree bits))
