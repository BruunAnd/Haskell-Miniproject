-- Author: Anders Langballe Jakobsen <alja15@student.aau.dk>, stud. no. 20154059
import Data.List (sort)
import Data.Set (toList, fromList)

-- Helper function to insert an element into a sorted list
-- I believe 
sortedInsert :: Ord a => a -> [a] -> [a]
sortedInsert elem [] = [elem]
sortedInsert elem (x:xs) = if elem <= x
                           then elem:x:xs
                           else x : sortedInsert elem xs

-- Given a char and a string, compute how often the char appears in the string
charFreq :: Char -> String -> Int
charFreq char str = length (filter (==char) str)

-- To count character frequency, I count how frequently each unique character appears
-- A simple approach is to convert the string to a set, s.t. there are no duplicates, and then converting it back
uniqueList :: Ord a => [a] -> [a]
uniqueList = toList . fromList

-- Create an association list from characters to their frequencies
buildFreqList :: String -> FrequencyList Char
buildFreqList str = let uniques = uniqueList str
                    in
                      map (\char -> (char, charFreq char str)) uniques

-- Type synonym for encoding map and frequency lists
type FrequencyList a = [(a, Int)]
type EncodingMap a = [(a, Bits)]

-- Data type for bits and type synonym for a list of bits
data Bit = Zero | One deriving (Show, Eq)
type Bits = [Bit]

-- Data type definition for the (Huffman binary tree)
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
freqToLeaves :: FrequencyList Char -> [BTree]
freqToLeaves freqs = map (\(a, b) -> Leaf a b) freqs

-- Construct a Huffman binary tree given a list of char-frequency pairs
buildTree :: FrequencyList Char -> BTree
buildTree list = buildTree' (sort (freqToLeaves list))
                 where
                    buildTree' [x] = x
                    buildTree' (x:y:r) = buildTree' (sortedInsert (mergeTree x y) r)

-- Recursively construct encoding map given a BTree
buildMap :: BTree -> EncodingMap Char
buildMap tree = buildMap' tree []
                where
                    -- From a branch, we will explore both the left and right branch
                    buildMap' (Branch l r _) list = buildMap' l (Zero:list) ++ buildMap' r (One:list)
                    -- When we encounter a leaf, we return the reverse of the path needed to get to the associated char
                    buildMap' (Leaf char _) list = [(char, reverse list)]

-- Encode a character using a mapping
-- Using Prelude's lookup here, which returns a Maybe type
-- Nothing should never be returned unless there is a bug in the encoder
encodeChar :: Char -> EncodingMap Char -> Bits
encodeChar char mapping = let result = lookup char mapping
                          in
                            case result of
                              Just value -> value
                              Nothing -> error "Error during lookup"

-- Encode a string given a mapping
encode' :: String -> EncodingMap Char -> Bits
encode' str mapping = concatMap (\char -> encodeChar char mapping) str

-- Encode a string and return its tree + the encoded string
encode :: String -> Maybe (BTree, Bits)
encode str = case str of 
                "" -> Nothing
                _  -> let tree = buildTree (buildFreqList str)
                      in
                        Just (tree, encode' str (buildMap tree))

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
