-- Author: Anders Langballe Jakobsen <alja15@student.aau.dk>, stud. no. 20154059
import Data.List (sort, insert, nub)
import Text.Read (readMaybe)

-- Create an association list from symbols to their frequencies
buildFreqList :: Eq a => [a] -> [(a, Int)]
buildFreqList list = map (\elem -> (elem, elemFreq elem)) $ nub list
                     where
                      elemFreq elem = length (filter (==elem) list)

-- Data type and type synonym definitions
data BTree a = Leaf a Int | Branch (BTree a) (BTree a) Int deriving Show
data Bit = Zero | One deriving (Eq, Read, Show)
type Bits = [Bit]

instance Eq (BTree a) where
  x == y = treeFreq x == treeFreq y

instance Ord (BTree a) where
  compare x y = compare (treeFreq x) (treeFreq y)

treeFreq :: BTree a -> Int
treeFreq (Leaf e f) = f
treeFreq (Branch l r f) = f

mergeTree :: BTree a -> BTree a -> BTree a 
mergeTree l r = Branch l r ((treeFreq l) + (treeFreq r))

-- Construct a Huffman binary tree given a list of symbol-frequency pairs
buildTree :: Eq a => [(a, Int)] -> BTree a
buildTree list = buildTree' . sort . leaves $ list
                 where
                   leaves freqs = map (\(a, b) -> Leaf a b) freqs
                   buildTree' [x] = x
                   buildTree' (x:y:r) = buildTree' $ insert (mergeTree x y) r

-- Construct encoding map by depth-first exploration
buildMap :: Eq a => BTree a -> [(a, Bits)]
buildMap tree = buildMap' tree []
                where
                  buildMap' (Branch l r _) list = buildMap' r (One:list) ++ buildMap' l (Zero:list)
                  -- When we encounter a leaf, we return the reverse of the path needed to get to the associated symbol
                  buildMap' (Leaf symbol _) list = [(symbol, reverse list)]

-- Encode a symbol using an encodingmap
encodeElement :: Eq a => a -> [(a, Bits)] -> Bits
encodeElement elem mapping = let
                               result = lookup elem mapping
                             in
                               case result of
                                 Just value -> value
                                 Nothing -> error "Error during lookup"

-- Encode a list of a totally ordered type and return an encoding and its Huffmann tree
encode :: Eq a => [a] -> Maybe (BTree a, Bits)
encode [] = Nothing
encode lst = Just (tree, encode' $ buildMap tree)
             where
              tree = buildTree (buildFreqList lst)
              encode' mapping = concatMap (\elem -> encodeElement elem mapping) lst

-- Decode a string given a Huffman tree and bits
-- If the root of the tree is a leaf and there are no bits, simply replicate the symbol at the leaf freq 
decode :: Eq a => BTree a -> Bits -> [a]
decode (Leaf symbol freq) [] = replicate freq symbol
decode tree bits = decode' tree bits
                   where
                     -- In the case there are no more bits to consume, consume the last leaf
                     decode' (Leaf elem _) [] = [elem]
                     -- Unlike when choosing a bit, we do not consume a bit when we consume a leaf
                     decode' (Leaf elem _) all = elem : decode' tree all
                     -- When we encounter a branch, we consume a bit and follow the path
                     decode' (Branch l r _) (bit:rest) = if bit == Zero then decode' l rest else decode' r rest

writeBitsToFile :: Bits -> FilePath -> IO ()
writeBitsToFile bits file = writeFile file (show bits)

readBitsFromFile :: FilePath -> IO (Maybe Bits)
readBitsFromFile filePath = do
  contents <- readFile filePath
  let constructed = readMaybe contents :: Maybe Bits
  return (constructed)

file_io_main = do
  contents <- readFile "input.txt"
  let encoded = encode contents
  case encoded of
    Nothing -> putStrLn "Nothing was returned by the encoder, is input.txt empty?"
    Just value -> let
                    tree = fst value
                    bits = snd value
                  in
                    do
                      writeBitsToFile bits "output.txt"
                      inBits <- readBitsFromFile "output.txt"
                      let decoded = decode tree bits
                      putStrLn ("Decoded from file: " ++ decoded)

io_main = do
  putStrLn "Enter string to encode and decode: "
  toEncode <- getLine
  let encoded = encode toEncode
  case encoded of
    Nothing -> putStrLn "Nothing was returned by the encoder, did you input an empty string?"
    Just value -> let 
                    tree = fst value
                    bits = snd value
                  in
                    do
                      putStrLn ("Encoded: " ++ show bits)
                      putStrLn ("Decoded: " ++ (decode tree bits))
