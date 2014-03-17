import Prelude as P

import Data.Map as M
import Data.PQueue.Prio.Min as Q

import Data.List
import Data.Tuple
import Data.Maybe

main :: IO ()
main =  do
        text <- getLine
        let pmf = P.map swap . M.toList $ pmfs text

        let chars = P.map fst . M.toList $ countChars text
        let emptyCode = M.fromList . zip chars $ cycle [""]

        let code = huffCode (Q.fromList pmf) emptyCode

        let encode = huffEncode code
        let decode = huffDecode code

        print $ entropy $ P.map fst pmf
        print code
        print . encode $ text
        print . decode . encode $ text
        print $ (decode . encode) text == text§

entropy :: [Double] -> Double
entropy ps = (-1) * sum (P.map (\p -> p * logBase 2 p) ps)

huffEncode :: Map Char String -> String -> String
huffEncode m s = concatMap (\c -> m ! c) s

huffDecode :: Map Char String -> String -> String
huffDecode m s = huffDecode' s mDecode []
    where mDecode = M.fromList (P.map swap (M.toList m))

huffDecode' :: String -> Map String Char -> String -> String
huffDecode' [] m a = a
huffDecode' s m a = huffDecode' (P.drop (length key) s) m (a ++ [m ! key])
    where key = fromJust . find (`isPrefixOf` s) $ M.keys m

huffCode :: MinPQueue Double String -> Map Char String -> Map Char String
huffCode pq cd
    | Q.size pq == 1 = cd
    | otherwise = huffCode newPQ newCD
        where p = pop2 pq
              newPQ = uncurry updateQ p
              newCD = M.unionsWith (++)  [ls, rs, cd]
                where ls = addCodeChar '0' (snd  ((fst p) !! 0)) M.empty
                      rs = addCodeChar '1' (snd  ((fst p) !! 1)) M.empty

countChars :: String -> Map Char Integer
countChars s = fromListWith (+) [(c, 1) | c <- s]

pmfs :: String -> Map String Double
pmfs s = M.map (\c -> (fromIntegral c) / total) counts
    where total = fromIntegral $ length s
          counts = M.fromList $ P.map (\c -> ([fst c], snd c)) (M.toList (countChars s))

pop2 :: (Ord a) => MinPQueue a b -> ([(a, b)], MinPQueue a b)
pop2 pq = (iterate popNext ([], pq)) !! 2

updateQ :: [(Double, String)] -> MinPQueue Double String -> MinPQueue Double String
updateQ l pq = Q.insert (fst tuple) (snd tuple) pq
    where catTup (v1, s1) (v2, s2) = (v1 + v2, s1 ++ s2)
          tuple = foldl1 catTup l

-- foldl?
addCodeChar :: Char -> String -> Map Char String -> Map Char String
addCodeChar c [] m = m
addCodeChar c s m = addCodeChar c (tail s) $ M.insertWith (++) (head s) [c] m

popNext :: (Ord a) => ([(a, b)], MinPQueue a b) -> ([(a, b)], MinPQueue a b)
popNext p = (vs ++ newV, newQ)
    where vs = fst p
          q = snd p
          newQ = Q.deleteMin q
          newV = [fromJust (Q.getMin q)]
