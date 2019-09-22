import Data.List.Split
import Data.List
import System.IO
import Data.Char (digitToInt)

dist (_, p1) (_, p2) = (sqrt (foldl (+) 0.0 (zipWith (\ x y -> (x - y)^2) p1 p2)))

distFromCloud (_, cloud) p = (foldr min h r) where (h:r) = (map (dist p) cloud)

nearestCloudIndex clouds p = (snd (foldr minIndex h r))
  where (h:r) = (zip (map (\ c -> distFromCloud c p) clouds) [0..((length clouds))])
        minIndex = (\ (d1, i1) (d2, i2) -> if d1 < d2 then (d1, i1) else (d2, i2))

updateAtIndex (x:xs) f 0 = (f x) : xs
updateAtIndex (x:xs) f i = x:(updateAtIndex xs f (i - 1))

appendToNearestCloud clouds p = updateAtIndex clouds (\ (n, c)-> (n, p:c)) (nearestCloudIndex clouds p)

appendToCloud [] n p = [(n, [p])]
appendToCloud ((cn,ps):cs) n p =
  if cn == n
     then (cn,p:ps):cs
     else (cn,ps):(appendToCloud cs n p)

removeIndex (_:xs) 0 = xs
removeIndex (x:xs) i = x:(removeIndex xs (i - 1))

distAndIndexToNearestCloud p cs = (foldl minDist h r)
  where (h:r) = (zip  [0..((length cs))] (map (\ c -> (distFromCloud c p)) cs))
        minDist = (\ (xi, xd) (ai, ad) -> if xd < ad then (xi, xd) else (ai, ad))

indexOfPointAndNearestCloud :: [(Int, Float)] -> (Int, Int)
indexOfPointAndNearestCloud dincs = (pi, ci)
  where (h:r) = (zip [0..(length dincs)] dincs)
        nearestPoint = (\ (xi, (xci, xcd)) (ai, (aci, acd)) -> if xcd < acd then (xi, (xci, xcd)) else (ai, (aci, acd)))
        (pi, (ci, _)) = (foldl nearestPoint h r)

appendPointsToClouds :: [([Char], [Float])] -> [(Int, [([Char], [Float])])] -> [(Int, [([Char], [Float])])]
appendPointsToClouds [] cs = cs
appendPointsToClouds ps cs = appendPointsToClouds ps' cs'
  where (pi, ci) = (indexOfPointAndNearestCloud
                    (map (\ p -> distAndIndexToNearestCloud p cs) ps))
        p = ps!!pi
        cs' = updateAtIndex cs (\ (n, c)-> (n, p:c)) ci
        ps' = removeIndex ps pi



main1 :: [([Char], [Float])] -> IO [([Char], [Float])]
main1 ps =
  do a <- getLine
     if null $ a
        then return ps
        else let (s:ss) = splitOn " " a
                 p = (s, (map (\x -> read x :: Float) ss))
                 ps' = p:ps
             in main1 ps'

main2 clouds ps =
  do eof <- isEOF
     if eof
        then return (clouds, ps)
        else
          do a <- getLine
             let (s:g) = (splitOn " ") $ a
                 n = (read (head g) :: Int)
                 p = (head (filter (\ (pn, _) -> s == pn) ps))
                 ps' = (filter (\ (pn, _) -> s /= pn) ps)
                 clouds' = appendToCloud clouds n p
              in main2 clouds' ps'

main = do
  ps <- main1 []
  ret_main2 <- main2 [] ps
  let (cs, ps') = ret_main2
      in putStr $ (intercalate "\n" $ (map (\ c -> show c)) $ appendPointsToClouds ps' cs) ++ "\n"
