import List
import Set

type Mat = { m:Int, n:Int, v:[[Float]] }

maybeApply : (a -> b) -> Maybe a -> Maybe b
maybeApply f ma =
  case ma of
    Just a -> Just (f a)
    Nothing -> Nothing

maybeApply2 : (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
maybeApply2 f ma mb =
  case (ma, mb) of
    (Just a, Just b) -> Just (f a b)
    _ -> Nothing

mat: [[Float]] -> Maybe Mat
mat l =
  let m = List.length l
      ns = Set.toList (Set.fromList (map List.length l))
      maybeN = if List.length ns == 1 then Just (List.head ns) else Nothing
  in maybeApply (\n -> { m=m, n=n, v=l} ) maybeN

prodSum : [Float] -> [Float] -> Float
prodSum a b = sum (zipWith (*) a b)

transpose: Mat -> Mat
transpose a =
  let txpRaw : [[Float]] -> [[Float]]
      txpRaw a = case a of
        [] -> []
        []::xss -> txpRaw xss
        (x::xs)::xss -> (x :: (map head xss)) :: txpRaw (xs :: (map tail xss))
      l = txpRaw a.v
  in { m=a.n, n=a.m, v=l }

mult: Mat -> Mat -> Maybe Mat
mult a b =
  if | a.n == b.m ->
      let b' = (transpose b).v
          prodSums : [[Float]] -> [Float] -> [Float]
          prodSums m v = map (prodSum v) m
          res = map (prodSums b') a.v
      in Just { m=a.m, n=b.n, v=res }
     | otherwise -> Nothing

disp: Mat -> Element
disp m =
  let cell x = container 40 20 middle (asText x)
      row = map cell
      rows = map row m.v
      rowEles = map (flow right) rows
  in flow down rowEles

dispM: Maybe Mat -> Element
dispM m' =
  case m' of
    Just m -> disp m
    Nothing -> plainText "error"

x = mat [[1,0,0], [0,1,0], [0, 0, 1]]
y = maybeApply transpose x
z = case (x, y) of
  (Just x', Just y') -> mult x' y'
  _ -> Nothing
main = dispM z


