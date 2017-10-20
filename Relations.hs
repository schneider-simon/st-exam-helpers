module Relations where

import Data.List

type Rel a = [(a,a)]

inverse :: Rel a -> Rel a
inverse = map (\ (x,y) -> (y,x))

symmetricClosure :: Ord a => Rel a -> Rel a
symmetricClosure r = sort ( nub (r ++ inverse r) )

-- Relation composition
infixr 5 @@
(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

fp :: Eq a => (a -> a) -> a -> a
fp f = until (\ x -> x == f x) f

transitiveClosure :: Ord a => Rel a -> Rel a
transitiveClosure r = fp (\ s -> (sort.nub) (s ++ (s @@ s))) r

isSymmetric :: Eq a => Rel a -> Bool
isSymmetric r = containedIn (inverse r) r

isTransitive :: Eq a => Rel a -> Bool
isTransitive r = containedIn (r @@ r) r

containedIn :: Eq a => [a] -> [a] -> Bool
containedIn xs ys = all (\ x -> x `elem` ys) xs

-- TODO: Other properties of relations
