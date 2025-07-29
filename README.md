This package introduces a set of type based arguments that can take in as many parameters as you wish:  

    mk4lst :: a -> a -> a -> a -> \[a\]
    mk4lst = list @4 -- \a b c d -> [a,b,c,d]

This uses the list function which allows you to build a function that takes an arbitrary ammount of elements and turns them into a list.

liftAn is a generalization of liftA2, which allows you to 
    import Data.Bifunctor (first,second)
    up,down,left,right :: (Int,Int) -> (Int,Int)
    up = second succ
    down = second pred
    left = first pred
    right = first succ
    
    allmoves :: (Int,Int) -> [(Int,Int)]
    allmoves = liftAn @4 mk4lst up down left right

mapN allows you to map inside n layers of functors: example

    -- we need to go up then move:
    upThenMove = mapN @2 up allmoves
constN allows you to skip an arbitrary amount of variables
    chain3 :: Parser a -> Parser b -> Parser c -> Parser ()
    chain3 = liftA3 (constN @3 ())
