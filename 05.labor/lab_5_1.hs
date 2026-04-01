import Distribution.Simple.Setup (trueArg)
import Control.Monad.Trans.Cont (reset)
-- - Írjunk egy Haskell-függvényt, amely egy String típusú listából meghatározza azokat a szavakat, amelyek karakterszáma a legkisebb. Például ha a lista a következő szavakat tartalmazza:  function class Float higher-order monad tuple variable Maybe recursion  akkor az eredmény-lista a következőkből áll: class Float monad tuple Maybe
-- - Írjunk egy talalat Haskell-függvényt, amely meghatározza azt a listát, amely a bemeneti listában megkeresi egy megadott elem előfordulási pozícióit.
--   Például a következő függvényhívások esetében az első az 5-ös előfordulási pozícióit, míg a második az e előfordulási pozícióinak listáját határozza meg.

--   ```haskell
--   > talalat 5 [3, 13, 5, 6, 7, 12, 5, 8, 5]
--   [2, 6, 8]
--   > talalat 'e' "Bigeri-vizeses"
--   [3,10,12]
--   ```
-- - Írjunk egy osszegT Haskell-függvényt, amely meghatározza egy (String, Int)értékpárokból álló lista esetében az értékpárok második elemeiből képzett összeget.
--   Például:

--   ```haskell
--   > ls = [("golya",120),("fecske",85),("cinege",132)]
--   > osszegT ls
--   337
--   ```
-- - Írjunk egy atlagTu Haskell-függvényt, amely egy kételemű, tuple elemtípusú lista esetében átlagértékeket számol a második elem szerepét betöltő listaelemeken. Az eredmény egy tuple elemtípusú lista legyen, amelynek kiíratása során a tuple-elemeket formázzuk, és külön sorba írjuk őket.
--   Például:

--   ```haskell
--   > :set +m
--   > ls = [("mari",[10, 6, 5.5, 8]), ("feri",[8.5, 9.5]),
--   | ("zsuzsa",[4.5, 7.9, 10]),("levi", [8.5, 9.5, 10, 7.5])]
--   > atlagTu ls
--   mari 7.375
--   feri 9.0
--   zsuzsa 7.466666666666666
--   levi 8.875
--   ```
--II. Írjunk Haskell-függvényt, amely a foldl vagy a foldr függvényt alkalmazva

-- - implementálja a length, sum, elem, reverse, product, maximum, insert-sort, ++, map, filter függvényeket,
-- - meghatározza egy lista pozitív elemeinek összegét,
-- - egy lista páros elemeinek szorzatát,
-- - n-ig a négyzetszámokat.
-- - meghatározza a $$P(x) = a_0 + a_1 x + a_2 x^2 + \ldots + a_n x^n$$ polinom adott $x_0$ értékre való behelyettesítési értékét: $$a_0 + x_0(a_1 + x_0(a_2 + x_0(a_3 + \ldots + x_0(a_{n-1}+ x_0 \cdot a_n))))$$

myLengthL ls = foldl op 0 ls
    where
        op k res = res + 1

myLengthR ls = foldr op 0 ls
    where
        op k res = res + 1

mySumL ls = foldl op 0 ls
    where
        op res k = res + k

mySumR ls = foldr op 0 ls
    where 
        op k res = res + k

myElemL c ls = foldl(op c) False ls
    where
        op c res k 
            | c == k = True
            | otherwise = res

myElemR c ls = foldr(op c) False ls
    where
        op c k res = (c==k) || res


myReverseL ls = foldl op [] ls
    where 
        op res k = k : res

myReverseR ls = foldr op [] ls
    where
        --op k res = res ++ [k]
        op k res = res <>[k]

myProductL ls = foldl op 1 ls
    where
        op res k = res * k

myProductR ls = foldl op 1 ls
    where
        op k res = res * k

myMaximumL ls = foldl op (head ls) ls
    where
        op res k
            | res > k = res
            | otherwise = k

myMaximumL1 ls = foldl1 op ls
    where
        op res k
            | res > k = res
            | otherwise = k

myIns x ls = foldr (op x) [] ls
    where 
        op x k res  
            | x > k = k : res
            | otherwise = x : k : res





 