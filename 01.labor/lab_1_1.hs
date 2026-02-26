osszeg :: Int -> Int -> Int
osszeg a b = a+b
kulonbseg :: Double -> Double -> Double
kulonbseg a b = a-b
szorzat a b =a*b


hanyados a b = a div b

osztmar a b = mod a b

osztmar2 a b = a mod b

--  egy első fokú egyenlet gyökét

elsoF a b = (-b)  /a
abszolut a 
    | a<0 = -a
    | otherwise = a
    
abszolut2 a = if a<0 then -a else a

elojel n = if n<0 then "negativ" else if n>0 then "pozitiv" else "nulla"


elojel1 n
    | n < 0 = "negativ"
    | n > 0 = "pozitiv"
    | otherwise = "nulla"

-- masodfoku egyenlet
-- masodf::(Floating b, Ord b) => b -> b -> b -> (b, b)
-- masodf a b c if delta < 0 then error "komplex szam" else (gy1,gy2)
-- where 
--     delta = b**2 - 4*a*c
--     gy1 = (-b + sqrt delta)/(2*a)
--     gy2=(-b - sqrt delta)/(2*a)


masodF2::(Ord a,Floating a) => a -> a->a->[a]
masodF2 a b c
    |delta<0 = error "komplex szamok"
    |delta==0 = [gy1]
    |otherwise = [gy1, gy2]
    where
        delta = b**2 - 4*a*c
        gy1 = (-b + sqrt delta)/(2*a)
        gy2 =(-b - sqrt delta)/(2*a)

elempar::Eq a =>(a,a) -> (a,a)->Bool
elempar ep1 ep2 = if(a==d && b==c) || (a==c && b==d)then True else False
    where
        (a,b)=ep1
        (c,d)=ep2

fakt2::(Ord t,Num t)=>t -> t
fakt2 n 
    |n<0 = error "neg. szam"
    |n==0 = 1
    |otherwise = n* fakt2(n-1)

hatvany::(Ord a,Floating a)=>a->a->a
hatvany x n
    |n<0 = error"neg. kitevo"
    |otherwise = x ** n

negyzetgyok::(Enum a,Floating a)=>a->[a]
negyzetgyok n = [sqrt i |i<-[1..n]]

negyzetszam::(Num a,Enum a)=>a->[a]
negyzetszam n = [i ^ 2 | i<-[1..n]]

nemNegyzet::(Enum a,Eq a,Floating a)=>a->[a]
nemNegyzet n = [i|i<-[1..n],i /= (sqrt i ** 2)]

parosOsztok x =[i | i<-[1..x],mod x i == 0,mod i 2 ==0]

osztok x = [i | i <- [1..x], mod x i == 0]

primszam x = osztok x == [1, x]

primszamN n = [i | i <- [1..n], primszam i]

primszamN2 n = [i | i <- [1..n], primszamL i]
    where
        primszamL si = osztokL si == [1,si]
        osztokL si2 = [i | i <- [1..si2], mod si2 i == 0]

-- - n-ig az összetett számok listáját,
osszetett n = [i | i <- [1..n], primszam i == False]

osszetett2 n = [i | i <- [1..n], not(primszam i)]

paratlanOsszetett n = [i | i <- [1..n], primszam i == False, mod i 2 /= 0]

paratlanOsszetett2 n = [i | i <- [1..n], not (primszam i), odd i]

pitagorasz n = [(a, b, c) | c <- [1..n], b <- [1..c], a <- [1..b], a ^ 2 + b ^ 2 == c ^ 2]

betuSzam = zip ['a' .. 'z'] [0 .. 25]

betuSzam2 = zip ['a' .. 'z'] [0 .. ]

szamok = zip [0..5][5, 4 ..]

szamok2 n = zip [0..n] [n, n-1 .. 0]

szamok3 n = [(i, n-i) | i <- [0..n]]




main :: IO()
main = do 
    -- putStrLn "Masodfoku egyenlet"
    -- print(masodF2 1 4 1)
    -- putStrLn("masodfoku egyenlet 2:" ++ show (masodF2 4 10 6))
    putStrLn "hatvany"
    print(hatvany 2 3)