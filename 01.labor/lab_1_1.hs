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

max_ :: Int -> Int-> String
max_ a b 
    |   a>b  =a
    |   a<b  =b
    |   otherwise = "egyenlo"