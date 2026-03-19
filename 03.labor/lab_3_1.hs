import Language.Haskell.TH (Pat(ListP))
type Pont =(Double, Double)
lsP ::[Pont]
lsP = [(4.5, 2.7),(6,7),(2.1, 7.9)]

p :: Pont
p = (5.7,2.3)

tavolsag (x1,y1) (x2,y2)=sqrt((x1-x2)**2 + (y1-y2)**2)

minTavolsag lsP p = foldl aux lsP
    where
        aux p1 p2 = if tavolsag p1 p < tavolsag p2 p then p1 else p2

minTavolsag2 [p1] _ = p1
minTavolsag2 (p1 : p2 : lsP) p
    |tavolsag p1 p < tavolsag p2 p = minTavolsag2(p1 : lsP) p
    |otherwise = minTavolsag2(p2 : lsP) p
