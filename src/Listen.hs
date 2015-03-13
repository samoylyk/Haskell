module Listen where

{- Übung 1a: programmieren Sie alle Funktionen direkt selbst,
             *ohne* Verwendung der Standard-Bibliothek.
             Eigene Funktionen aus der LV können Sie verwenden.
            
             !Vergessen Sie nicht regelmäßig einzuchecken!
             
             Gesamtpunktezahl: 30
-}

-- Liefert die Länge (= Anzahl der Zeichen) eines Strings zurück
-- Beispiel: längeS "Haskell" == 7
-- Punkte: 1
--längeS :: String -> Int
längeS :: [a] -> Int
längeS [] = 0
längeS (x:xs) = 1 + längeS(xs)

-- Liefert den längesten String aus einer Liste von Strings zurück
-- Beispiel: längsterString ["ein", "zwei", "dreizehn", "zwölf"] == "dreizehn"
-- Punkte: 3
längsterString :: [String] -> String
längsterString [] = ""
längsterString (x:xs)
          | längeS x > längeS y = x
          | otherwise = y
          where y = längsterString xs
{- Tipp: Bei dieser Funktion ist eine Hilfsvariable bzw. ein Akkumulator nützlich.
         Merken Sie sich den längsten String (und optional dessen Länge) und
         vergleichen Sie den mit dem aktuellen String.
         Werfen Sie einen Blick auf das sumPosNeg-Beispiel, um eine Idee zu erhalten.
-}

-- Liefert die ersten n Zeichen zurück. Ist die Liste kürzer als die angegebene
-- Zeichenzahl, dann sollen einfach so viele Zeichen wie möglich retourniert werden.
-- Beispiel: nimm 3 "ABCDEF" == "ABC"; nimm 3 "AB" == "AB"; nimm 3 [] == []
-- Punkte: 2
--nimm :: Int -> [Char] -> [Char]
nimm :: Int -> [a] -> [a]
nimm 0 _ = []
nimm _ [] = []
nimm n (x:xs) | n > 0 = x : nimm (n-1) xs


-- Löscht die ersten n Zeichen einer Liste. Ist die Liste kürzer als die angegebene
-- Zeichenzahl, dann sollen einfach die leere Liste retourniert werden.
-- Beispiel: verwirf 3 "ABCDEF" == "DEF"; verwirf 3 "AB" == []; verwirf 3 [] == []
-- Punkte: 2
--verwirf :: Int -> [Char] -> [Char]
verwirf :: Int -> [a] -> [a]
verwirf _ [] = []
verwirf 0 xs = xs
verwirf n (x:xs) | n > 0 = verwirf (n-1) xs


-- Vergessen Sie nicht regelmäßig einzuchecken

-- Teilt eine Liste von Zahlen in der Hälfte und retourniert im
-- Tupel die vordere und die hintere Hälfte
-- Beispiel: halbieren [1,2,3,4,5] == ([1,2,3], [4,5])
-- Punkte: 2
halbieren :: [Int] -> ([Int], [Int])
halbieren [] = ([], [])
halbieren xs = (nimm halb xs, verwirf halb xs)
               where halb = div (längeS xs) 2
{- Tipp: Verwenden Sie die Funktionen längeS, nimm und verwirf.
         Verwenden Sie Hilfsvariable. -}

-- (Schwierig) Teilt eine Liste von Zahlen in zwei Teile
-- Ähnlich wie beim Kartenspiel werden die Zahlen *abwechselnd* auf die "Stapel" gelegt
-- Beispiel: zweiteilen [1,2,3,4,5] == ([1,3,5], [2,4]) 
-- Punkte: 3
zweiteilen :: [Int] -> ([Int], [Int])
zweiteilen [] = ([], [])
zweiteilen [x] = ([x], [])
zweiteilen (x:y:xys) = (x:xs, y:ys)
                       where (xs, ys) = zweiteilen xys

-- Vergessen Sie nicht regelmäßig einzuchecken

-- Kehrt eine Liste um
-- Beispiel: umkehren [1,2,3] == [3,2,1]
-- Punkte: 3
umkehren :: [Int] -> [Int]
umkehren list = umk_acc list []
  where umk_acc [] acc = acc
        umk_acc (x:xs) acc = umk_acc xs (x:acc)
{- Tipp: am einfachsten lässt sich die Funktion umsetzen, wenn Sie eine
         Hilfsfunktion mit Akkumulator verwenden. -}


-- Liefert alle lokalen Minima zurück.
-- Ein lokales Minimum ist, wenn die vorhergende und die nachfolgende Zahl größer ist
-- Beispiel: lokalesMinimum [4,1,6,8,7,9,3,4] == [1,7,3]
-- Punkte: 2
lokalesMinimum :: [Int] -> [Int]
lokalesMinimum [] = []
lokalesMinimum (x:[]) = []
lokalesMinimum (x:y:[]) = []
lokalesMinimum (x:y:z:xs)
          | y < x && y < z = y:lokalesMinimum (z:xs)
          | otherwise = lokalesMinimum (y:z:xs)
{- Tipp: Der Cons-Operator kann auch mehrfach zur Dekonstruktion oder Kombination
         verwendet werden, z.B. (x:y:xs) oder (x:y:z:xs) -}


-- Vergessen Sie nicht regelmäßig einzuchecken


-- Filtert alle ungeraden Zahlen
-- Beispiel: filterUngerade [1,2,3,7,12] == [1,3,7]
-- Punkte: 2
filterUngerade :: [Int] -> [Int]
filterUngerade [] = []
filterUngerade (x:xs)
          | mod x 2 == 1 = x:filterUngerade xs
          | otherwise = filterUngerade xs

-- Gibt an, ob die gesuchte Zahl in der Liste enthalten ist
-- Beispiel: enthält 3 [1,2,3,4] == True; enthält 3 [1,2,4] == False
-- Punkte: 2
enthält :: Int -> [Int] -> Bool
enthält _ [] = False
enthält n (x:xs)
          | n == x = True
          | otherwise = enthält n xs

-- Vergessen Sie nicht regelmäßig einzuchecken

-- Zählt, wie häufig die gesuchte Zahl in der Liste vorkommt
-- Beispiel: zählen 3 [1,3,5,8,3,7] == 2
-- Punkte: 2
zählen :: Int -> [Int] -> Int
zählen n [] = 0
zählen n (x:xs)
          | n == x = 1 + zählen n xs
          | otherwise = zählen n xs

-- Vergessen Sie nicht regelmäßig einzuchecken

-- (Schwierig) Fügt eine Liste von Listen zu einer Liste zusammen
-- Beispiel: zusammenfügen ["Hallo", " ", "Haskell", "!"] == "Hallo Haskell!" 
-- Punkte: 3
zusammenfügen :: [String] -> String
zusammenfügen xss = [x | xs <- xss, x <- xs]

-- Retourniert alle Suffixe eines Strings
-- Beispiel: suffixe "Haskell" = ["Haskell", "askell", "skell", "kell", "ell", "ll", "l", ""]
-- Punkte: 3
suffixe :: String -> [String]
suffixe [] = [""]
suffixe (x:xs) = (x:xs) : suffixe xs

