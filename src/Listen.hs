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
längeS :: String -> Int
längeS = undefined

-- Liefert den längesten String aus einer Liste von Strings zurück
-- Beispiel: längsterString ["ein", "zwei", "dreizehn", "zwölf"] == "dreizehn"
-- Punkte: 3
längsterString :: [String] -> String
längsterString = undefined
{- Tipp: Bei dieser Funktion ist eine Hilfsvariable bzw. ein Akkumulator nützlich.
         Merken Sie sich den längsten String (und optional dessen Länge) und
         vergleichen Sie den mit dem aktuellen String.
         Werfen Sie einen Blick auf das sumPosNeg-Beispiel, um eine Idee zu erhalten.
-}

-- Liefert die ersten n Zeichen zurück. Ist die Liste kürzer als die angegebene
-- Zeichenzahl, dann sollen einfach so viele Zeichen wie möglich retourniert werden.
-- Beispiel: nimm 3 "ABCDEF" == "ABC"; nimm 3 "AB" == "AB"; nimm 3 [] == []
-- Punkte: 2
nimm :: Int -> [Char] -> [Char]
nimm n l = undefined


-- Löscht die ersten n Zeichen einer Liste. Ist die Liste kürzer als die angegebene
-- Zeichenzahl, dann sollen einfach die leere Liste retourniert werden.
-- Beispiel: verwirf 3 "ABCDEF" == "DEF"; verwirf 3 "AB" == []; verwirf 3 [] == []
-- Punkte: 2
verwirf :: Int -> [Char] -> [Char]
verwirf n l = undefined


-- Vergessen Sie nicht regelmäßig einzuchecken

-- Teilt eine Liste von Zahlen in der Hälfte und retourniert im
-- Tupel die vordere und die hintere Hälfte
-- Beispiel: halbieren [1,2,3,4,5] == ([1,2,3], [4,5])
-- Punkte: 2
halbieren :: [Int] -> ([Int], [Int])
halbieren = undefined
{- Tipp: Verwenden Sie die Funktionen längeS, nimm und verwirf.
         Verwenden Sie Hilfsvariable. -}

-- (Schwierig) Teilt eine Liste von Zahlen in zwei Teile
-- Ähnlich wie beim Kartenspiel werden die Zahlen *abwechselnd* auf die "Stapel" gelegt
-- Beispiel: zweiteilen [1,2,3,4,5] == ([1,3,5], [2,4]) 
-- Punkte: 3
zweiteilen :: [Int] -> ([Int], [Int])
zweiteilen = undefined

-- Vergessen Sie nicht regelmäßig einzuchecken

-- Kehrt eine Liste um
-- Beispiel: umkehren [1,2,3] == [3,2,1]
-- Punkte: 3
umkehren :: [Int] -> [Int]
umkehren = undefined
{- Tipp: am einfachsten lässt sich die Funktion umsetzen, wenn Sie eine
         Hilfsfunktion mit Akkumulator verwenden. -}


-- Liefert alle lokalen Minima zurück.
-- Ein lokales Minimum ist, wenn die vorhergende und die nachfolgende Zahl größer ist
-- Beispiel: lokalesMinimum [4,1,6,8,7,9,3,4] == [1,7,3]
-- Punkte: 2
lokalesMinimum :: [Int] -> [Int]
lokalesMinimum = undefined
{- Tipp: Der Cons-Operator kann auch mehrfach zur Dekonstruktion oder Kombination
         verwendet werden, z.B. (x:y:xs) oder (x:y:z:xs) -}


-- Vergessen Sie nicht regelmäßig einzuchecken


-- Filtert alle ungeraden Zahlen
-- Beispiel: filterUngerade [1,2,3,7,12] == [1,3,7]
-- Punkte: 2
filterUngerade :: [Int] -> [Int]
filterUngerade = undefined


-- Gibt an, ob die gesuchte Zahl in der Liste enthalten ist
-- Beispiel: enthält 3 [1,2,3,4] == True; enthält 3 [1,2,4] == False
-- Punkte: 2
enthält :: Int -> [Int] -> Bool
enthält = undefined


-- Vergessen Sie nicht regelmäßig einzuchecken

-- Zählt, wie häufig die gesuchte Zahl in der Liste vorkommt
-- Beispiel: zählen 3 [1,3,5,8,3,7] == 2
-- Punkte: 2
zählen :: Int -> [Int] -> Int
zählen = undefined

-- Vergessen Sie nicht regelmäßig einzuchecken

-- (Schwierig) Fügt eine Liste von Listen zu einer Liste zusammen
-- Beispiel: zusammenfügen ["Hallo", " ", "Haskell", "!"] == "Hallo Haskell!" 
-- Punkte: 3
zusammenfügen :: [String] -> String
zusammenfügen = undefined

-- Retourniert alle Suffixe eines Strings
-- Beispiel: suffixe "Haskell" = ["Haskell", "askell", "skell", "kell", "ell", "ll", "l", ""]
-- Punkte: 3
suffixe :: String -> [String]
suffixe = undefined
