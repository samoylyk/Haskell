module ECKarte where

import Listen

{- Übung 1b: Prüfziffern-Algorithmus von Kreditkarten
             Implementieren Sie alle Funktionen direkt selbst,
             *ohne* Verwendung der Standard-Bibliothek.
             Eigene Funktionen aus der LV bzw. aus dem
             Modul Listen.hs können Sie verwenden.
            
             !Vergessen Sie nicht regelmäßig einzuchecken!
             
             Gesamtpunktezahl: 15
-}

-- Gibt die Ziffern einer Zahl als Liste zurück
-- Beispiel: ziffern 45612 == [4,5,6,1,2]
-- Punkte: 3
ziffern :: Int -> [Int]
ziffern 0 = []
ziffern n = g n []
            where g 0 acc = acc
                  g n acc = g (div n 10) (mod n 10 : acc)
{- Tipp: verwenden Sie eine Rekursion und die Funktionen "div" (Division) und "mod" (Modulo, Rest),
         um die Berechnung durchzuführen. Auch ein Akkumulator kann helfen. -}

-- Gibt die Ziffern einer Zahl in umgekehrter Reihenfolge als Liste zurück
-- Beispiel: ziffern 45612 == [2,1,6,5,4]
-- Punkte: 1
ziffernRev :: Int -> [Int]
ziffernRev 0 = []
ziffernRev n = umkehren (ziffern n)
{- Mit der umkehren-Funktion aus dem Listen-Modul sollte das einfach sein. -}


-- Vergessen Sie nicht regelmäßig einzuchecken

-- Verdoppelt jede zweite Zahl in der Liste, beginnend mit der zweiten
-- Beispiel: verdopple [3,1,2,3,4,5] == [3,2,2,6,4,10]
-- Punkte: 2
verdopple :: [Int] -> [Int]
verdopple = undefined
{- Tipp: Der Cons-Operator kann auch mehrfach zur Dekonstruktion oder Kombination
         verwendet werden, z.B. (x:y:xs) oder (x:y:z:xs) -}

-- Bilde die Quersumme aller Ziffern einer Liste von Zahlen
-- Beispiel: querSumme [12,3,5,22] == 1+2+3+5+2+2 == 15
-- Punkte: 3
querSumme :: [Int] -> Int
querSumme = undefined
{- Tipp: Verwenden Sie eine Hilfsfunktion, die mithilfe der ziffern-Funktion die Zahlen
         aufzusplittet und die Teile summiert. Diese Ergebnisse ergeben in Summe die querSumme. -} 

-- Vergessen Sie nicht regelmäßig einzuchecken


-- Überprüfe, ob eine Zahl nach dem Luhn-Algorithmus korrekt ist
-- Beispiel: valide 5672 == False; valide 5678 == True
-- Punkte: 3
valide :: Int -> Bool
valide = undefined
{- Tipp: verwenden Sie alle vorhergehenden Funktione. ziffernRev ist hilfreich. -}

-- Errechne die notwendige Prüfziffer für eine Zahl (die Prüfziffer wird am Ende angehängt)
-- Beispiel: prüfziffer 876 == 3 (weil: valide 8763 == True)
-- Punkte: 3
prüfziffer :: Int -> Int
prüfziffer = undefined
