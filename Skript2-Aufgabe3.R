## Wissenschafliches Arbeiten WS2022/23 - Gruppe 11
## Funktionen-R-Skript 2 fuer Aufgabe 3
## Erstellung der Funktionen zur Auswertung des Datensatzes:

# Mode: Erstelle Funktion die den Modus berechnet (fuer Aufgabe 3a in Skript1-A3)
#
# Eingabe: v (numerischer Vektor)
#
# Ausgabe: Modus

Mode <- function(x){
  stopifnot(is.numeric(x))
  as.numeric(names(sort(-table(x)))[1])
}
# Die Funktion findet Anwendung in Skript 1 Aufgabe 3 und ist zur Berechnung
# des Modus eines Vektors.


# kategorie: um eine visuelle Grafik der Variablen zu erstellen die eine Wertung haben.
# haben wir die Funktion die in Aufg. 3e) geschrieben wurde als eine Helferfunktion in Aufg. 3f)
# implementiert.

kategorie_x <- function(x){
  
  
  # die Quantile von x berechnen:
  quant <- quantile(x, probs = seq(0, 1, length.out = 4))
  
  
  # definiere die Kategorie und sie zurückgeben:
  return(cut(x, breaks = quant, labels = c("niedrig", "mittel", "hoch"), 
             include.lowest = TRUE))
}

# Die Funktion findet Anwendung in Skript 1 Aufgabe 3 und wird folglich auch in Aufgabe 4
# verwendet.