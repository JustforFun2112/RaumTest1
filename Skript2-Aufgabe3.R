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