# Funktionen-R-Skript 1 fuer Aufgabe 3

# a)

my_afunktion <- function(x){
  a <- summary(x) 
  
  b <- sd(x,na.rm = TRUE) 
  
  c <- var(x,na.rm = TRUE)
  
  d <- IQR(x,na.rm = TRUE)
  
  e <- max(x,na.rm = TRUE)-min(x,na.rm = TRUE)
  
  print(a) # Min., Max., Quartile, Median, Mittelwert ausgeben
  print(b) # Standardabweichung ausgeben
  print(c) # Varianz ausgeben
  print(d) # Interquartilsabstand
  print(e) # Spannweite
  
  # a,b,c,d,e nacheinander ausgegeben
}

### oder mit describe Funktion, benoetigt "psych" Paket

my_afunktion2 <- function(x){
  a <- describe(x,IQR = TRUE) 
  
  print(a) # berechnet Stichprobenumfang, Mittelwert, Median, Min., Max.,
           # Standardabweichung, Interquartilsabstand, und Spannweite
  }

#e

# eine Funktion, die als input einen numerischen Vektor x mit ordinal skalierten
# Daten und, und die Daten in quantielbasiert kategorisiert zurückgibt.
kategorie_x <- function(x){
  
  
  # die Quantile von x berechnen:
  quant <- quantile(x, probs = seq(0, 1, length.out = 4))
  
  
  # definiere die Kategorie und sie zurückgeben:
  return(cut(x, breaks = quant, labels = c("niedrig", "mittel", "hoch"), 
             include.lowest = TRUE))

}

