# Funktionen-R-Skript 1 fuer Aufgabe 3

# Paket laden:
library(psych)
 
# a)

my_afunktion <- function(x){
  a <- summary(x) 
  
  b <- sd(x,na.rm = TRUE) 
  
  c <- var(x,na.rm = TRUE)
  
  d <- IQR(x,na.rm = TRUE)
  
  e <- max(x,na.rm = TRUE)-min(x,na.rm = TRUE)
  
  print(c(a,b,c,d,e)) # Min., Max., Quartile, Median, Mittelwert ausgeben
  print(b) # Standardabweichung ausgeben
  print(c) # Varianz ausgeben
  print(d) # Interquartilsabstand
  print(e) # Spannweite
  
  # a,b,c,d,e nacheinander ausgegeben
}
<<<<<<< Updated upstream

### Alternative:
=======
my_afunktion()
### oder mit describe Funktion, benoetigt "psych" Paket
>>>>>>> Stashed changes

# descriptive_stats: Berechnet simple deskriptive Statistiken von einem Vektor/
#                    einer Datenmenge.
#
# Eingabe: v (numerischer Vektor)
#
# Ausgabe: deskriptive Statistiken

descriptive_stats <- function(v){
  
  # Abrruchkriterium:
  stopifnot(is.numeric(v))
  
  # sortierte Daten speichern:
  sv <- sort(v)
  
  # Berechne sinnvolle Deskriptive Statistiken:
  stats <- c(length(v), max(v), min(v), abs(max(v) - min(v)),
             mean(v), median(v), sv[ceiling(length(sv) * 0.75)], 
             sv[ceiling(length(sv) * 0.25)], sd(v), mad(v),
             as.numeric(names(sort(-table(age)))[1]), length(unique(v)))
  
  # Tabelle erstellen:
  res <- data.frame(Statistics = c("Size", "Maximum", "Minimum", "Range",
                                   "Mean", "Median", "0.75-Quartile", 
                                   "0.25-Quartile", "SD",
                                   "MAD", "Mode", "n-Distinct" 
                                   ), Value = stats)
  
  # Tabelle printen:
  print(res)
}

# Erstelle Beispiel:
age <- round(rnorm(50, mean = 20, sd = 3))
descriptive_stats(age)


<<<<<<< Updated upstream


 
=======
  
  
# b)



# c) 

install.packages("DescTools")

library(DescTools)

# Kreutabelle 

kreuztabelle <- xtabs(~ d$Geschlecht + d$Studiengang , data=d) # x ist eine Spalte des Datensatzes und y eine andere Spalte ,data ist einfach der Datensatz
ftable(kreuztabelle)   # Häufigkeiten
prop.table(kreuztabelle) # Prozente

# Spearman Rangkorrelation und Rangkorrelation nach Kendall (ACHTUNG: nur für ordinale Daten verwenden)

cor(x,y,method = "spearman") 

cor(x,y,method = "kendall")

# Phi koeffizient, Kontingenzkoeffizient und Cramers V
# ACHTUNG Phi koeffizient nur bei dichotomen Variablen,also Variablen die nur zwei Merkmale annehmen können benutzen)

Phi(x,y) # Phi- Koeffizient

ContCoef(x,y) # Pearsons Kontingenzkoeffizient

CramerV(x,y) # Cramer V




## Funktion

my_cfunction <- function(x,y){
  
   a <- cor(x,y,method = "spearman") 
   
   b <- cor(x,y,method = "kendall")
   
   c <- Phi(x,y)
   
   d <- ContCoef(x,y)
   
   e <- CramerV(x,y)
   
   print(c(a,b,c,d,e))
   
}

my_cfunction(x,y)
