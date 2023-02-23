# Funktionen-R-Skript 1 fuer Aufgabe 3


# Paket laden:
library(psych)

# a) ...


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

### Alternative:

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



# b)
library(MASS)
categorical_var <- function(csvfile) {
  # CSV-Datei in einen Dataframe einlesen
  dataframe <- read.csv(csvfile)
  
  # Kategoriale Variablen im Dataframe suchen
  categorical_vars <- c()
  for (column in names(dataframe)) {
    if (is.factor(dataframe[[column]]) || is.character(dataframe[[column]])) {
      categorical_vars <- c(categorical_vars, column)
        value_counts <- table(dataframe[[column]], useNA = "ifany")
        value_percents <- prop.table(value_counts) * 100
        
        name_col <- unique(dataframe[[column]])
        # Berechnung der Anzahl der fehlenden Werte und des Prozentsatzes
        num_missing <- sum(is.na(categorical_vars))
        percent_missing <- mean(is.na(categorical_vars)) * 100
        
        # Deskriptive Statistik:
        cat(sprintf("spalte Name: %s \n",categorical_vars))
        cat(sprintf("Anzahl von unique werten: %d\n", length(value_counts)))
        cat(sprintf("Häufigkeit von %s: %s\n", name_col, value_counts))
        cat(sprintf("Prozent von %s: %s %%\n", name_col, value_percents))
        cat(sprintf("Relativ Häufigkeit von %s: %s\n", name_col, fractions(prop.table(value_counts))))
        cat(sprintf("Anzahl von fehlenden werten: %d (%.2f%%)\n", num_missing, percent_missing))

    }
  }
}







# c) 

install.packages("DescTools")

library(DescTools)

# Kreutabelle 

kreuztabelle <- xtabs(~ x + y , data=...) # x ist eine Spalte des Datensatzes und y eine andere Spalte ,data ist einfach der Datensatz
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


# d)

'''
cohens_d: Cohens d-Effektgröße, ein standardisiertes Maß für den 
Unterschied zwischen zwei Mittelwerten, 
berechnet als die Differenz zwischen den Mittelwerten geteilt durch die gepoolte Standardabweichung.
'''

bivariate_stats <- function(df, metric_col, dich_col) {
  
  # Dataframe filtern, um nur Beobachtungen mit nicht na Werten in beiden Spalten aufzunehmen. 
  df <- df[complete.cases(df[, c(metric_col, dich_col)]), ]
  
  #  Der Mittelwert jeder dichotome Variable berechnen
  means <- aggregate(df[[metric_col]], by=list(df[[dich_col]]), FUN=mean)
  
  # Cohen's d berechnen
  d <- (means$x[1] - means$x[2]) / sd(df[[metric_col]])
  
  # Ausgabe Dataframe erstellen
  output_df <- data.frame(
    variable = c(metric_col, dich_col),
    mean_0 = c(mean(df[df[[dich_col]] == 0, metric_col]), NA),
    mean_1 = c(mean(df[df[[dich_col]] == 1, metric_col]), NA),
    n_0 = c(sum(df[[dich_col]] == 0), NA),
    n_1 = c(sum(df[[dich_col]] == 1), NA),
    cohen_d = c(NA, d)
  )
  
  return(output_df)
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

# f)

# zur erkennung nutzen wir table um den gegebenen Vekotor in eine weitere numerische
# Aufteilung zu unterteilen
# dann wird nur noch als output der Gegebene Vektor in einen Graphen mit Balkendiagrammen erstellt.
my_plotfunction <- function(x){
  z <- table(x)
  
  return(barplot(z))
}
