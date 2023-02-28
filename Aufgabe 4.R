
# Aufgabe 4

library(readr)

# Datensatz_Aufgabe2 <- read.csv("C:\...")  Einlesen vom Datensatz aus Aufg.2

age <- Datensatz_Aufgabe2$age
Studiengang <- Datensatz_Aufgabe2$subject
INT_Mathe <- Datensatz_Aufgabe2$maths_rating
INT_Prog <- Datensatz_Aufgabe2$coding_rating
Mathe_LK <- Datensatz_Aufgabe2$advanced_maths

# Deskriptive Statistiken mit funktionen aus 3 a)

age <- Datensatz_Aufgabe2$age

descriptive_stats(age)

#      Statistics      Value
# 1           Size 100.000000
# 2        Maximum  29.000000
# 3        Minimum  20.000000
# 4          Range   9.000000
# 5           Mean  24.860000
# 6         Median  25.000000
# 7  0.75-Quartile  26.000000
# 8  0.25-Quartile  24.000000
# 9             SD   1.874981
# 10           MAD   1.482600
# 11          Mode  24.000000
# 12    n-Distinct  10.000000


# Deskriptive Statistiken mit funktionen aus 3 b)

library(MASS)

categorical_var(Datensatz_Aufgabe2)  # Path bezogene Funktion, auslese von Datensatz_Aufgabe2
#spalte Name: subject 
#Anzahl von unique werten: 4
#Häufigkeit von Data Science: 37
 #Häufigkeit von Informatik: 23
 #Häufigkeit von Mathematik: 10
 #Häufigkeit von Statistik: 30
#Prozent von Data Science: 37 %
 #Prozent von Informatik: 23 %
 #Prozent von Mathematik: 10 %
 #Prozent von Statistik: 30 %
#Relativ Häufigkeit von Data Science: 37/100
 #Relativ Häufigkeit von Informatik: 23/100
# Relativ Häufigkeit von Mathematik: 1/10
 #Relativ Häufigkeit von Statistik: 3/10
#Anzahl von fehlenden werten: 0 (0.00%)
 #         Name Häufigkeit Prozent Relative Häufigkeit fehlende Werte in prozent
#1 Data Science         37      37                0.37              0          0
#2   Informatik         23      23                0.23              0          0
#3   Mathematik         10      10                0.10              0          0
#4    Statistik         30      30                0.30              0          0







# Deskriptive Statistiken mit funktionen aus 3 c)

kreuztabelle <- xtabs(~ x_row + y_column, Datensatz_Aufgabe2) 



ftable(kreuztabelle) 

###################
#       y_column Data Science Informatik Mathematik Statistik
# x_row                                                      
# 0                         7          4          2         5
# 1                        25         22          6        29
###################


####   0 = nein, 1 = ja,
## Es besteht ein Zusammenhang Zwischen all diesen
## Mathematischen/ Informatik- Studiengängen und der Wahl des Mathe-LK's



prop.table(kreuztabelle) 

###################
# x_row Data Science Informatik Mathematik Statistik
#     0         0.07       0.04       0.02      0.05
#     1         0.25       0.22       0.06      0.29
###################


## deutlich wird es and er Aufteilung da 82% aller Befragten Studenten den Mathe-LK
## belegt haben

## um aber weitere Deutungen zu erheben, muesste man einen Studiengan nehmen, der nicht 
## mathematisch angelehnt ist, z.B. Sprachwissenschaften oder Kunst und Musik.

my_cfunction(INT_Prog,INT_Mathe)
#[1] -0.1814017 -0.1408809  0.4852707  0.4365810  0.1981109

## nach Def. der einzelnen Werte zeigt sich das es zwischen dem Interesse an Programmierung und Mathematik 
## nur minimale Zusammenhänge gibt, diese sind leider nicht stark genug um eine 
## eindeutige Aussage zu treffen.


my_cfunction(INT_Prog,Mathe_LK)
#[1] 0.10716072 0.09518088 0.24725881 0.24003030 0.24725881


## Doch zeigt sich das bei Interesse an Programmierung, dass vermehrt auch den Mathe-LK
## hatten 


my_cfunction(INT_Mathe,Mathe_LK)
#[1] -0.1288486 -0.1130193  0.2290166  0.2232372  0.2290166

## das Gleiche gilt leider nicht für das Interesse an Mathematik im vergleich zur 
## LK wahl, denn hier deutet es auf keinen eindeutigen Zusammenhang sondern auf einen 
## darauf das es in keiner Relation zueinander Steht.





# bivariate Statistiken mit funktionen aus 3 d)

bivariate_stats(Datensatz_Aufgabe2,2,6)

#   variable   mean_0   mean_1 n_0 n_1   cohen_d
# 1        2 25.05263 24.81481  19  81        NA
# 2        6       NA       NA  NA  NA 0.1268369

## 


# quantielbasierte Daten e)

O <- data.frame(age,INT_Mathe,INT_Prog)

apply(O,2, kategorie_x)
#      age       INT_Mathe INT_Prog 
# [1,] "niedrig" "hoch"    "mittel" 
# [2,] "niedrig" "mittel"  "mittel" 
# [3,] "mittel"  "niedrig" "mittel" 
# [4,] "hoch"    "hoch"    "hoch"   
# [5,] "mittel"  "niedrig" "niedrig"
# [6,] "niedrig" "mittel"  "hoch"   
# [7,] "niedrig" "niedrig" "niedrig"
# [8,] "mittel"  "hoch"    "mittel" 
# [9,] "mittel"  "niedrig" "niedrig"

# Visuelle Darstellung dieser Daten

par(mfrow = c(1,1))
my_plotfunction(Studiengang, main="Studiengang")

par(mfrow = c(1,3))

my_plotfunction(age,main = "age")
my_plotfunction(INT_Mathe, main = "INT_Mathe")
my_plotfunction(INT_Prog,main= "INT_Prog")

# es wird sichtbar dass hauptsaechlich juengere Studierende vorhanden sind,
# im Allgemeinen ein durchschnittliches Interesse and Mathematik und Programmierung 
# ueberwiegt.
# Und die Programmierung doch von mehr Studierenden als Interesant gedeutet 
# wurde als Mathematik

