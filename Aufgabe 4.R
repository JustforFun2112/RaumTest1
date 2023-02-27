
# Aufgabe 4

library(readr)

Datensatz <- read.csv("C:\...")




# Deskriptive Statistiken mit funktionen aus 3 a)

age <- Datensatz$age

descriptive_stats(age)

#       Statistics      Value
# 1           Size 100.000000
# 2        Maximum  29.000000
# 3        Minimum  19.000000
# 4          Range  10.000000
# 5           Mean  25.110000
# 6         Median  25.000000
# 7  0.75-Quartile  26.000000
# 8  0.25-Quartile  24.000000
# 9             SD   1.632344
# 10           MAD   1.482600
# 11          Mode  26.000000
# 12    n-Distinct   9.000000


# Deskriptive Statistiken mit funktionen aus 3 b)

library(MASS)

categorical_var("C:\...")
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





# bivariate Statistiken mit funktionen aus 3 d)
data <- read.csv("C:\...")

bivariate_stats(data,6,7)

#variable   mean_0   mean_1 n_0 n_1     cohen_d
#1        6 3.404255 3.471698  47  53          NA
#2        7       NA       NA  NA  NA -0.03287552

