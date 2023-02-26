# Aufgabe 4

library(readr)

Datensatz <- read_excel("C:\...")




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

