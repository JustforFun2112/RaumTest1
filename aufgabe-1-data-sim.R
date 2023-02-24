## Wissenschafliches Arbeiten WS2022/23 - Gruppe 11
## Erstellung des Datensatzes:
## Aufgabe 1)

# Paket um Excel Datei tu erstellen:
# install.packages("writexl")
library(writexl)

# sim_data: Funktion die Datensatz simulieren soll:
#           - 1. Spalte: ID
#           - 2. Spalte: Alter
#           - 3. Spalte: Studienfach
#           - 4. Spalte: Interesse an Mathematik (1-7)
#           - 5. Spalte: Interesse am Programmieren (1-7)
#           - 6. Spalte: Mathe LK (Ja/Nein)
#
# Eingabe: - N (Anzahl an zu simulierenden Beobachtungen)
#          - showData (TRUE, falls data.frame ausgegeben werden soll)
#
# Ausgabe: generierter Datensatz als data.frame in .R und speichern als .csv

sim_data = function(N, showData = "False"){
  # browser()
  
  # Abbruchkriterium:
  stopifnot(is.numeric(N))
  
  # 1 - ID Spalte:
  id <- 1:N
  
  # 2 - Alter Spalte:
  # Basierend auf Normalverteilung aus Aufgabenstellung.
  # Das Alter wird gerundet, da niemand Alter in nicht natuerlichen Zahlen angibt!
  age <- round(rnorm(N, mean = 25, sd = 2)) 
  
  # 3 - Studienfach Spalte:
  # Waehle die W'keiten von Statistik und Data Science jeweils 1/3 bzw 3/9,
  # Informatik 2/9 und Mathematik 1/9.
  subject_pool <- c("Statistik", "Data Science", "Informatik", "Mathematik")
  # Waehle nun aus dem Pool zufaellige Studienfaecher N Mal aus:
  subject <- sample(subject_pool, N, replace = TRUE, prob = c(3/9, 3/9, 2/9, 1/9)) 
  
  # Schreibe Spalte 4 und Spalte 5 in einer Schleife, um Rechenzeit zu
  # verkuerzen. Lege zunaechst die W'keiten fest, Mathe zu moegen:
  # 4 - Interesse an Mathematik und an Programmierung Spalten gleichzeitig
  maths_ma <- c(0.05, 0.05, 0.05, 0.10, 0.20, 0.30, 0.25) # Mathematik
  maths_st <- c(0.10, 0.10, 0.10, 0.10, 0.20, 0.20, 0.20) # Statistik
  maths_ds <- c(0.10, 0.15, 0.15, 0.15, 0.20, 0.15, 0.15) # Data Science
  maths_in <- c(0.10, 0.10, 0.15, 0.15, 0.20, 0.15, 0.15) # Informatik
  # Dazu nehme an, das Interesse an Mathematik sei von gross nach niedrig so
  # sortiert: Mathematik >> Statistik > Data Science == Informatik
  
  # 5 - Interesse an Programmieren Spalte:
  # Gehe dabei wieder von einem Interesse von gross nach klein sortiert aus:
  # Informatik > Data Science >> Statistik == Mathematik
  code_in <- c(0.02, 0.04, 0.06, 0.08, 0.20, 0.30, 0.30) # Informatik
  code_ds <- c(0.05, 0.10, 0.10, 0.15, 0.20, 0.25, 0.15) # Data Science
  code_st <- c(0.10, 0.10, 0.10, 0.15, 0.15, 0.15, 0.10) # Statistik
  code_ma <- c(0.10, 0.10, 0.10, 0.15, 0.15, 0.15, 0.10) # Mathematik
  # Aus Gruenden der Veranschaulichung in 2 Nachkommastellen angegeben!
    
  # Simulation von 4 und 5 (Idee: Shaban)
  rating <- sapply(subject, function(x){
    # 4 if Bedingungen fuer jedes Fach:
   if (x == "Mathematik"){
      c(sample(1:7, 1, replace = TRUE, prob = maths_ma),
        sample(1:7, 1, replace = TRUE, prob = code_ma))
    } else if (x == "Statistik"){
      c(sample(1:7, 1, replace = TRUE, prob = maths_st),
        sample(1:7, 1, replace = TRUE, prob = code_st))
    } else if (x == "Data Science"){
      c(sample(1:7, 1, replace = TRUE, prob = maths_ds),
        sample(1:7, 1, replace = TRUE, prob = code_ds))
    } else{ # Hier Informatik:
      c(sample(1:7, 1, replace = TRUE, prob = maths_in), 
        sample(1:7, 1, replace = TRUE, prob = code_in))
    }
  })
  # Theoretisch haette man die W'keiten auch umdrehen koennen, aber es sollen 
  # leichte Unterschiede zu sehen sein.
  # Die Einfluesse sind sehr leicht, muessten aber im Anschluss nachvollziebar
  # sein! 
  
  maths_rating <- as.vector(rating[1, ])
  coding_rating <- as.vector(rating[2, ])
  
  # 6 - Mathe LK Spalte: (Ja/ Nein)
  # Vermutlich hat der Grossteil der Studierenden Mathe_LK gehabt. Gehe 
  # einfach davon aus, dass die Mehrheit Mathe LK hatte
  # (Unabhaengig von Studiengang)
  advanced_maths <- sample(0:1, N, replace = TRUE, prob = c(0.2, 0.8))
  
  # Zusammenstellen zu einem data.frame:
  data <- data.frame(id, age, subject, maths_rating, coding_rating, advanced_maths)
  write_xlsx(data, "Datensatz_Aufgabe1.xlsx")

  # gebe data.frame auch in R zurueck:
  if(showData == TRUE){return(data)}
}

# Zuletzt noch die Funktion sim_data ausfuehren:
# Funktion wird ohne seed ausgefuehrt, damit die gleich eingestellten W'keiten
# nicht exakt aussehen. Einzelnd immer einen seed zu setzten ist auch witzlos.
sim_data(100, showData = "FALSE")
    

