## Wissenschafliches Arbeiten WS2022/23 - Gruppe 11
## Bearbeitet von Felix Hansch und ....


## Aufgabe 1

# sim_data: Funktion die Datensatz simulieren soll:
#           - 1. Spalte: ID
#           - 2. Spalte: Alter
#           - 3. Spalte: Studienfach
#           - 4. Spalte: Interesse an Mathematik (1-7)
#           - 5. Spalte: Interesse am Programmieren (1-7)
#           - 6. Spalte: Mathe LK (Ja/Nein)
#
# Eingabe: N (Anzahl an zu simulierenden Beobachtungen)
#
# Ausgabe: data.frame 

sim_data = function(N){
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
  
  # 4 - Interesse an Mathematik Spalte:
  # Der Zusammenhang dabei ist frei waehlbar. Idee: 
  # Mathematik >> Statistik > Informatik == Data Science (absteigendes Interesse)
  maths_rating <- NULL
  # Waehle aus Effizienzgruenden eine for-Schleife:
  for(i in 1:N){
    # Versuche alles moeglichst aehnlich zu verteilen:
    # Wenn das Individuum Mathematik studiert:
    if(subject[i] == "Mathematik"){
      maths_rating[i] <- sample(1:7, 1, replace = TRUE, 
                         prob = c(0.05, 0.05, 0.05, 0.1, 0.2, 0.3, 0.25))
    }
    # Wenn das Individuum Statistik studiert:
    if(subject[i] == "Statistik"){
      maths_rating[i] <- sample(1:7, 1, replace = TRUE, 
                         prob = c(0.1, 0.1, 0.1, 0.1, 0.2, 0.2, 0.2))
    }
    # Wenn das Individuum Informatik/ Data Science studiert:
    if(subject[i] == "Informatik" || subject[i] == "Data Science"){
      maths_rating[i] <- sample(1:7, 1, replace = TRUE, 
                         prob = c(0.1, 0.1, 0.1, 0.15, 0.2, 0.2, 0.15))
    }
    # ab hier wiederholt sich alles
    # Die Einfluesse sind sehr leicht, muessten aber im Anschluss nachvollziebar
    # sein! (probs sind in Summe 1)
  }
  
  # 5 - Interesse an Programmieren Spalte:
  # Idee: Vielleicht Informatik > Data Science >> Statistik > Mathematik
  coding_rating <- rep("ubk", N)
    
  # 6 - Mathe LK Spalte: (Ja/ Nein)
  # Idee: W'keit Mathe LK gehabt zu haben: 
  # Mathematik > Statistik > Data Science == Informatik (aehnlich wie Spass an Mathe)
  advanced_maths <- rep("ubk", N)
  
  # Zusammenstellen zu einem data.frame:
  return(data.frame(id, age, subject, maths_rating, coding_rating, advanced_maths))
}

# set.seed()? Noch einen Seed setzen?
sim_data(100)

# Irgendwie noch als csv abspeichern...
