# Dateipfad festlegen
setwd("/Users/shirin/Dropbox/FOM/Semester_4/WissMethoden_quantitativeDatenanalyse")
#setwd("H:/R")

# CSV Datei auswählen
data_lotto <- read.csv2("data_lotto_date.csv")
# Liste von Zahlen, nach welchen gesucht und sortiert werden soll
search_list = list(1,2,3,5,8,13,21,34)
# Zufallsgenerator festlegen
#set.seed(1896)


# sort_by_fibo ist eine Funktion, welche beim Aufruf zwei Übergabeparameter erwartet. Diese beiden parameter werden in die Variablen table und search_for
# gespeichert.
sort_by_fibo <- function(table, search_for){
  # leange ist eine variable, welche die laenge der variable table mittels der funktion nrow() ermittelt und speichert
  laenge = nrow(table)
  # eine for-schleife, welche so oft durchläuft, wie die variable table Zeilen hat. Die Anzahl der Durchläufe wird mit der Variable laenge bestimmt
  for (i in 1:laenge){
    # wert speichert die Zeile der Tabelle table. Es wird die jeweilige Zeile gespeichert, in welchem Durchlauf wir uns befinden. Erster Durchlauf = erste Zeile
    # zweiter Durchlauf = Zweite Zeile, 50. Durchlauf = 50. Zeile usw. i ist der aktuelle Durchlaufwert
    wert = table[i,]
    # eine for-schleife, welche so oft durch läuft, bis die Anzahl an Spalten erreicht ist, welche die in wert ermittelte Zeile hat. Eine zeile besteht normal auf 6
    # Spalten, für jede Lottozahl der Ziehung. Sie kann aber auch ein datum enthalten, oder eine 7. Superzahl.
    for (j in 1:ncol(wert)){
      # zelle speichert den Inhalt der Spalte der Zeile, abhänig davon in welchem Durchlauf sich die Schleife befindet. 1. Durchlauf Inhalt 1.Spalte der Zeile.
      # 6. Durchlauf Inhalt der 6. Spalte der Zeile
      zelle = wert[,j]
      # Ruft die Funktion check_fobi auf und übergibt ihr 3 Werte
      # Inhalt der aktuellen Zelle (zelle), die aktuelle Zeile (wert), und die Liste mit Zahlen, nach welchen gesucht werden soll (search_for)
      check_fobi(zelle, wert, search_for)
    }
  }
}

# check_fobi ist eine Funktion, welche drei Werte als Übergabeparameter erwartet, eine Zahl, eine Zeile und eine Liste an Einträgen, die mit der Zahl verglichen
# werden soll.
check_fobi <- function(zahl, zeile1, search_for){
  # if-Abfrage, welche überprüft, ob die gegeben Zahl (zahl) in der Liste an Zahlen (search_for) enthalten ist. Wenn dem so ist, wird der Inhalt der If-Abfrage
  # ausgeführt.
  if (zahl %in% search_for){
    # ruft die Funktion check() auf und übergibt ihr zwei Werte, den inhalt der Variable zeile1, welches eine Zeile mit 6 Zellen ist und die variable zahl, welches
    # eine der 6 Zellen ist.
    check(zeile1,zahl)
  }
}

# check ist eine funktion, welche zwei werte als Übergabeparameter erwartet, zeile2, welches eine ganze Zeile ist, und zelle, welches der Wert einer Zelle der Zeile ist
check <- function(zeile2, zelle){
  # filename ist der Name der Datei, in welche die Zeile geschrieben wird, welche in der Variable zeile2 gespeichert ist. Die variable filename wird zusammen gesetzt
  # aus der Zahl in der Variable zelle und dem String "_check_output.csv". Wenn die Zelle eine 3 ist enthält die Variable filename den Wert "3_check_output.csv".
  filename = paste(zelle, "_Sort_by_Fibo.csv",sep="")
  # write.table ist eine funktion, welche einen Wert (zeile2) in eine Datei schreibt (filename), als Trennzeichen (seperator) wird das Zeichen ";" mitgegeben.
  # außerdem wird festgelegt, dass es keine Zeilennamen und keine Spaltennamen gibt. Das was geschrieben wird (zeile2) wird an das bestehene File angehängt (append = TRUE)
  write.table(zeile2, file = filename, sep = ";", 
              col.names = FALSE,row.names = FALSE, append=TRUE)
}


# startet das Programm durch den Aufruf der Sort_by_fibo Funktion. Übergibt der Funktion die beiden zuvor Festgelegten Variablen data_lotto und
# search_list.
n = sort_by_fibo(data_lotto, search_list)
