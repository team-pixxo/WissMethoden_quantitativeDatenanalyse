# Dateipfad festlegen
setwd("/Users/shirin/Dropbox/FOM/Semester_4/WissMethoden_quantitativeDatenanalyse")

#Gesamt-Lottoziehung einlesen
data_lotto_total <- read.csv2("data_lotto_date.csv")
datalength <- nrow(data_lotto_total)
datalength

#Listen einlesen

filename_list <- list("1_Sort_by_Fibo.csv","2_Sort_by_Fibo.csv","3_Sort_by_Fibo.csv","5_Sort_by_Fibo.csv","8_Sort_by_Fibo.csv","13_Sort_by_Fibo.csv","21_Sort_by_Fibo.csv","34_Sort_by_Fibo.csv")


Fibo1 <- read.csv2("1_Sort_by_Fibo.csv")
rbind(Fibo1)
Fibo2 <- read.csv2("2_Sort_by_Fibo.csv")
rbind(Fibo2)
Fibo3 <- read.csv2("3_Sort_by_Fibo.csv")
rbind(Fibo3)
Fibo5 <- read.csv2("5_Sort_by_Fibo.csv")
rbind(Fibo5)
Fibo8 <- read.csv2("8_Sort_by_Fibo.csv")
rbind(Fibo8)
Fibo13 <- read.csv2("13_Sort_by_Fibo.csv")
rbind(Fibo13)
Fibo21 <- read.csv2("21_Sort_by_Fibo.csv")
rbind(Fibo21)
Fibo34 <- read.csv2("34_Sort_by_Fibo.csv")
rbind(Fibo34)

colnames(Fibo1) <- c("GewZahl_1", "GewZahl_2","GewZahl_3","GewZahl_4","GewZahl_5","GewZahl_6","Wochentag")
colnames(Fibo2) <- c("GewZahl_1", "GewZahl_2","GewZahl_3","GewZahl_4","GewZahl_5","GewZahl_6","Wochentag")
colnames(Fibo3) <- c("GewZahl_1", "GewZahl_2","GewZahl_3","GewZahl_4","GewZahl_5","GewZahl_6","Wochentag")
colnames(Fibo5) <- c("GewZahl_1", "GewZahl_2","GewZahl_3","GewZahl_4","GewZahl_5","GewZahl_6","Wochentag")
colnames(Fibo8) <- c("GewZahl_1", "GewZahl_2","GewZahl_3","GewZahl_4","GewZahl_5","GewZahl_6","Wochentag")
colnames(Fibo13) <- c("GewZahl_1", "GewZahl_2","GewZahl_3","GewZahl_4","GewZahl_5","GewZahl_6","Wochentag")
colnames(Fibo21) <- c("GewZahl_1", "GewZahl_2","GewZahl_3","GewZahl_4","GewZahl_5","GewZahl_6","Wochentag")
colnames(Fibo34) <- c("GewZahl_1", "GewZahl_2","GewZahl_3","GewZahl_4","GewZahl_5","GewZahl_6","Wochentag")


#Anteile von am häufigsten gemeinsam gezogenen Fibozahlen bestimmen.


quota <- function(data1,data2,data3,data4,data5,data6,data7,data8,quelle) {
  #Fibo x mit y [1]
  total_row <- nrow(quelle)
  test1 <- rbind(data1,data2)
  result1 <- which(duplicated(test1))
  length1 <- length(result1)
  quota1 <- (length1)/(total_row)
  #Fibo x mit y [2]
  test2 <- rbind(data1,data3)
  result2 <- which(duplicated(test2))
  length2 <- length(result2)
  quota2 <- (length2)/(total_row)
  #Fibo x mit y [3]
  test3 <- rbind(data1,data4)
  result3 <- which(duplicated(test3))
  length3 <- length(result3)
  quota3 <- (length3)/(total_row)
  #Fibo x mit y [4]
  test4 <- rbind(data1,data5)
  result4 <- which(duplicated(test4))
  length4 <- length(result4)
  quota4 <- (length4)/(total_row)
  #Fibo x mit y [5]
  test5 <- rbind(data1,data6)
  result5 <- which(duplicated(test5))
  length5 <- length(result5)
  quota5 <- (length5)/(total_row)
  #Fibo x mit y [6]
  test6 <- rbind(data1,data7)
  result6 <- which(duplicated(test6))
  length6 <- length(result6)
  quota6 <- (length6)/(total_row)
  #Fibo x mit y [7]
  test7 <- rbind(data1,data8)
  result7 <- which(duplicated(test7))
  length7 <- length(result7)
  quota7 <- (length7)/(total_row)
  total_data <- data.frame(quota1,quota2,quota3,quota4,quota5,quota6,quota7)
  colnames(total_data) <- c("data1+2","data1+3","data1+4","data1+5","data1+6","data1+7","data1+8")
  leng <- data.frame(length1,length2,length3,length4, length5, length6, length7)
  colnames(leng) <- c("data1+2","data1+3","data1+4","data1+5","data1+6","data1+7","data1+8")
  #Fibo <<- sort(total_data)
  print(sort(total_data))
  print(sort(leng))
}

Fibo_1 <- quota(Fibo1,Fibo2,Fibo3,Fibo5,Fibo8,Fibo13, Fibo21, Fibo34,data_lotto_total)
Fibo_2 <- quota(Fibo2,Fibo1,Fibo3,Fibo5,Fibo8,Fibo13, Fibo21, Fibo34,data_lotto_total)
Fibo_3 <- quota(Fibo3,Fibo1,Fibo1,Fibo5,Fibo8,Fibo13, Fibo21, Fibo34,data_lotto_total)
Fibo_5 <- quota(Fibo5,Fibo2,Fibo3,Fibo1,Fibo8,Fibo13, Fibo21, Fibo34,data_lotto_total)
Fibo_8 <- quota(Fibo8,Fibo2,Fibo3,Fibo5,Fibo1,Fibo13, Fibo21, Fibo34,data_lotto_total)
Fibo_13 <- quota(Fibo13,Fibo2,Fibo3,Fibo5,Fibo8,Fibo1, Fibo21, Fibo34,data_lotto_total)
Fibo_21 <- quota(Fibo21,Fibo2,Fibo3,Fibo5,Fibo8,Fibo13, Fibo1, Fibo34,data_lotto_total)
Fibo_34 <- quota(Fibo34,Fibo2,Fibo3,Fibo5,Fibo8,Fibo13, Fibo21, Fibo1,data_lotto_total)



