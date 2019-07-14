#Bootstrap | Permutation Vergleich
setwd("/Users/shisha/Desktop/WissMethoden_quantitativeDatenanalyse")

library(mosaic)

data_lotto_total <- read.csv2("data_lotto_date.csv")
data_lotto_total
Orig_Fibo1 <- read.csv2("1_Sort_by_Fibo.csv")
rbind(Orig_Fibo1)
Orig_Fibo2 <- read.csv2("2_Sort_by_Fibo.csv")
rbind(Orig_Fibo2)
Orig_Fibo21 <- read.csv2("21_Sort_by_Fibo.csv")
rbind(Orig_Fibo21)
Orig_Fibo34 <- read.csv2("34_Sort_by_Fibo.csv")
rbind(Orig_Fibo34)

colnames(data_lotto_total) <- c("GewZahl_1", "GewZahl_2","GewZahl_3","GewZahl_4","GewZahl_5","GewZahl_6","Wochentag")
colnames(Orig_Fibo1) <- c("GewZahl_1", "GewZahl_2","GewZahl_3","GewZahl_4","GewZahl_5","GewZahl_6","Wochentag")
colnames(Orig_Fibo2) <- c("GewZahl_1", "GewZahl_2","GewZahl_3","GewZahl_4","GewZahl_5","GewZahl_6","Wochentag")
colnames(Orig_Fibo21) <- c("GewZahl_1", "GewZahl_2","GewZahl_3","GewZahl_4","GewZahl_5","GewZahl_6","Wochentag")
colnames(Orig_Fibo34) <- c("GewZahl_1", "GewZahl_2","GewZahl_3","GewZahl_4","GewZahl_5","GewZahl_6","Wochentag")



quota <- function(data1,data2,quelle,filename1) {
  #Fibo x mit y [1]
  total_row <- nrow(quelle)
  test <- rbind(data1,data2)
  result <- which(duplicated(test))
  IDs <<- c(result)
  ID_data <- test[IDs,]
  Sa <- ID_data %>% filter(Wochentag == "Samstag")
  Mi <- ID_data %>% filter(Wochentag == "Mittwoch")
  An <- ID_data %>% filter(Wochentag != "Samstag") %>% filter(Wochentag != "Mittwoch")
  Sam <- nrow(Sa)
  Mit <- nrow(Mi)
  And <- nrow(An)
  length <- length(result)
  quota <- (length)/(total_row)
  total_data <- data.frame(quota)
  leng <- data.frame(length)
  #print(quota)
  quota <- c(total_data,leng, Mit, Sam, And)
  write.table(quota, file = filename1, sep = ";", 
              col.names = FALSE,row.names = FALSE, append=TRUE)
  #colnames("Anteil","Ziehungsanzahl","Ziehungen Mittwoch","Ziehungen Samstag","Ziehungen andere Tage")
}

filename1 = "Lotto_Quota_Fibo_1_2.csv"
Orig_Fibo_1_2 <- quota(Orig_Fibo1,Orig_Fibo2,data_lotto_total,filename1)
filename2 = "Lotto_Quota_Fibo_21_34.csv"
Orig_Fibo_21_34 <- quota(Orig_Fibo21,Orig_Fibo34,data_lotto_total,filename2)

Bootstrap_Fibo_1_2 <- read.csv2("Bootstrap_Quota_Fibo_1_2.csv")
colnames(Bootstrap_Fibo_1_2) <- c("Anteil","Ziehungsanzahl","Ziehungen Mittwoch","Ziehungen Samstag","Ziehungen andere Tage")
Bootstrap_Fibo_21_34 <- read.csv2("Bootstrap_Quota_Fibo_21_34.csv")
colnames(Bootstrap_Fibo_21_34) <- c("Anteil","Ziehungsanzahl","Ziehungen Mittwoch","Ziehungen Samstag","Ziehungen andere Tage")

Permutation_Fibo_1_2 <- read.csv2("Permutation_Quota_Fibo_1_2.csv")
colnames(Permutation_Fibo_1_2) <- c("Anteil","Ziehungsanzahl","Ziehungen Mittwoch","Ziehungen Samstag","Ziehungen andere Tage")
Permutation_Fibo_21_34 <- read.csv2("Permutation_Quota_Fibo_21_34.csv")
colnames(Permutation_Fibo_21_34) <- c("Anteil","Ziehungsanzahl","Ziehungen Mittwoch","Ziehungen Samstag","Ziehungen andere Tage")

Lotto_Fibo_1_2 <- read.csv2("Lotto_Quota_Fibo_1_2.csv", header = FALSE)

colnames(Lotto_Fibo_1_2) <- c("Anteil","Ziehungsanzahl","Ziehungen Mittwoch","Ziehungen Samstag","Ziehungen andere Tage")
Lotto_Fibo_21_34 <- read.csv2("Lotto_Quota_Fibo_21_34.csv")


m1 <- mean(Bootstrap_Fibo_1_2$Ziehungsanzahl)
m2 <- Lotto_Fibo_1_2[,2]
q <- quantile( ~ Ziehungsanzahl, probs = c(0.025, 0.975), data = Bootstrap_Fibo_1_2)
q <- as.data.frame(q)
q1 <- q[1,1]
q2 <-q[2,1]
gf_bar(~Ziehungsanzahl, data = Bootstrap_Fibo_1_2) %>% gf_vline(colour=c("black","black", "red"), xintercept =c(q1,q2,m2))


gf_bar(~Bootstrap_Fibo_1_2[,5], data = Bootstrap_Fibo_1_2)
gf_barh(~Anteil, data = Bootstrap_Fibo_1_2)

gf_bar(~Ziehungsanzahl, data = Permutation_Fibo_21_34)
a <- gf_barh(~Permutation_Fibo_21_34[,1], data = Permutation_Fibo_21_34)
geom_path(data=Permutation_Fibo_21_34,x=c(wert),y=10000)
a
gf_barh(~Anteil, data = Bootstrap_Fibo_1_2)


gf_bar(~Bootstrap_Fibo_21_34[,2], data = Bootstrap_Fibo_21_34)
w = Permutation_Fibo_21_34$Ziehungsanzahl
IQR(w)
inspect(w)

gf_histogram(w)
pop_sd <- sd(w)*sqrt((length(w)-1)/(length(w)))

pop_sd
pop_mean <- mean(w)
pop_mean

z <- (63 - pop_mean)/pop_sd
z

p_yellow1 <- pnorm(63, pop_mean, pop_sd)
p_yellow2 <- pnorm(z)

p_yellow1
p_yellow2

p_blue1 <- 1 - p_yellow1
p_blue2 <- 1 - p_yellow2 

p_blue1
