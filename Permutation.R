
setwd("C:/temp/R")
#setwd("/Users/shisha/Dropbox/FOM/Semester_4/WissMethoden_quantitativeDatenanalyse")




main <- function(new_draws){
  print(Sys.time())
  lotto = read_file_csv2("data_lotto_date.csv")
  length_source <- nrow(lotto)
  number_new_draw = new_draws
  runs = 0
  lotto_new = create_new_lotto (runs,number_new_draw, length_source, lotto)
  print(Sys.time())
}


create_new_draw <- function(random_max, table){ 
  table_new = c()
  for (i in 1:6){
    r1 = integer()
    wert = floor(runif(1, min=1, max=random_max))
    r1 = c(table[wert,i])
    table_new[i] <- r1
  }
  return(table_new)
}


create_new_lotto <- function(count,number_new_draw,random_max, table){
  lotto_new = matrix()
  while (count <= number_new_draw) {
    new_draw = create_new_draw(random_max,table)
    if(!(anyDuplicated(new_draw))){
      FF <- as.matrix(t(new_draw))
      write.table(FF, file = "outputdir/output_new_lotto.csv", sep = ";", 
                  col.names = FALSE,row.names = FALSE, append=TRUE)
      count = count+1
    }
  }
  return(lotto_new)
}


read_file_csv2 <- function(filename){
  file_content <- read.csv2(filename)
  return(file_content)
}


test_lotto <- function(){
  lotto = read_file("lotto.txt")
  length_source <- nrow(lotto)
  x = create_new_draw(length_source, lotto)
  x = create_new_lotto(1,1000,length_source,lotto)
  return(x)
}



m=main(150000)




