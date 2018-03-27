library(readr)
Player_Data <- read_csv("E:/6thsemcse/cisco/Player_Data.csv")
View(Player_Data)

Player_Data$X1 <- NULL
Player_Data$Bat_SR[Player_Data$Bat_SR == 120]
Player_Data$Bat_SR[Player_Data$Bat_SR == 120] = mean(Player_Data$Bat_SR)


Player_Data$X1 <- NULL
mean(Player_Data$Bat_SR)
Player_Data$Name[Player_Data$Bat_SR == 120.00]
Player_Data$Bat_SR[Player_Data$Bat_SR == 120.00] = mean(Player_Data$Bat_SR)
Player_Data$runsPerInnings <- Player_Data$Runs_Scored/Player_Data$Bat_Inning
Player_Data$wktsPerInnings <- Player_Data$Wkts/Player_Data$Bowl_Inning
Player_Data[is.na(Player_Data$wktsPerInnings) , "wktsPerInnings"] <- 0

a = 0


Knn <- function(name1, Player_Data){
  flag  = 0
  tempdf <- data.frame(value = numeric() , i = numeric())
  res = c()
  i = 1
  b = Player_Data$Name == name1
  if(sum(Player_Data$Name == name1) == 1) {
    flag = 1
    while(i < 88 ) {
      tempdf[i,] <- c(((abs(Player_Data$Bat_Avg[i] - Player_Data$Bat_Avg[b])^2 +
                    abs((Player_Data$Bat_SR[i])-(Player_Data$Bat_SR[b]))^2 + 
                    abs((Player_Data$runsPerInnings[i])-(Player_Data$runsPerInnings[b]))^2+
                    abs((Player_Data$X6s[i])-(Player_Data$X6s[b]))^2 + 
                    abs((Player_Data$X4s[i])-(Player_Data$X4s[b]))^2)**0.5)/5 , i)
      i = i + 1
    }  
    tempdf = tempdf[order(tempdf$value),]
    return(tempdf)
  } else {
    return(a)
  }
}

closestBatsmen <- data.frame(name = character(),b1 = character() ,b2 = character() ,b3 = character(),b4 = character(),
                                    b5 = character() ,b6 = character() , b7 = character() ,b8 = character() ,
                                    b9 = character() ,b10 = character() ,stringsAsFactors = FALSE)
# K = 10
i  = 1

while(i < 89){
  a = NULL
  a = Knn(Player_Data$Name[i],Player_Data)
  closestBatsmen[i, ] <- c(Player_Data$Name[i] , Player_Data$Name[a$i[2]] , Player_Data$Name[a$i[3]] , Player_Data$Name[a$i[4]] , 
                           Player_Data$Name[a$i[5]], Player_Data$Name[a$i[6]], Player_Data$Name[a$i[7]], 
                           Player_Data$Name[a$i[8]],Player_Data$Name[a$i[9]], Player_Data$Name[a$i[10]], 
                           Player_Data$Name[a$i[11]])
  i = i + 1
}

write.csv(closestBatsmen, file = "E:/6thsemcse/cisco/knnBat.csv")

