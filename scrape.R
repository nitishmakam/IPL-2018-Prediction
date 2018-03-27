#install.packages('rvest')

library("rvest", lib.loc="~/R/win-library/3.4")
library("readr", lib.loc="~/R/win-library/3.4")

library(readr)
PlayerURLs <- read_csv("E:/6thsemcse/cisco/PlayerURLs.csv")
View(PlayerURLs)

batsmen_data_extract <- c("Bat_Inning" , "Runs_Scored", "Bat_Avg" , "Bat_SR" , "4s" , "6s")
bowler_data_extract <- c("Bowl_Inning", "Balls_B" , "Wkts" , "Econ", "Bowl_Avg" ,"Bowl_SR")

Player_Data <- NULL
Player_Data <- data.frame("Name" = character(), "Bat_Inning" = numeric() , "Runs_Scored" = numeric() , "Bat_Avg" = numeric() , 
                          "Bat_SR" = numeric() , "4s" = numeric() , "6s" = numeric() , 
                          "Bowl_Inning" = numeric() , "Balls_B" = numeric() , "Wkts" = numeric() ,     
                          "Econ" = numeric() , "Bowl_Avg" = numeric() ,  "Bowl_SR" = numeric() , stringsAsFactors=FALSE)
i = 4
[#25,27,#33,35, , #44 , #46 , #71]
#------------------------------------------Start-------------------------------
while(i<88){

    if(PlayerURLs$Debut[i] == 0){
      #Specifying the url for desired website to be scrapped
      url <- PlayerURLs$URL[i]
      
      #Reading the HTML code from the website
      webpage <- read_html(url)
      
      
      batsmen_data <- NULL
      bowler_data <- NULL
      
    
      batsmen_data_html <- html_nodes(webpage,'tr+ tr .text-right:nth-child(14) , .cb-plyr-tbl:nth-child(2) tr+ tr .text-right:nth-child(13) , .cb-plyr-tbl:nth-child(2) tr+ tr .text-right:nth-child(9) , .cb-plyr-tbl:nth-child(2) tr+ tr .text-right:nth-child(7) , .cb-plyr-tbl:nth-child(2) tr+ tr .text-right:nth-child(5) , .cb-plyr-tbl:nth-child(2) tr+ tr .cb-plyr-tbody+ .text-right')#TI
      
      bowler_data_html <- html_nodes(webpage,'.cb-plyr-tbl+ .cb-plyr-tbl tr+ tr .text-right:nth-child(6) , .cb-plyr-tbl+ .cb-plyr-tbl tr+ tr .text-right:nth-child(11) , .cb-plyr-tbl+ .cb-plyr-tbl tr+ tr .text-right:nth-child(10) , .cb-plyr-tbl+ .cb-plyr-tbl tr+ tr .text-right:nth-child(4) , .cb-plyr-tbl+ .cb-plyr-tbl tr+ tr .text-right:nth-child(9) , .cb-plyr-tbl+ .cb-plyr-tbl tr+ tr .cb-plyr-tbody+ .text-right')#TI
      
      batsmen_data <- html_text(batsmen_data_html)
      
      bowler_data <- html_text(bowler_data_html)
      #------------------------Bowler------------------------------------------------------
      
      if(length(bowler_data) == 0){
        bowler_data <- c(0,0,0,0,0,0) 
      }
      if(length(batsmen_data) == 0){
        batsmen_data_data <- c(0,0,0,0,0,0) 
      }
 
      names(batsmen_data) <- batsmen_data_extract
      
      batsmen_data
      

      names(bowler_data) <- bowler_data_extract
      
      bowler_data
      
      
      #--------------------------------------Combining-------------------------------------
      final_data <- NULL
      final_data <- c(batsmen_data,bowler_data)
      
      final_data <- as.numeric(final_data)
      
      Player_Data[i, ] <- c(PlayerURLs$Name[i] , final_data)
      print(PlayerURLs$URL[i])
    }
      i = i + 1  
    }


#Ppl with all 

batsmen_data_html <- html_nodes(webpage,'.cb-col-100 .cb-plyr-thead tr:nth-child(4) .text-right:nth-child(14) , .cb-plyr-tbl:nth-child(2) tr:nth-child(4) .text-right:nth-child(13) , .cb-plyr-tbl:nth-child(2) tr:nth-child(4) .text-right:nth-child(9) , .cb-plyr-tbl:nth-child(2) tr:nth-child(4) .text-right:nth-child(7) , .cb-plyr-tbl:nth-child(2) tr:nth-child(4) .text-right:nth-child(5) , .cb-plyr-tbl:nth-child(2) tr:nth-child(4) .cb-plyr-tbody+ .text-right')#All

bowler_data_html <- html_nodes(webpage,'.cb-plyr-tbl+ .cb-plyr-tbl tr:nth-child(4) .text-right:nth-child(11) , .cb-plyr-tbl+ .cb-plyr-tbl tr:nth-child(4) .text-right:nth-child(10) , .cb-plyr-tbl+ .cb-plyr-tbl tr:nth-child(4) .text-right:nth-child(9) , .cb-plyr-tbl+ .cb-plyr-tbl tr:nth-child(4) .text-right:nth-child(6) , .cb-plyr-tbl+ .cb-plyr-tbl tr:nth-child(4) .text-right:nth-child(4) , .cb-plyr-tbl+ .cb-plyr-tbl tr:nth-child(4) .cb-plyr-tbody+ .text-right')#All



# Ppl with only ODI , T20 , and IPL
batsmen_data_html <- html_nodes(webpage,'tr~ tr+ tr .text-right:nth-child(14) , .cb-plyr-tbl:nth-child(2) tr~ tr+ tr .text-right:nth-child(13) , .cb-plyr-tbl:nth-child(2) tr~ tr+ tr .text-right:nth-child(9) , .cb-plyr-tbl:nth-child(2) tr~ tr+ tr .text-right:nth-child(7) , .cb-plyr-tbl:nth-child(2) tr~ tr+ tr .text-right:nth-child(5) , .cb-plyr-tbl:nth-child(2) tr~ tr+ tr .cb-plyr-tbody+ .text-right')#OTI

bowler_data_html <- html_nodes(webpage,'.cb-plyr-tbl+ .cb-plyr-tbl tr~ tr+ tr .text-right:nth-child(11) , .cb-plyr-tbl+ .cb-plyr-tbl tr~ tr+ tr .text-right:nth-child(10) , .cb-plyr-tbl+ .cb-plyr-tbl tr~ tr+ tr .text-right:nth-child(9) , .cb-plyr-tbl+ .cb-plyr-tbl tr~ tr+ tr .text-right:nth-child(4) , .cb-plyr-tbl+ .cb-plyr-tbl tr~ tr+ tr .text-right:nth-child(6) , .cb-plyr-tbl+ .cb-plyr-tbl tr~ tr+ tr .cb-plyr-tbody+ .text-right')#OTI

# Ppl with only  ODI, and IPL
batsmen_data_html <- html_nodes(webpage,'.cb-plyr-tbl:nth-child(2) tr+ tr .text-right:nth-child(13) , tr+ tr .text-right:nth-child(14) , .cb-plyr-tbl:nth-child(2) tr+ tr .text-right:nth-child(9) , .cb-plyr-tbl:nth-child(2) tr+ tr .text-right:nth-child(7) , .cb-plyr-tbl:nth-child(2) tr+ tr .text-right:nth-child(5) , .cb-plyr-tbl:nth-child(2) tr+ tr .cb-plyr-tbody+ .text-right') #OI

bowler_data_html <- html_nodes(webpage,'.cb-plyr-tbl+ .cb-plyr-tbl tr+ tr .text-right:nth-child(10) , .cb-plyr-tbl+ .cb-plyr-tbl tr+ tr .text-right:nth-child(11) , .cb-plyr-tbl+ .cb-plyr-tbl tr+ tr .text-right:nth-child(9) , .cb-plyr-tbl+ .cb-plyr-tbl tr+ tr .text-right:nth-child(6) , .cb-plyr-tbl+ .cb-plyr-tbl tr+ tr .text-right:nth-child(4) , .cb-plyr-tbl+ .cb-plyr-tbl tr+ tr .cb-plyr-tbody+ .text-right')#OI



# Ppl with only T20 , and IPL
batsmen_data_html <- html_nodes(webpage,'tr+ tr .text-right:nth-child(14) , .cb-plyr-tbl:nth-child(2) tr+ tr .text-right:nth-child(13) , .cb-plyr-tbl:nth-child(2) tr+ tr .text-right:nth-child(9) , .cb-plyr-tbl:nth-child(2) tr+ tr .text-right:nth-child(7) , .cb-plyr-tbl:nth-child(2) tr+ tr .text-right:nth-child(5) , .cb-plyr-tbl:nth-child(2) tr+ tr .cb-plyr-tbody+ .text-right')#TI

bowler_data_html <- html_nodes(webpage,'.cb-plyr-tbl+ .cb-plyr-tbl tr+ tr .text-right:nth-child(6) , .cb-plyr-tbl+ .cb-plyr-tbl tr+ tr .text-right:nth-child(11) , .cb-plyr-tbl+ .cb-plyr-tbl tr+ tr .text-right:nth-child(10) , .cb-plyr-tbl+ .cb-plyr-tbl tr+ tr .text-right:nth-child(4) , .cb-plyr-tbl+ .cb-plyr-tbl tr+ tr .text-right:nth-child(9) , .cb-plyr-tbl+ .cb-plyr-tbl tr+ tr .cb-plyr-tbody+ .text-right')#TI


#Ppl with only ipl 

batsmen_data_html <- html_nodes(webpage,'td:nth-child(14) , .cb-plyr-tbl:nth-child(2) td:nth-child(13) , .cb-plyr-tbl:nth-child(2) td:nth-child(9) , .cb-plyr-tbl:nth-child(2) td:nth-child(7) , .cb-plyr-tbl:nth-child(2) td:nth-child(5) , .cb-plyr-tbl:nth-child(2) .cb-plyr-tbody+ .text-right')#I

bowler_data_html <- html_nodes(webpage,'.cb-plyr-tbl+ .cb-plyr-tbl td:nth-child(11) , .cb-plyr-tbl+ .cb-plyr-tbl td:nth-child(10) , .cb-plyr-tbl+ .cb-plyr-tbl td:nth-child(9) , .cb-plyr-tbl+ .cb-plyr-tbl td:nth-child(6) , .cb-plyr-tbl+ .cb-plyr-tbl td:nth-child(4) , .cb-plyr-tbl+ .cb-plyr-tbl .cb-plyr-tbody+ .text-right')#I
