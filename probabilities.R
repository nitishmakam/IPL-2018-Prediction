filenames<-c('CSK.csv','DD.csv','KKR.csv','KXIP.csv','MI.csv','RCB.csv','RR.csv','SRH.csv')
bowlers<-character(0)
batsmen<-character(0)
for(i in 1:length(filenames))
{
  file<-read.csv(filenames[i],stringsAsFactors = FALSE,header=TRUE)
  #append(batsmen,file$Player.name)
  batsmen<-c(batsmen,file$Player.name)
  bowlers<-c(bowlers,tail(file$Player.name,5))
}
prob<-data.frame(matrix(ncol=length(bowlers),nrow=length(batsmen)))
rownames(prob)<-batsmen
colnames(prob)<-bowlers

deliveries<-read.csv("deliveries.csv",header=TRUE,stringsAsFactors = FALSE)
for(i in batsmen)
{
  for(j in bowlers)
  {
    current<-subset(deliveries,batsman==i & bowler==j,select=c('batsman_runs','player_dismissed'))
    if(nrow(current)==0)
    {
      next
    }
    probabilities<-c()
    for(k in 0:6)
    {
      probabilities<-c(probabilities,((length(which(current$batsman_runs==k))/nrow(current))*1000))
    }
    probabilities<-c(probabilities,round((length(which(current$player_dismissed==i))/nrow(current)*1000),digits=0))
    for(l in 2:(length(probabilities)-1))
    {
      probabilities[l]<-round((probabilities[l]+probabilities[l-1]),digits=0)
    }
    prob[i,j][[1]]<-list(probabilities)
  }
}
