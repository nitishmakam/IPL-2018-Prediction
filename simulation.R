load("new_prob.Rda")
pointstable<-data.frame(matrix(ncol=5,nrow=8),stringsAsFactors = FALSE)
teamnames<-c('CSK','DD','KKR','KXIP','MI','RCB','RR','SRH')
colnames(pointstable)<-c('Team','Won','Lost','Points','NRR')
pointstable$Team<-teamnames
pointstable$Won<-integer(8)
pointstable$Lost<-integer(8)
pointstable$Points<-integer(8)
#x<-.Random.seed
set.seed(18)
pointstable$NRR<-c(0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0)
filenames<-c('CSK.csv','DD.csv','KKR.csv','KXIP.csv','MI.csv','RCB.csv','RR.csv','SRH.csv')
players<-character(0)
#set.seed(31)

for(i in 1:length(filenames))
{
  file<-read.csv(filenames[i],stringsAsFactors = FALSE,header=TRUE)
  #append(batsmen,file$Player.name)
  players<-c(players,file$Player.name)
}
playerstats<-data.frame(matrix(ncol=3,nrow=88),stringsAsFactors = FALSE)
colnames(playerstats)<-c('Name','Runs','Wickets')
playerstats$Name<-players
playerstats$Runs<-integer(88)
playerstats$Wickets<-integer(88)
schedule<-read.csv('schedule.csv',header=TRUE,stringsAsFactors = FALSE)
CSK<-read.csv("CSK.csv",header=TRUE,stringsAsFactors = FALSE)
DD<-read.csv("DD.csv",header=TRUE,stringsAsFactors = FALSE)
KKR<-read.csv("KKR.csv",header=TRUE,stringsAsFactors = FALSE)
KXIP<-read.csv("KXIP.csv",header=TRUE,stringsAsFactors = FALSE)
MI<-read.csv("MI.csv",header=TRUE,stringsAsFactors = FALSE)
RR<-read.csv("RR.csv",header=TRUE,stringsAsFactors = FALSE)
RCB<-read.csv("RCB.csv",header=TRUE,stringsAsFactors = FALSE)
SRH<-read.csv("SRH.csv",header=TRUE,stringsAsFactors = FALSE)

team<-data.frame(matrix(ncol=2,nrow=8),stringsAsFactors = FALSE)
colnames(team)<-c('name','players')
team$name[1]<-'CSK'
team$name[2]<-'DD'
team$name[3]<-'KKR'
team$name[4]<-'KXIP'
team$name[5]<-'MI'
team$name[6]<-'RR'
team$name[7]<-'RCB'
team$name[8]<-'SRH'
team$players[1]<-list(CSK$Player.name)
team$players[2]<-list(DD$Player.name)
team$players[3]<-list(KKR$Player.name)
team$players[4]<-list(KXIP$Player.name)
team$players[5]<-list(MI$Player.name)
team$players[6]<-list(RR$Player.name)
team$players[7]<-list(RCB$Player.name)
team$players[8]<-list(SRH$Player.name)

#results<-data.frame(matchno=numeric(60),hometeam=character(60),awayteam=character(60),winningteam=character(0),stringsAsFactors = FALSE)
results<-data.frame(matrix(ncol=4,nrow=60))
colnames(results)<-c('matchno','hometeam','awayteam','winningteam')
simulate <- function(team1,team2){
  #assign("playerstats",playerstats,.GlobalEnv)
 bowlers1<-team1[7:11]
score1<-0
wickets1<-0
striker<-team1[1]
nonstriker<-team1[2]
batindex<-2
bowler<-team2[11]
for(i in 1:20)
{
  bowler<-team2[7+i%%5]
  for(j in 1:6)
  {
    r1<-sample(1:1000,1)
    r2<-sample(1:1000,1)
    ranges<-new_prob[striker,bowler]
    if(is.na(ranges) | is.null(ranges) || length(ranges)==1)
    {
      probs<-c(406,775,840,844,957,957,1000,50)
    }
    else
    {
      probs<-as.numeric(unlist(ranges))
    }
    if(r2<probs[8]){
      wickets1<-wickets1+1
      playerstats$Wickets[which(playerstats$Name==bowler)]<<-playerstats$Wickets[which(playerstats$Name==bowler)]+1
      if(wickets1>=10){
        return (c(score1,wickets1))
      }
      striker<-team1[batindex+1]
      batindex<-batindex+1
    }
    for(l in 1:(length(probs)-1))
    {
      if(r1<as.integer(probs[l]))
      {
        runs<-l-1
        score1<-score1+runs
        playerstats$Runs[which(playerstats$Name==striker)]<<-playerstats$Runs[which(playerstats$Name==striker)]+runs
        if(runs%%2==1)
        {
          temp<-nonstriker
          nonstriker<-striker
          striker<-temp
        }
        break
      }
    }
    
  }
  temp<-nonstriker
  nonstriker<-striker
  striker<-temp
  
}
return (c(score1,wickets1))
}

for(match in 1:56)
{
  results$matchno[match]<-match
  hometeam<-schedule$hometeam[match]
  awayteam<-schedule$awayteam[match]
  results$hometeam[match]<-hometeam
  results$awayteam[match]<-awayteam
  toss<-sample(0:1,1)
  if(toss==0)
  {
    teamno1<-as.vector(unlist(team$players[which(team$name==hometeam)]))
    teamno2<-as.vector(unlist(team$players[which(team$name==awayteam)]))
    batting1team<-hometeam
    batting2team<-awayteam
  }
  else
  {
    teamno2<-as.vector(unlist(team$players[which(team$name==hometeam)]))
    teamno1<-as.vector(unlist(team$players[which(team$name==awayteam)]))
    batting1team<-awayteam
    batting2team<-hometeam
  }
  team1scores<-simulate(teamno1,teamno2)
  team2scores<-simulate(teamno2,teamno1)
  rr1<-((team1scores[1])/20)
  rr2<-((team2scores[1])/20)
  team1games<-as.numeric(pointstable[which(pointstable$Team==batting1team),'Won']) + as.numeric(pointstable[which(pointstable$Team==batting1team),'Lost'])
  team2games<-as.numeric(pointstable[which(pointstable$Team==batting2team),'Won']) + as.numeric(pointstable[which(pointstable$Team==batting2team),'Lost'])
  nrr1<-as.numeric(pointstable[which(pointstable$Team==batting1team),'NRR'])
  nrr2<-as.numeric(pointstable[which(pointstable$Team==batting2team),'NRR'])
  pointstable[which(pointstable$Team==batting1team),'NRR']<-as.numeric(((nrr1)+(rr1-rr2)/10)/(team1games+1))
  pointstable[which(pointstable$Team==batting2team),'NRR']<-as.numeric(((nrr2)+(rr2-rr1)/10)/(team2games+1))
  if(team1scores[1]<team2scores[1])
  {
    results$winningteam[match]<-batting2team
  }
  else if(team1scores[1]>team2scores[1])
  {
    results$winningteam[match]<-batting1team
  }
  else
  {
    if(team1scores[2]<team2scores[2])
    {
      #results$winningteam[match]<-team$name[which(teamno1==as.vector(unlist(team$players)))]
      results$winningteam[match]<-batting1team
    }
    else
    {
      #results$winningteam[match]<-team$name[which(teamno2==as.vector(unlist(team$players)))] 
      results$winningteam[match]<-batting2team
    }
        
  }
  
}

write.csv(results,'results.csv',row.names = FALSE)

for(m in 1:56)
{
  if(results$winningteam[m]==results$hometeam[m])
  {    winteam<-results$hometeam[m]
      loseteam<-results$awayteam[m]
  }
  else
  {
    winteam<-results$awayteam[m]
    loseteam<-results$hometeam[m]
  }
  pointstable$Won[which(pointstable$Team==winteam)]<-pointstable$Won[which(pointstable$Team==winteam)]+1
  pointstable$Lost[which(pointstable$Team==loseteam)]<-pointstable$Lost[which(pointstable$Team==loseteam)]+1
  pointstable$Points[which(pointstable$Team==winteam)]<-pointstable$Points[which(pointstable$Team==winteam)]+2
  if(m==28)
  {
    pointstablemidway<-pointstable[order(-pointstable$Points,-pointstable$NRR),]
    write.csv(pointstablemidway,'pointstablemidway.csv',row.names = FALSE)
  }
}
write.csv(pointstable,'PointsTable.csv',row.names=FALSE)
write.csv(playerstats,'PlayerStats.csv',row.names=FALSE)
print(max(playerstats$Runs))
print(playerstats$Name[which(playerstats$Runs==max(playerstats$Runs))])
print(max(playerstats$Wickets))
purplecap<-playerstats$Name[which(playerstats$Wickets==max(playerstats$Wickets))]
print(purplecap[1])
pointstable<-pointstable[order(-pointstable$Points,-pointstable$NRR),]
write.csv(pointstable,'pointstable.csv',row.names=FALSE)

schedule[57,'hometeam']<-pointstable$Team[1]
schedule[57,'awayteam']<-pointstable$Team[2]
schedule[58,'hometeam']<-pointstable$Team[3]
schedule[58,'awayteam']<-pointstable$Team[4]

#QUALIFIER 1 and ELIMINATOR
for(match in 57:58)
{
  results$matchno[match]<-match
  hometeam<-schedule$hometeam[match]
  awayteam<-schedule$awayteam[match]
  results$hometeam[match]<-hometeam
  results$awayteam[match]<-awayteam
  toss<-sample(0:1,1)
  if(toss==0)
  {
    teamno1<-as.vector(unlist(team$players[which(team$name==hometeam)]))
    teamno2<-as.vector(unlist(team$players[which(team$name==awayteam)]))
    batting1team<-hometeam
    batting2team<-awayteam
  }
  else
  {
    teamno2<-as.vector(unlist(team$players[which(team$name==hometeam)]))
    teamno1<-as.vector(unlist(team$players[which(team$name==awayteam)]))
    batting1team<-awayteam
    batting2team<-hometeam
  }
  team1scores<-simulate(teamno1,teamno2)
  team2scores<-simulate(teamno2,teamno1)
  rr1<-((team1scores[1])/20)
  rr2<-((team2scores[1])/20)
  team1games<-as.numeric(pointstable[which(pointstable$Team==batting1team),'Won']) + as.numeric(pointstable[which(pointstable$Team==batting1team),'Lost'])
  team2games<-as.numeric(pointstable[which(pointstable$Team==batting2team),'Won']) + as.numeric(pointstable[which(pointstable$Team==batting2team),'Lost'])
  nrr1<-as.numeric(pointstable[which(pointstable$Team==batting1team),'NRR'])
  nrr2<-as.numeric(pointstable[which(pointstable$Team==batting2team),'NRR'])
  pointstable[which(pointstable$Team==batting1team),'NRR']<-as.numeric(((nrr1)+(rr1-rr2)/10)/(team1games+1))
  pointstable[which(pointstable$Team==batting2team),'NRR']<-as.numeric(((nrr2)+(rr2-rr1)/10)/(team2games+1))
  if(team1scores[1]<team2scores[1])
  {
    results$winningteam[match]<-batting2team
  }
  else if(team1scores[1]>team2scores[1])
  {
    results$winningteam[match]<-batting1team
  }
  else
  {
    if(team1scores[2]<team2scores[2])
    {
      #results$winningteam[match]<-team$name[which(teamno1==as.vector(unlist(team$players)))]
      results$winningteam[match]<-batting1team
    }
    else
    {
      #results$winningteam[match]<-team$name[which(teamno2==as.vector(unlist(team$players)))] 
      results$winningteam[match]<-batting2team
    }
    
  }
  
}

if (results$winningteam[57]==results$hometeam[57]){
  schedule[59,'hometeam']<-results$awayteam[57]
}  
if(results$winningteam[57]!=results$hometeam[57]){
  schedule[59,'hometeam']<-results$hometeam[57]
}
  
schedule[59,'awayteam']<-results$winningteam[58]

#Qualifier 2
for(match in 59)
{
  results$matchno[match]<-match
  hometeam<-schedule$hometeam[match]
  awayteam<-schedule$awayteam[match]
  results$hometeam[match]<-hometeam
  results$awayteam[match]<-awayteam
  toss<-sample(0:1,1)
  if(toss==0)
  {
    teamno1<-as.vector(unlist(team$players[which(team$name==hometeam)]))
    teamno2<-as.vector(unlist(team$players[which(team$name==awayteam)]))
    batting1team<-hometeam
    batting2team<-awayteam
  }
  else
  {
    teamno2<-as.vector(unlist(team$players[which(team$name==hometeam)]))
    teamno1<-as.vector(unlist(team$players[which(team$name==awayteam)]))
    batting1team<-awayteam
    batting2team<-hometeam
  }
  team1scores<-simulate(teamno1,teamno2)
  team2scores<-simulate(teamno2,teamno1)
  rr1<-((team1scores[1])/20)
  rr2<-((team2scores[1])/20)
  team1games<-as.numeric(pointstable[which(pointstable$Team==batting1team),'Won']) + as.numeric(pointstable[which(pointstable$Team==batting1team),'Lost'])
  team2games<-as.numeric(pointstable[which(pointstable$Team==batting2team),'Won']) + as.numeric(pointstable[which(pointstable$Team==batting2team),'Lost'])
  nrr1<-as.numeric(pointstable[which(pointstable$Team==batting1team),'NRR'])
  nrr2<-as.numeric(pointstable[which(pointstable$Team==batting2team),'NRR'])
  pointstable[which(pointstable$Team==batting1team),'NRR']<-as.numeric(((nrr1)+(rr1-rr2)/10)/(team1games+1))
  pointstable[which(pointstable$Team==batting2team),'NRR']<-as.numeric(((nrr2)+(rr2-rr1)/10)/(team2games+1))
  if(team1scores[1]<team2scores[1])
  {
    results$winningteam[match]<-batting2team
  }
  else if(team1scores[1]>team2scores[1])
  {
    results$winningteam[match]<-batting1team
  }
  else
  {
    if(team1scores[2]<team2scores[2])
    {
      #results$winningteam[match]<-team$name[which(teamno1==as.vector(unlist(team$players)))]
      results$winningteam[match]<-batting1team
    }
    else
    {
      #results$winningteam[match]<-team$name[which(teamno2==as.vector(unlist(team$players)))] 
      results$winningteam[match]<-batting2team
    }
    
  }
  
}

schedule[60,'hometeam']<-results$winningteam[57]
schedule[60,'awayteam']<-results$winningteam[59]
for(match in 60)
{
  results$matchno[match]<-match
  hometeam<-schedule$hometeam[match]
  awayteam<-schedule$awayteam[match]
  results$hometeam[match]<-hometeam
  results$awayteam[match]<-awayteam
  toss<-sample(0:1,1)
  if(toss==0)
  {
    teamno1<-as.vector(unlist(team$players[which(team$name==hometeam)]))
    teamno2<-as.vector(unlist(team$players[which(team$name==awayteam)]))
    batting1team<-hometeam
    batting2team<-awayteam
  }
  else
  {
    teamno2<-as.vector(unlist(team$players[which(team$name==hometeam)]))
    teamno1<-as.vector(unlist(team$players[which(team$name==awayteam)]))
    batting1team<-awayteam
    batting2team<-hometeam
  }
  team1scores<-simulate(teamno1,teamno2)
  team2scores<-simulate(teamno2,teamno1)
  rr1<-((team1scores[1])/20)
  rr2<-((team2scores[1])/20)
  team1games<-as.numeric(pointstable[which(pointstable$Team==batting1team),'Won']) + as.numeric(pointstable[which(pointstable$Team==batting1team),'Lost'])
  team2games<-as.numeric(pointstable[which(pointstable$Team==batting2team),'Won']) + as.numeric(pointstable[which(pointstable$Team==batting2team),'Lost'])
  nrr1<-as.numeric(pointstable[which(pointstable$Team==batting1team),'NRR'])
  nrr2<-as.numeric(pointstable[which(pointstable$Team==batting2team),'NRR'])
  pointstable[which(pointstable$Team==batting1team),'NRR']<-as.numeric(((nrr1)+(rr1-rr2)/10)/(team1games+1))
  pointstable[which(pointstable$Team==batting2team),'NRR']<-as.numeric(((nrr2)+(rr2-rr1)/10)/(team2games+1))
  if(team1scores[1]<team2scores[1])
  {
    results$winningteam[match]<-batting2team
  }
  else if(team1scores[1]>team2scores[1])
  {
    results$winningteam[match]<-batting1team
  }
  else
  {
    if(team1scores[2]<team2scores[2])
    {
      #results$winningteam[match]<-team$name[which(teamno1==as.vector(unlist(team$players)))]
      results$winningteam[match]<-batting1team
    }
    else
    {
      #results$winningteam[match]<-team$name[which(teamno2==as.vector(unlist(team$players)))] 
      results$winningteam[match]<-batting2team
    }
    
  }
  
}

print(results$winningteam[60])
