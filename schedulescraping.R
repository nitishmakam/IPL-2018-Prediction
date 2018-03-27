library(rvest)
teams<-c('TBC','Chennai Super Kings','Delhi Daredevils','Royal Challengers Bangalore','Kolkata Knight Riders','Sunrisers Hyderabad','Rajasthan Royals','Kings XI Punjab','Mumbai Indians')
schedule<-data.frame(matchno=integer(60),time=character(60),hometeam=character(60),awayteam=character(60),stadium=character(60),location=character(60),stringsAsFactors = FALSE)
levels(schedule$hometeam)<-teams
levels(schedule$awayteam)<-teams
url<-'http://www.iplt20.com/schedule'
for(i in 1:60)
{
  schedule$matchno[i]=i
}
webpage<-read_html(url)
fixtureteams<-html_text(html_nodes(webpage,'.fixture__team-name'))
for(i in seq(1,119,2))
{
  schedule$hometeam[i/2+1]<-fixtureteams[i]
  schedule$awayteam[i/2+1]<-fixtureteams[i+1]
}
fixturesinfo<-html_text(html_nodes(webpage,'.fixture__info'))
for(i in 1:60)
{
  matchinfo<-fixturesinfo[i]
  matchinfo<-gsub('[\n\t\r]|^\\s+|\\s+$','',matchinfo)
  matchinfo<-strsplit(matchinfo,',')
  schedule$stadium[i]<-trimws(matchinfo[[1]][3],"l")
  schedule$location[i]<-trimws(matchinfo[[1]][4],"l")
  time<-strsplit(matchinfo[[1]][2],' ')
  schedule$time[i]<-trimws(time[[1]][2],"l")
}
