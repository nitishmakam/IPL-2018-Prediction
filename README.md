# IPL-2018-Prediction
This repository contains code and the csv files that were used to predict various aspectcs of IPL 2018 - done as part of Cisco 3D Hackathon held at PES University in March 2018.
It also contains R code that were used to scrape data from websites such as - iplt20.com for the schedule, cricbuzz for player stats.
IPL data for the past 10 seasons was taken from kaggle.

This project was to predict the champions of the 2018 IPL,mid-season and end-season points table,orange and purple cap winners.

So, in order to achieve this, we simulated each ball of the entire IPL 2018 season by initially getting the probabilities of occurrence of an event on a ball for each pair of a batsman and a bowler.
We then cumulated the probabilities and recorded them. The possible events on each ball were - scoring 0,1,2,3,4,5,6 runs and the probability of getting a wicket.
So we generate two random integers for each particular ball, one for the batsman - for the runs and one for the bowler - for the wicket.
Whichever interval the random integer fell into, that was the event that was taken to be occurring on that particular ball.
So each ball was simulated like this and all the stats were recorded.

With all this done, we predicted the champions of IPL, mid and end season points table, and the orange and purple cap winners.

Predicted Champions - RCB
Orange Cap - Ajinkya Rahane
Purple Cap - Imran Tahir
