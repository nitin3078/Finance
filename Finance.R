

setwd("C:\\Users\\nitin\\Desktop\\SMA")

library(tidyverse)
library(ggplot2)



######### Section 1

spy <- read.csv("SPY.csv")

str(spy)
summary(spy)

# change Date column as date object
spy$Date<- as.Date(spy$Date)

# sort Dataframe by date
spy<- spy[(order(spy$Date)),]

# calculate return
spy <- spy %>% mutate(return = (spy$Close - spy$Open)/spy$Open)

# cumulative return
spy$cumsum <- cumsum(spy[,14])

# plot
ggplot(spy, aes(Date, cumsum)) + geom_point(col="blue") + geom_line(col="blue")+scale_x_date() +
  labs(title="cummulative return for spy",
       x ="Year", y = "Cumulative return")


# Same procedure with TSLA

tsla <- read.csv("TSLA.csv")

str(tsla)
summary(tsla)

# change Date column as date object
tsla$Date<- as.Date(tsla$Date)

# sort Dataframe by date
tsla<- tsla[(order(tsla$Date)),]

# calculate return
tsla <- tsla %>% mutate(return = (tsla$Close - tsla$Open)/tsla$Open)

# cumulative return
tsla$cumsum <- cumsum(tsla[,14])

# plot
ggplot(tsla, aes(Date, cumsum)) + geom_point(col="green") + geom_line(col="green")+ scale_x_date() +
  labs(title="cummulative return for tsla",
       x ="Year", y = "Cumulative return")

# superimposing both results
ggplot(spy, aes(Date, cumsum)) + geom_point(col="blue") + geom_line(col="blue")+scale_x_date() +
  geom_point(data = tsla, color = "green") + geom_line(data=tsla,col="green") +
  labs(title="Comparison of cummulative return-  spy (blue) and tsla (green)",
       x ="Year", y = "Cumulative return")
  


#### Section 2

score <- read.table("TSLA_score.txt",sep = '\t',header = TRUE)

score$date<- as.Date(score$date)

# create a new dataframe where score >=2
score2 <- score %>% filter(s.score>=2)

# filtering tsla dataset for dates where score>=2
tsla2<-tsla

# dropping original cumsum and return columns
tsla2<-tsla2[,-c(14,15)]

tsla2$return <- ifelse(tsla2$Date %in% score2$date,(tsla2$Close-tsla2$Open)/tsla2$Open,0) 

#Calculate cummulative return

tsla2$cumsum <- cumsum(tsla2[,14])

# plot when s-score >=2

ggplot(tsla2, aes(Date, cumsum)) + geom_point(col="green") + geom_line(col="green")+ scale_x_date() +
  labs(title="cummulative return for tsla when s-score >=2",
       x ="Year", y = "Cumulative return")


# Same analysis for s-score <=-2

# create a new dataframe where score >=2
score3 <- score %>% filter(s.score<=-2)

# filtering tsla dataset for dates where score>=2
tsla3<-tsla


tsla3<-tsla3[,-c(14,15)]

tsla3$return <- ifelse(tsla3$Date %in% score3$date,(tsla3$Close-tsla3$Open)/tsla3$Open,0) 

#Calculate cummulative return

tsla3$cumsum <- cumsum(tsla3[,14])

# plot when s-score >=2

ggplot(tsla3, aes(Date, cumsum)) + geom_point(col="green") + geom_line(col="green")+ scale_x_date() +
  labs(title="cummulative return for tsla when s-score <=-2",
       x ="Year", y = "Cumulative return")



# section 3 

# overlay 4 and 5


ggplot(tsla2, aes(Date, cumsum)) + geom_point(col="green") + geom_line(col="green")+ scale_x_date() +
  labs(title="comparison between s-score <=-2(blue) and s-score>=2(green) for cummulative return (tsla)",
       x ="Year", y = "Cumulative return") +
  geom_point(data=tsla3,col="blue") + geom_line(data=tsla3,col="blue")


# overlay 1,2 and 4

ggplot(spy, aes(Date, cumsum)) + geom_point(col="blue") + geom_line(col="blue")+scale_x_date() +
  geom_point(data = tsla, color = "green") + geom_line(data=tsla,col="green") +
  labs(title="Comparison of cummulative return-  spy (blue), tsla (green) and tsla(red) for s-score>=2",
       x ="Year", y = "Cumulative return") +
  geom_point(data=tsla2,col="red") + geom_line(data=tsla2,col="red")
  



# histogram of s-score and summary statistics

ggplot(score, aes(s.score)) + geom_histogram(breaks=seq(-4.3, 4.3, by =.1), fill="blue") +
  labs(title="Histogram for s-score") +
  labs(x="s-score", y="Count")

summary(score$s.score)


# Cumulative return series for 5 market days

tsla4<- tsla

tsla4 <- 
  tsla4 %>%
  mutate(five_day_close = dplyr::lead(Close, n = 5, default = 0))

# Remove last 5 rows from analysis

n<-dim(tsla4)[1]
tsla4<-tsla4[1:(n-5),]

# dropping original cumsum and return columns
tsla4<-tsla4[,-c(14,15)]


# calculatin cummulative return series for 5 market day

# calculate return
tsla4 <- tsla4 %>% mutate(return = (tsla4$five_day_close - tsla4$Open)/tsla4$Open)

# cumulative return
tsla4$cumsum <- cumsum(tsla4[,15])

# plot
ggplot(tsla4, aes(Date, cumsum)) + geom_point(col="green") + geom_line(col="green")+ scale_x_date() +
  labs(title="cummulative return 5 market day for tsla",
       x ="Year", y = "Cumulative return")

