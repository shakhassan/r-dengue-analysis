#############
#INTRODUCTION
#############

#built-in constants
letters
month.name
month.abb
pi
#built-in functions
sqrt(64)
round(3.141593, digits=2)
strsplit("monas/h", "/")
toupper("hello world")
paste("Today is", "")
#user input
my.name <- readline(prompt="Enter your name: ")
#others
help.search("zoo")
?<keyword> #eg : ?axis
getwd()

###################################
#HANDS-ON EXERCISE: DENGUE DATA SET
###################################

#ThingsToKnowBeforeProceedToExercise
#setRepositories()
#install.packages("PackageNameCaseSensitive")
#library("PackageNameCaseSensitive")

#skip
#dengue_data = readLines("Jumlah kes denggi 2011_wheader_formatted.csv")
#dengue_data = dengue_data[-1,3:54]
#dengue_data = read.csv(textConnection(dengue_data), header = TRUE, stringsAsFactors = FALSE)

#SET working directory
setwd("/Users/rjirwanshah/Desktop/Monash/Case Study - Dengue")
getwd()

#READ csv.
dengue_data=read.csv("Dengue_Cases_Malaysia_2011.csv", header = TRUE, sep = ",")

class(dengue_data)

#GET dimension of the data frame - number of rows (1st) & columns (2nd)
dim(dengue_data)

#GET column names
names(dengue_data)

#other commands :
#information about the structure of some object
str(dengue_data)

#GET summary of each column that shows minimum, maximum, missing values, and the stuff in between
summary(dengue_data)

#1 - CALCULATE total of dengue cases in each state 
#and add new column to the data.frame(dengue_data)

weekly_statistic <- dengue_data[3:length(dengue_data)]
total_cases_each_state <- transform(dengue_data, SUM = rowSums(weekly_statistic))
#note :
 #(1) dengue_data[3:length(dengue_data)] also can be written as dengue_data[3:54]
 #(2) dengue_data[3:length(dengue_data)] - start at "3" (array) to exclude non-numeric column : NEGERI, Year

#2- CALCULATE total dengue cases in Malaysia for the year 2011
total_cases_malaysia <- sum(total_cases_each_state$SUM)

#3- CALCULATE the highest case across all weeks and states
max(weekly_statistic, na.rm=TRUE)
#note : also can be written as max(dengue_data[3:length(dengue_data)], na.rm=TRUE)

#4- CALCULATE the highest case for each week across all states
#number = dimension of data:
highest_each_week <- apply(weekly_statistic, 2, max)
#note : apply(weekly_statistic, 2, max) also can be written as sapply(weekly_statistic, max)

#5- CALCULATE the highest case for each state across all weeks
highest_each_state <- apply(weekly_statistic, 1, max)
#note: also can try : highest_each_state <- transform(apply(dengue_data[3:54], 1, max))

#6- CALCULATE the average of dengue cases for each state for the whole year
rowMeans(weekly_statistic, na.rm = TRUE)

#7- plot : to visualize total dengue cases in each state for the year 2011
#note : xaxt = axis numbering, ann = axis labels
plot(total_cases_each_state$SUM,main="Dengue Cases in Malaysia : 2011", ylab="Total Cases", xlab="Negeri", xaxt='n', ann=FALSE)
axis(1, at=1:15, labels=total_cases_each_state$NEGERI, ylim=c(0,10000), las=2)
title(main="Dengue Cases in Malaysia : 2011", ylab="Total Cases")

#optional :
text(total_cases_each_state$SUM, labels=total_cases_each_state$SUM, cex=0.4, pos=2, col="red")

#barplot : to visualize total dengue cases in each state for the year 2011
#par(mai=c(2,2,4,2))
barplot(total_cases_each_state$SUM, main="Dengue Cases in Malaysia : 2011", ylab="Total Cases", names.arg=total_cases_each_state$NEGERI, ylim=c(0,10000), las=2)

#ggplot : to visualize total dengue cases in each state for the year 2011
#par(mai=c(0,0,8,0))
q <- ggplot(data = total_cases_each_state, main="Dengue Cases in Malaysia : 2011", mapping = aes(x = total_cases_each_state$NEGERI, y = total_cases_each_state$SUM)) + geom_point(color="red")
q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.title.x = element_blank())
q <- q + labs(y="Total Cases", title="Dengue Cases in Malaysia : 2011")
q

#change column name "Minggu week #" to week #
colnames(dengue_data)[3:54] <- c("01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52")
#remove Year column
dengue_data$Year <- NULL

#transpose
matrx2 <- dengue_data
matrx2_transpose <- t(matrx2)
matrx2_dataframe_transpose <- as.data.frame(matrx2_transpose)
write.table(matrx2_dataframe_transpose,file="matrx2_transpose.csv",sep=",",col.names=F)
matrx2_data_frame <- read.csv("matrx2_transpose.csv", header=TRUE, dec=".",sep=",")

#plot weekly data using basic plot()
plot(matrx2_data_frame$SELANGOR,main="Dengue Cases in SELANGOR : 2011", ylab="Total Cases", xlab="NEGERI", xaxt='n', ann=FALSE,type='l')
axis(1, at=seq(2, 52, by = 2), ylim=c(0,300), las=2)
title(main="Dengue Cases in SELANGOR : 2011", ylab="Total Cases", xlab="Week")

#png(file="k.png")

#plot subset (define range of week) of weekly data using basic plot()
plot(matrx2_data_frame$SELANGOR, xaxt='n', ann=FALSE,type='l', xlim=range(1:10))
axis(1, at=1:52, labels=matrx2_data_frame$NEGERI, ylim=c(0,300), las=2)
title(main="Dengue Cases in SELANGOR : Week #1-#10 2011", ylab="Total Cases", xlab="Week")

#plot weekly data for Selangor using ggplot()
q <- ggplot(data = matrx2_data_frame, main="Dengue Cases in SELANGOR : 2011", mapping = aes(x = matrx2_data_frame$NEGERI, y = matrx2_data_frame$SELANGOR)) + geom_line(color="red")
q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.title.x = element_blank())
q <- q + labs(y="Total Cases",x = "Week",title="Dengue Cases in SELANGOR : 2011")
q

#practice : try geom_point(),
ggsave("dengue_cases_selangor_2011.png")

#qplot : simpler version of ggplot()
p <- qplot(matrx2_data_frame$NEGERI, matrx2_data_frame$SELANGOR, data=matrx2_data_frame, geom="line")
p + labs(x = "Week", y="Total Cases", title="Dengue Cases in SELANGOR : 2011")
dev.print(p, "dengue_cases_selangor_2011.jpeg")

#change week number to date
matrx2_data_frame$NEGERI <- as.Date(paste(2011, matrx2_data_frame$NEGERI, 1, sep="-"), "%Y-%U-%u")

#aggreagate weekly data in each state to be monthly
library(zoo)
library(xts)
x.zoo <- read.zoo(matrx2_data_frame, header = T)
matrx2_data_frame_agg <- apply.monthly(x.zoo, colSums)
class(matrx2_data_frame_agg)
#plotting zoo object - PERLIS
autoplot(matrx2_data_frame_agg[,1], geom = "point")

plot(matrx2_data_frame_agg[,1], xaxt='n', ann=FALSE,type='l')
axis(1, at=1:12, labels=matrx2_data_frame_agg[, 0], ylim=c(0,300), las=2)
title(main="Dengue Cases in SELANGOR : 2011", ylab="Total Cases", xlab="Month")

#install.packages("lubridate")
library(lubridate)
#tapply(matrx2_data_frame$SELANGOR, week(matrx2_data_frame$NEGERI), mean)
weeks <- month(matrx2_data_frame_agg[,1])
sums <- tapply(matrx2_data_frame_agg[,2], weeks, sum)
#option : access data frame matrx2_data_frame
weeks <- month(matrx2_data_frame$NEGERI)
sums <- tapply(matrx2_data_frame$SELANGOR, weeks, sum)

library(ggplot2)
library(scales)
#as.Date(paste(2011, matrx2_data_frame$NEGERI, 1, sep="-"), "%Y-%U-%u")
matrx2_data_frame$Month <- as.Date(cut(matrx2_data_frame$NEGERI,breaks = "month"))
matrx2_data_frame$Week <- as.Date(cut(matrx2_data_frame$NEGERI,breaks = "week",start.on.monday = TRUE))
ggplot(data = matrx2_data_frame, aes(Month, matrx2_data_frame$SELANGOR)) + stat_summary(fun.y = sum, geom = "line", color="red") + scale_x_date(labels = date_format("%Y-%m"), date_breaks = "1 month")



#multiplot with Rmisc: plot SELANGOR & PUTRAJAYA on the same page
selangor <- ggplot(data = matrx2_data_frame,
                   main="Dengue Cases in SELANGOR : 2011", 
                   mapping = aes(x = matrx2_data_frame$NEGERI,
                                 y = matrx2_data_frame$SELANGOR)) + 
  geom_line(color="red") + 
  labs(y="Total Cases",x = "Month", title="Dengue Cases in SELANGOR : 2011")

putrajaya <- ggplot(data = matrx2_data_frame, 
                    main="Dengue Cases in SELANGOR : 2011", 
                    mapping = aes(x = matrx2_data_frame$NEGERI, 
                                  y = matrx2_data_frame$WPKL.PUTRAJAYA)) + 
  geom_line(color="red") + 
  labs(y="Total Cases",x = "Month", title="Dengue Cases in PUTRAJAYA : 2011")

kelantan <- ggplot(data = matrx2_data_frame, 
                   main="Dengue Cases in KELANTAN : 2011", 
                   mapping = aes(x = matrx2_data_frame$NEGERI, 
                                 y = matrx2_data_frame$KELANTAN)) + 
  geom_line(color="red") + 
  labs(y="Total Cases",x = "Month", title="Dengue Cases in KELANTAN : 2011")

johor <- ggplot(data = matrx2_data_frame, 
                   main="Dengue Cases in JOHOR : 2011", 
                   mapping = aes(x = matrx2_data_frame$NEGERI, 
                                 y = matrx2_data_frame$JOHOR)) + 
  geom_line(color="red") + 
  labs(y="Total Cases",x = "Month", title="Dengue Cases in JOHOR : 2011")

kedah <- ggplot(data = matrx2_data_frame, 
                main="Dengue Cases in KEDAH : 2011", 
                mapping = aes(x = matrx2_data_frame$NEGERI, 
                              y = matrx2_data_frame$KEDAH)) + 
  geom_line(color="red") + 
  labs(y="Total Cases",x = "Month", title="Dengue Cases in KEDAH : 2011")

install.packages("Rmisc")

library(Rmisc)
multiplot(selangor, putrajaya, johor, kedah)

# plot many states on the same graph
z <- ggplot(data = matrx2_data_frame, aes(x = matrx2_data_frame$NEGERI)) +
  geom_line(aes(y = matrx2_data_frame$SELANGOR, colour = "Sel")) + 
  geom_line(aes(y = matrx2_data_frame$SARAWAK, colour = "SRW")) + 
  geom_line(aes(y = matrx2_data_frame$PERLIS, colour = "Per")) +
  geom_line(aes(y = matrx2_data_frame$KELANTAN, colour = "Kltn")) +
  geom_line(aes(y = matrx2_data_frame$TERENGGANU, colour = "Trg")) +
  geom_line(aes(y = matrx2_data_frame$PAHANG, colour = "Phg")) +
  geom_line(aes(y = matrx2_data_frame$JOHOR, colour = "Jhr")) +
  scale_colour_discrete(name = "State") +
  labs(y="Total Cases",x = "Week",title="Dengue Cases in Selected States : 2011")
z

#WIP 14122016 : multiple lines on same graph
ggplot(data = matrx2_data_frame, aes(matrx2_data_frame$NEGERI)) + 
  geom_line(aes(y = matrx2_data_frame$SELANGOR, colour = "red")) + 
  geom_line(aes(y = matrx2_data_frame$JOHOR, colour = "green")) + 
  geom_line(aes(y = matrx2_data_frame$WPKL.PUTRAJAYA, colour = "blue"))

#set dev.off()
dev.off()
