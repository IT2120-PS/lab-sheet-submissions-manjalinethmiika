getwd()
setwd("C:\\Users\\it23782372\\Desktop\\IT23782372")
Delivery_Times<-read.table("Exercise - Lab 05.txt",header=TRUE,sep=",")

head(Delivery_Times)
str(Delivery_Times)



#2
histogram<-hist(Delivery_Times$Delivery_Time_.minutes.,
                breaks = seq(20, 70, length.out = 10),
                right = FALSE,
                main = "Histogram of Delivery Times",
                xlab = "Delivery Time (minutes)",
                col = "green",
                border = "black")
#3 
install.packages("e1071")
library(e1071)
skewness(Delivery_Times$Delivery_Time)


#4
breaks <- round(histogram$breaks)
freq<-histogram$counts
mids<-histogram$mids

classes <- c()

for(i in 1:length(breaks)-1){
  classes[i] <- paste0("[", breaks[i], ",", breaks[i+1], ")")
}

cbind(Classes = classes, Frequency = freq)

lines(mids, freq)
plot(mids,freq,type = 'o', main = "Frequency for Delivers Time", xlab = "Delivery Time", ylab = "Frequency", ylim = c(0,max(freq)))



cum.freq <- cumsum(freq)

new<-c()
for(i in 1:length(breaks)){
  if(i==1){
    new[i]= 0
  }else{
    new[i]= cum.freq[i-1]
  }
}

plot(breaks, new, type = 'o', main = "Cumulative Frequency Polygon for Delivery Times",
     xlab = "Delivery Times", ylab = "Cumulative Frequency", ylim = c(0,max(cum.freq)))

cbind(Upper = breaks, CumFreq = new)



plot(breaks,new,type="o",
     main ="Cumulative Frequency Polygon (Ogive)",
     ylab = "Cumulative Frequency", xlab = "Delivery Time",
     ylim=c(0,max(cum.freq)))