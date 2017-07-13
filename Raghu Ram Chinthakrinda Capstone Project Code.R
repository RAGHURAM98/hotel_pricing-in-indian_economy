# Hotel Room Pricing In The Indian Market
# NAME: Raghu Ram Chinthakrinda
# EMAIL: edm14b010@iiitdm.ac.in
# COLLEGE : IIITDM KANCHEEPURAM

#set the working directory
setwd("D:/capstone project")

#Reading the dataset
hotel.df<-read.csv("Cities42.csv")

#Viewing the data
View(hotel.df)

#attaching the dataset
attach(hotel.df)

#first 10 rows of the dataset
head(hotel.df)

#last 10 rows of the dataset
tail(hotel.df)

#omitting the na from the dataframe
hotel.df=na.omit(hotel.df)

#dimensiions of the data frame 
dim(hotel.df)

#column names of the data frame
colnames(hotel.df)


#Removing the repeated date by gsub command
Date<-gsub("18-Dec-16", "Dec 18 2016",  Date)
Date<-gsub("21-Dec-16", "Dec 21 2016",  Date)
Date<-gsub("24-Dec-16", "Dec 24 2016",  Date)
Date<-gsub("25-Dec-16", "Dec 25 2016",  Date)
Date<-gsub("28-Dec-16", "Dec 28 2016",  Date)
Date<-gsub("31-Dec-16", "Dec 31 2016",  Date)
Date<-gsub("4-Jan-17", "Jan 04 2017",  Date)
Date<-gsub("4-Jan-16", "Jan 04 2017",  Date)
Date<-gsub("8-Jan-16", "Jan 08 2017",  Date)
Date<-gsub("8-Jan-17", "Jan 08 2017",  Date)
Date<-gsub("Jan 4 2017", "Jan 04 2017",  Date)
Date<-gsub("Jan 8 2017", "Jan 08 2017",  Date)


#Checking the dates
table(Date)

#Changing dates to factors for labelling 
Date<-factor( Date)
is.factor( Date)

#Checking the labelling
levels( Date)



#Analyzing the summary of the data and describing the variables
library(psych)
summary=describe(hotel.df)
summary[,c(3,4,5,8,9)]


#detaching the hotel.df dataset
detach(hotel.df)

#taking only numeric dataset by eliminating test
numeric_data=hotel.df[c(4,3,5,6,7,8,11,12,13,17,18,19,20)]
attach(numeric_data)

#checking the important variables
library(Boruta)
l=Boruta(RoomRent~.,data = numeric_data)
print (l)

#Taking Y = RoomRent, identifying the most relevent predictor variables by  correlation corrgram
#Corrgram

library(corrgram)
corrgram(numeric_data, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Corrgram of Hotel  data")

#more correlation is shown in swimming pool, starrating, hotel capacity w.r.t to room rent


#corrleation between room rent and remaining variables
cor(RoomRent,numeric_data)
#independent variablea are population, ismetrocity, freebreakfast
#categorical variables are has swimming pool, starrating, hotel capacity

#Table for HasSwimmingPool
Swim<-table(HasSwimmingPool)
barplot(Swim,main="Barrplot of Hotel Swimming Pool")
##hasswimming pool is categorical
    
#Table for StarRating
starRating<-table(StarRating)
barplot(starRating,main = "Barrplot for Star Rating")
##starrating is categorical

#BoxPlot for HotelCapacity
boxplot( HotelCapacity, main="Boxplot for Hotel Capacity",horizontal = TRUE)
##hotel capacity is continous


#Scatterplot pair wise for predictor variable
library(car)
#StarRating Vs RoomRent
scatterplot( StarRating, RoomRent,main="RoomRent of Hotels  with StarRating",ylab = "RoomRent in INR", xlab="Star rating out of 5",cex=1.1)

#RoomRent Vs HotelCapacity
scatterplot( RoomRent, HotelCapacity,main="RoomRent of Hotels  with Hotel capacity",ylab = "Hotel Capacity in rooms", xlab="RoomRent in INR",cex=1.1)
scatterplot( HotelCapacity,RoomRent,main="RoomRent of Hotels  with Hotel capacity",ylab = "Hotel Capacity in rooms", xlab="RoomRent in INR",cex=1.1)

#RoomRent Vs HasSwimmingPool
plot(jitter( RoomRent),jitter( HasSwimmingPool),main="RoomRent of Hotels  with HasSwimmingPool",ylab = "Has Swimmng Pool ", xlab="RoomRent",cex=1.1)


library(lattice)
bwplot(HasSwimmingPool~RoomRent, data = hotel.df,main="RoomRent of Hotels  with HasSwimmingPool",ylab = "Has Swimmng Pool ", xlab="RoomRent" )
    
#Scatterplot matrix
    
scatterplotMatrix( hotel.df[ ,c("RoomRent","HasSwimmingPool","StarRating", "HotelCapacity")], 
      spread=FALSE, smoother.args=list(lty=2),
      main="Scatter Plot Matrix", diagonal = "histogram")
        
    
#Corrgram of Y, x1, x2, x3
library(corrgram)
xyz<-data.frame( RoomRent,  HasSwimmingPool,  HotelCapacity,  StarRating)
corrgram(xyz, order=TRUE, lower.panel=panel.shade,
             upper.panel=panel.pie, text.panel=panel.txt,
             main="Corrgram of Hotel Prices In India")
    
#Variance-Covariance Matrix for Y, x1, x2, x3
x<-hotel.df[,c("HasSwimmingPool","StarRating", "HotelCapacity")]
y<-hotel.df[,c("RoomRent")]
cor(x,y)
cov(x,y)
var(x,y)



#week4 day6
#Forming a variable which is having RoomRent less than 1 lakh because the outliers effect the average
RoomRent1.df <-hotel.df[which( RoomRent<100000),]

#Comparing other factors and their pattern using other trends with roomrent
   
#Analyzing IsWeekeng effect on RoomRent
table( IsWeekend)
   
table1<-table( IsWeekend)
barplot(table1, main="Distribution of Weekend", xlab="Not weekend(0)         Weekend(1)", col="orange")
   
#Effect of Isweekend on RoomRent
iw= aggregate(RoomRent ~ IsWeekend, data=hotel.df,mean)
iw
   
boxplot(RoomRent~IsWeekend,data=hotel.df, main="Room rent vs. IsWeekend", ylab="Not weekend(0)  weekend(1)", xlab="Room Rent in rupees ", col=c("red","blue"),horizontal=TRUE)

#Without extreme outliers 
boxplot(RoomRent~IsWeekend,data=RoomRent1.df, main="Room rent vs. IsWeekend", ylab="Not weekend(0)  weekend(1)", xlab="Room Rent in rupees ", col=c("red","blue"),horizontal=TRUE)
   

library(lattice)
histogram(Date, data = hotel.df, main="Distribution of Dates", xlab = "Differnt of Dates", col="Blue")
   
   
#Effect of different dates on RoomRent
d = aggregate(RoomRent ~ Date, data = hotel.df,mean)
d
   
scatterplot(d$Date,d$RoomRent, main="Scatterplot between Date and RoomRent", xlab="Date", ylab = "Room Rent in Rupees")
boxplot(RoomRent~Date,data=hotel.df, main="Room rent vs. Date", xlab="Different Dates", ylab="Room Rent in rupees ", col=c("red","blue","green","yellow"))
   
##Without extreme outliers
boxplot(RoomRent~Date,data=RoomRent1.df, main="Room rent vs. Date", xlab="Different Dates", ylab="Room Rent in rupees ", col=c("red","blue","green","yellow"))
   
#Analyzing IsMetroCity effect on RoomRent
table( IsMetroCity)
   
table1<-table( IsMetroCity)
barplot(table1, main="Distribution of IsMetroCity", xlab="Not a Metro city(0)         Is a Metro City(1)", col="blue")
   
#Effect of IsMetroCity on RoomRent
imc = aggregate(RoomRent ~ IsMetroCity, data = hotel.df, mean)
imc
   
boxplot(RoomRent~IsMetroCity,data=hotel.df, main="Room rent vs. IsMetroCity", ylab="Metro city(1) or not(0)", xlab="Room Rent in rupees ", col=c("red","blue","green","yellow"),horizontal=TRUE)
##Without extreme outliers
boxplot(RoomRent~IsMetroCity,data=RoomRent1.df, main="Room rent vs. IsMetroCity", ylab="Metro city(1) or not(0)", xlab="Room Rent in rupees ", col=c("red","blue","green","yellow"),horizontal=TRUE)
   
   
#Analyzing IsTouristDestination effect on RoomRent
table( IsTouristDestination)
   
table1<-table( IsTouristDestination)
barplot(table1, main="Distribution of IsToursitDestination", xlab="Not a Tourist Destination(0)         Is a Tourist Destination(1)", col="yellow")
   
#Effect of IsTouristDestination on RoomRent
itd = aggregate(RoomRent ~ IsTouristDestination, data = hotel.df, mean)
itd
   
boxplot(RoomRent~IsTouristDestination,data=hotel.df, main="Room rent vs. IsTouristDestination", ylab="IsTouristDestination(1) or not(0)", xlab="Room Rent in rupees ", col=c("red","blue","green","yellow"),horizontal=TRUE)
##Without extreme outliers
boxplot(RoomRent~IsTouristDestination,data=RoomRent1.df, main="Room rent vs. IsTouristDestination", ylab="IsTouristDestination(1) or not(0)", xlab="Room Rent in rupees ", col=c("red","blue","green","yellow"),horizontal=TRUE)
   
#Analyzing FreeWifi Vs RoomRent
fw<-table( FreeWifi)
barplot(fw, main="Borplot of FreeWifi",xlab= "FreeWifi" ,col="red")
   
#Effect of FreeWifi on RoomRent
fw = aggregate(RoomRent ~ FreeWifi, data = hotel.df, mean)
fw
   
##With extreme outliers of roomrent
boxplot(RoomRent~FreeWifi,data=hotel.df, main="Room rent vs. FreeWifi", ylab="Free Wifi available(1)", xlab="Room Rent in rupees ", col=c("red","blue","green","yellow"),horizontal=TRUE)
##Without extreme outliers of roomrent
boxplot(RoomRent~FreeWifi,data=RoomRent1.df, main="Room rent vs. FreeWifi", ylab="Free Wifi available(1)", xlab="Room Rent in rupees ", col=c("red","blue","green","yellow"),horizontal=TRUE)
   
#Analyzing FreeBreakfast Vs RoomRent
fw<-table( FreeBreakfast)
barplot(fw, main="Borplot of FreeBreakfast",xlab= "FreeWifi" ,col="red")
   
   
#Effect of FreeBreakfast on RoomRent
fb= aggregate(RoomRent ~ FreeBreakfast, data =RoomRent1.df, mean)
fb
 
##With extreme outliers of roomrent
boxplot(RoomRent~FreeBreakfast,data=hotel.df, main="Room rent vs. FreeBreakfast", ylab="Free Breakfast available(1)", xlab="Room Rent in rupees ", col=c("green","yellow"),horizontal=TRUE)
##Without extreme outliers of roomrent
boxplot(RoomRent~FreeBreakfast,data=RoomRent1.df, main="Room rent vs. FreeBreakfast", ylab="Free Breakfast available(1)", xlab="Room Rent in rupees ", col=c("green","yellow"),horizontal=TRUE)
   
   
#Analyzing Airport distance from hotel effects in what way on RoomRent
summary(Airport)
boxplot( Airport, main="Boxplot of Airport",xlab= "Distance of airport from hotel(Km)" ,col="green",horizontal = TRUE)
   
#Effect of Airport distance on RoomRent
scatterplot( Airport, RoomRent, main="Room rent vs. Airport distance", xlab="Airport distance(km)", ylab="Room Rent in rupees ",cex=1.1)

   
##Hypothesis
   #1.Average RoomRent in hotels having swimming pool is more than that which don't have.
   t.test(RoomRent~HasSwimmingPool, alternative="less")
   
   #2.Average RoomRent in hotels with high star rating is high as compared to one which has less star rating.
   t.test( RoomRent, StarRating)
   
   #3.Average RoomRent in hotels providing Free Breakfast is more than that which don't provide.
   t.test(RoomRent~FreeBreakfast, alternative="less")
   
   #4.Average RoomRent in metro cities hotels is more than that of non metro cities.
   t.test(RoomRent~IsMetroCity, alternative="less")
   
   #5.Average RoomRent in hotels having more hotel capacity is more compared to one with less capacity.
   t.test( RoomRent, HotelCapacity)
   
   
   #chi sq test
   chisq.test(RoomRent,HotelCapacity)
   chisq.test(RoomRent,StarRating)
   chisq.test(RoomRent,HasSwimmingPool)
   
   
#Generating a multiple linear regression model for RoomRent
   #1.
   fit1<-lm(RoomRent~StarRating+HasSwimmingPool+HotelCapacity-1, data = hotel.df)
   summary(fit1)
   confint(fit1)
   
   #Coefficents of the model
   fit1$coefficients
   
   
   fit2<-lm(RoomRent~StarRating+HasSwimmingPool+HotelCapacity+IsWeekend+IsTouristDestination-1, data = hotel.df)
   summary(fit2)
   confint(fit2)
   
   #Coefficents of the model
   fit2$coefficients
  
   
   #3.
   fit3<-lm(RoomRent~StarRating+HasSwimmingPool+HotelCapacity+Airport-1, data = hotel.df)
   summary(fit3)
   confint(fit3)
   
   #Coefficents of the model
   fit3$coefficients

   #4.
   fit4<-lm(RoomRent~StarRating+HasSwimmingPool+HotelCapacity+FreeBreakfast+Airport+Population+FreeWifi-1, data = hotel.df)
   summary(fit4)
   confint(fit4)
   
   #Coefficents of the model
   fit4$coefficients   
   
   #detaching the numeric_data set
   detach(numeric_data)
   