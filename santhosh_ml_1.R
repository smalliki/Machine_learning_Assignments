#Dataset is taken from USI machine learning repository
#Dataset choosen is Bank data
library(readr)
bankdata <- read_delim("~/bankdata.txt", 
                       ";", escape_double = FALSE, trim_ws = TRUE)
View(bankdata)

#Descripitve Statistics

m <- mean(bankdata$balance) #to find average balance of bank "mean"
print(m) # It is used to print 
md <- median(bankdata$duration) # used to find the middle value of duration
print(md)
table(bankdata$marital) #to find the frequency of marital column
sort(-table(bankdata$age))[1] #mode of age column
range(bankdata$age) #to find min and max of age
quantile(bankdata$age) #to find quantile ranges
IQR(bankdata$age) #inter quartile range
v<-var(bankdata$age) # variance of age
sd(bankdata$age) #Standard deviation of age using sd() function
sqrt(v) #Standard Deviation without using sd() function

#Transformation

#log values of campaign 
log10(bankdata$campaign)[c(1:100)] 
# printing exponential values of first 100 entries of campaign
print(exp(bankdata$campaign)[c(1:100)]) 
cube<-(bankdata$campaign)^3 #cube of campaign values
print(cube)

#Plotting graphs

x<-bankdata$age[1:100] #assigning first 100 values of age to "x" variable
#assigning first 100 values of duration to "z" variable
z<-bankdata$duration[1:100]
t<-table(x) #frequency of age
par(col.lab="brown") #to color labels

# barplot of ages

#to give color to each bar in barplot
color<-c("blue", "grey", "red","orange","brown","yellow","green","pink")
cols <-     ifelse((t ==1) , color[1],
                   ifelse((t ==2), color[2],
                          ifelse((t ==3) , color[3],
                                 ifelse((t ==4), color[4],
                                        ifelse((t ==5) , color[5], 
                                               ifelse((t==6) , color[6],
                                                      ifelse((t==7) , color[7] , color[8] )))))))
x1<-barplot(table(x),width=1.5,ylim=c(0,10),xlim =c(0,70),
            xlab="Ages",ylab = "Frequency",main="Frequency Count", col = cols)
#labelling of bars
text(x1,table(x),labels = table(x),adj = c(0.5,-0.5), col= "darkcyan")

#scatterplot between ages and duration

plot(x,z,xlab="Age of the customer",ylab="Savings Acc duration",
     main="Relation b/w Ages and savings acc duration",pch=18, col= "deeppink4")
lm(x~z) #to find reggression line
abline(lm(x~z),col='red') #to plot regression line





