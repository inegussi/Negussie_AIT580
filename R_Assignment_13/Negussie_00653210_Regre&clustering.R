###------------------
###Regression and Clustering
###------------------

###Students Name:Israel Negussie
###GNumber:00653210




data <- read.csv('~/Desktop/AIT 580/Negussie_AIT580/data/EmployeeAttrition.csv')
scatter.smooth(data$MonthlyIncome,data$TotalWorkingYears)
#There seems to be a positive relationship between monthly income
# and working years which makes sense because as workers have more experience
#they are more valuable.

scatter.smooth(data$Age,data$DistanceFromHome)
#There seems to be no relation between the two variables and
# I also didnt expect that there would be as logically those two are unrelated.

install.packages("polycor")
print(cor(data$MonthlyIncome,data$TotalWorkingYears))
print(cor(data$Age,data$DistanceFromHome))

linear<-lm(data$MonthlyIncom~data$TotalWorkingYears)
summary(linear)
confint(linear, level = 0.95)

library(dplyr)

mycluster<-select(data,HourlyRate,TotalWorkingYears)
 cluster<-kmeans(mycluster, 3)

 
cluster
plot(data[c("TotalWorkingYears","HourlyRate")],
     col=cluster$cluster)
# it seems like there is an even dristribution of hourly rates among workers
# with varying experience. There doesnt seem to be a trend that more qorking years equals higher pay.

#when i changed it to 5 clusters, there was a cluster formed for above 20 working years and
# another one for between 50 and 70 $ hourly rate

