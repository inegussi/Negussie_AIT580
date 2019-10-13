###--------------------------------------
#Student Name:Israel Negussie
#GNumber:00653210
###--------------------------------------

rm(list=ls())

data <- read.csv('Desktop/AIT 580/AIT580/data/EmployeeAttrition.csv')

# this is just for testing to use "print" statement.
print(data[1,])



# a. Find the number of rows and columns in the dataset (5 points)

print(nrow (data))
print(ncol (data))

# b. Find the maximum Age in the dataset (5 points)
print(max(data[,1]))

# c. Find the minimum DailyRate in the dataset (5 points)
print(max(data[,4]))

# d. Find the average/mean MontlyIncome in the dataset (5 points)
print(mean(data$MonthlyIncome))


# e. How many employees rated WorkLifeBalance as 1 (5 points)
print(sum(data$WorkLifeBalance== '1', na.rm=TRUE))

# f. What percent of total employees have TotalWorkingYears less than equal to 5? Also calculate the percentage for TotalWorkingYears greater than 5 (5 points)
print((length(which(data$TotalWorkingYears<=5))/(nrow (data))))
print((length(which(data$TotalWorkingYears>5))/(nrow (data))))
# g. Print EmployeeNumber, Department and MaritalStatus for those employees whose Attrition is Yes and RelationshipSatisfaction is 1 and YearsSinceLastPromotion is greater than 3 (10 points)
print(new.data <- subset(data, data$Attrition=='Yes' & data$RelationshipSatisfaction=='1' & data$YearsSinceLastPromotion>3))
print(newdata2<-new.data[c(10,5,18)])
# h. Find the mean, median, mode, standard deviation and frequency distribution of EnvironmentSatisfaction for males and females separately. (Hint: For frequency distribution use table() function (10 points)
Modefn <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
print(Modefn(Acme$Gender))
print(mean(data$EnvironmentSatisfaction[data$Gender == 'Male']))            
print(median(data$EnvironmentSatisfaction[data$Gender == 'Male']))
print(sd(data$EnvironmentSatisfaction[data$Gender == 'Male']))
print(table(data$EnvironmentSatisfaction[data$Gender == 'Male']))   


print(mean(data$EnvironmentSatisfaction[data$Gender == 'Female']))            
print(median(data$EnvironmentSatisfaction[data$Gender == 'Female']))
print(sd(data$EnvironmentSatisfaction[data$Gender == 'Female']))
print(table(data$EnvironmentSatisfaction[data$Gender == 'Female']))  



#Part 2 Acme

library(readr)
Acme <- read_csv("Desktop/AIT 580/AIT580/data/Acme.csv")

# Identify data types for each attribute in the dataset (5 points)
print(typeof(Acme$Years))
print(typeof(Acme$StSalary))
print(typeof(Acme$Gender))
print(typeof(Acme$Degree))

#Produce a summary statistic for each attribute in the dataset (5 points)
print(mean(Acme$Years))
print(median(Acme$StSalary))
print(sum(Acme$Gender== 'M', na.rm=TRUE))
Modefn <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
print(Modefn(Acme$Gender))

# Produce visualizations for each attribute (Hint: use hist() function) (5 points)
(hist(Acme$Years))
(hist(Acme$Gender))
(hist(Acme$StSalary))
(hist(Acme$Degree))

# Display the relationship between
#Years of Experience and Starting Salary for all employees (5 points)


plot(Acme$Years,Acme$StSalary)


# Years of Experience and Starting Salary for each gender (5 points)
print(male<-subset(Acme,Acme$Gender=='M'))
print(female<-subset(Acme,Acme$Gender=='F'))
plot(male$Years,male$StSalary)
plot(female$Years,female$StSalary)

# Years of Experience and Starting Salary for each degree (5 points)
#(Hint: use Scatter Plots)
print(BS<-subset(Acme,Acme$Degree=='BS'))
print(MS<-subset(Acme,Acme$Degree=='MS'))
print(Phd<-subset(Acme,Acme$Degree=='PhD'))
plot(BS$Years,BS$StSalary)
plot(MS$Years,MS$StSalary)
plot(Phd$Years,Phd$StSalary)

# Find the correlation between Starting Salary and Years of Experience? (5 points)
#Is the correlation different for each gender? (5 points)
print("No,There is a positive correlation for both gender")
# Is the correlation different for each degree? (5 points)
print("higher degree attainment reults in higher salary although no clear distrubution exists")
# What can you conclude about Acme with respect to gender bias after your overall analysis? (5
#type(Acme)

print("Acme doesnt discriminate with regards to equal pay")

