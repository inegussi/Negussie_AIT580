###------------------
###Hypothesis Testing
###------------------

###Students Name:Israe Negussie
###GNumber:G00653210

setwd("~/Desktop/AIT 580/Negussie_AIT580")

rm(list=ls())

data <- read.csv('data/EmployeeAttrition.csv')


# Your hypothesis testings here...

male_income<-print((data$MonthlyIncome[data$Gender == 'Male']))  
women_income<-print((data$MonthlyIncome[data$Gender == 'Female'])) 
print(t.test(male_income,women_income, alternative="less", var.qual=T))
#p-value is not less than the signifcance level 0.05 so we can not reject the null hypothosis.


male_worklife<-print((data$MonthlyIncome[data$Gender == 'Male']))  
female_worklife<-print((data$MonthlyIncome[data$Gender == 'Female'])) 
print(t.test(male_worklife,female_worklife, alternative="greater", var.qual=T))
#p-value is not less than the signifcance level 0.05 so we can not reject the null hypothosis.

Single <-print((data$YearsAtCompany[data$MaritalStatus == 'Single']))  
Married <-print((data$YearsAtCompany[data$MaritalStatus == 'Married'])) 
print(t.test(Single,Married, alternative="greater", var.qual=T))
#p-value is not less than the signifcance level 0.05 so we can not reject the null hypothosis.
#Single preople years at the company is less than married.

Attritionyes<-print((data$EnvironmentSatisfaction[data$Attrition == 'Yes'])) 
Attritionno<-print((data$EnvironmentSatisfaction[data$Attrition == 'No'])) 
print(t.test(Attritionyes,Attritionno, alternative="greater", var.qual=T))
#p-value is not less than the signifcance level 0.05 so we can not reject the null hypothosis.
#Attrition = No has greater environmentql satisfaction

Manager<-print((data$MonthlyIncome[data$JobRole == 'Manager']))  
LabTech<-print((data$MonthlyIncome[data$JobRole == 'Laboratory Technician'])) 
print(t.test(Manager,LabTech, alternative="less", var.qual=T))
#Manager Income is higher than lab tech. dont reject null hypothesis

print(t.test(data$YearsAtCompany,data$DailyRate))
#There is insufficient evidence to conclude there is a significant linear 
#relationship between x and y because the correlation coefficient which is p value 
#is not significantly different from zero.

print(t.test(data$YearsAtCompany,data$MonthlyIncome))
#There is insufficient evidence to conclude there is a significant linear 
#relationship between x and y because the correlation coefficient which is p value 
#is not significantly different from zero.

print(t.test(data$YearsAtCompany,data$MaritalStatus))

print(t.test(data$MonthlyIncome,data$PerformanceRating))
#There is insufficient evidence to conclude there is a significant linear 
#relationship between x and y because the correlation coefficient which is p value 
#is not significantly different from zero.

print(t.test(data$MonthlyIncome,data$WorkLifeBalance))

#There is insufficient evidence to conclude there is a significant linear 
#relationship between x and y because the correlation coefficient which is p value 
#is not significantly different from zero.
