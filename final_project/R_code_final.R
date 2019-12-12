#Israel Negussie (G00653210)
#final_project_code
#i used this code for my analysis to answer questions of interest


data<-read.csv("FieldOfStudyData1516_1617_PP.csv")
library(dplyr)
majors <- data %>%
  group_by(CIPDESC) %>%
  summarise(counts = n())


aggregate(data$COUNT, by=list(Category=data$CIPDESC), FUN=sum)

data2<- data[data$COUNT != 'PrivacySuppressed',]
data2$COUNT <- as.numeric(as.character(data2$COUNT))
data3<-aggregate(data2$COUNT, by=list(Category=data2$CIPDESC), FUN=sum)
df <-data3[order(-data3$x),]
print(df)
top20<-df[1:20,]
as.data.frame.table(top20)

data4<-data[data$MD_EARN_WNE != 'PrivacySuppressed',]
data4$MD_EARN_WNE <- as.numeric(as.character(data4$MD_EARN_WNE))
data5<-aggregate(data4$MD_EARN_WNE, by=list(Category=data4$CIPDESC), FUN=median)
df2<-data5[order(-data5$x),]
top20earnings<-df2[1:20,]
print(top20earnings)

data6<-data[data$DEBTMEDIAN != 'PrivacySuppressed',]
data6$DEBTMEDIAN <- as.numeric(as.character(data6$DEBTMEDIAN))
boxplot(data6$DEBTMEDIAN)
print(mean(data6$DEBTMEDIAN))

aggregate(data2$CIPDESC ~ data2$COUNT, data2, sum)
  
library(dplyr)
data2 %>% 
  group_by(data2$CIPDESC) %>% 
  summarise(majors = count(data2$COUNT))


print(majors)
df <-majors[order(-majors$counts),]
print(df)
top20<-df[1:20,]
print(top20)
histo<-hist(top20$counts)
print(histo)
barplot(top20$counts, names=top20$CIPDESC, col=rgb(0.2,0.4,0.6,0.6) )
