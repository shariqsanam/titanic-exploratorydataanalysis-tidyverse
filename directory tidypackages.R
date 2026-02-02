library("tidyverse")

##Reading from files
##get the working directory
getwd()

#Set the Working directory
setwd("C:/Users/Lenovo/Desktop/R Program")

getwd()

##Reading CSV Files
titanic<-read.csv("titanic_data.csv",header=T)

##structure of data
str(titanic)

##first six records
head(titanic)

tail(titanic)

##summary of data
summary(titanic)

##selecting specific columns(single columns)
titanic$Survived
titanic$Gender

##multiple columns
titanic[,c("Name","Gender","Fare")]
titanic[,5:8]

#########FILTER OPERATION (ROW SELECTIONS)
#filter and select passengers above the age of 35
titanic[titanic$Age>35,c("PassengerId","Gender","Age")]

titanic[titanic$Age>35,c("Gender","Age","Fare")]

head(titanic[titanic$Age>35,c("Gender","Age","Fare")])

##selecting using SELECT-select specific columns
sel_set_1 <- titanic %>% select(Pclass,Age,Fare,Survived)
library(tidyverse)

##selecting using SELECT-select specific columns
sel_set_1 <- titanic %>% select(Pclass,Age,Fare,Survived)

## Females only
female_passengers <- titanic %>%
  filter(Gender=="female") %>% select(Pclass,Age, Fare, Survived)
titanic$Survival_status<-ifelse(titanic$Survived==1,"Survived","Not Survived")

##create a family count column using mutate()
library(tidyverse)
titanic<- titanic %>% mutate(FamilyMembers=titanic$SibSp+titanic$Parch)

##create an adult/child column age
titanic<- titanic %>% mutate(AgeGroup=ifelse(titanic$Age>18,"Adult","Child"))



####SORTING####
#sort by ascending fare
fares_asc<-titanic %>% arrange(Fare)

#by descending
fares_dsc<-titanic %>% arrange(desc(Fare))



##update the age of passenger id 
titanic$Age[titanic$PassengerId==1]

titanic$Age[titanic$PassengerId==1] <-23

titanic$Ticket[titanic$PassengerId==5]

titanic$Ticket[titanic$PassengerId==5] <-373451

#Group by
##find the number of male/female
titanic %>% group_by(Pclass)  %>% summarise(count=n())
titanic %>% group_by(Pclass)  %>% summarise(count=n())

library(tidyverse)

titanic %>% group_by(Gender) %>% summarise(count=n())
titanic %>% group_by(Gender) %>% summarise(AvgAge=mean(Age))
titanic %>% group_by(Gender) %>% summarise(AvgFare=mean(Fare))

#Count survivors by gender
titanic %>% group_by(Gender) %>% summarise(Survivors=sum(Survived))

#Count passengers by class
titanic %>% group_by(Pclass) %>% summarise(count=n())

#Count survivors by class
titanic %>% group_by(Pclass) %>% summarise(Survivors=sum(Survived))

#count survivors by class and arrange 
titanic %>% group_by(Pclass) %>% summarise(Survivors=sum(Survived)) %>% arrange(Survivors)

#writing data to a file
write.csv(titanic,"Titanic_Modified.csv")


#######
#Reading from a text file
text_data<-readLines("text.txt")
text_data

students<-read.table("Name.txt",header=TRUE)
students

#writing to a text file
student_data<-data.frame(
 Name=c("Abhishek","Mayuri","Arun","Risha"),
 Age=c(20,21,19,22),
 Marks=c(85,76,39,98))

write.table(student_data,"students.txt",sep="/t",row.names=FALSE)






