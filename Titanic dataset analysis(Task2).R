data<-read.csv("C:/Users/priya/Downloads/titanic_train.csv")#load data
#structure of the data
str(data)
#summary of the dataset
summary(data)
#Data cleaning
colSums(is.na(data))
#handling missing values
#replacing missing values with median 
data$Age[is.na(data$Age)]<-median(data$Age,na.rm=TRUE)
#convert relavent colunms to factor
data$Survived<-as.factor(data$Survived)
data$Pclass<-as.factor(data$Pclass)
data$Sex<-as.factor(data$Sex)
data$Embarked<-as.factor(data$Embarked)
library(ggplot2)
#Exploratory Data Analysis
#Univariate Analysis
#Distribution of Survived
ggplot(data,aes(x=Survived))+geom_bar(fill="steelblue")+labs(title="Distribution of Survival")
#Distribution Of Age
ggplot(data,aes(x=Age))+geom_histogram(bins=30,fill="lightblue",color="black")+labs(title="Distribution of Age")
#age distribution by survival
ggplot(data,aes(x=Age,fill=Survived))+geom_histogram(bins=30,position = "identity",alpha=0.6)+labs(title="Age Distribution by Survival",x="Age",y="Count")+scale_fill_manual(values=c("tomato","lightblue"))
#Fare distribution by Survival
ggplot(data,aes(x=Fare,fill=Survived))+geom_histogram(bins=30,position = "identity",alpha=0.6)+labs(title="Fare Distribution by Survival",x="Fare",y="Count")+scale_fill_manual(values=c("tomato","lightblue"))

#Bivariate Data Analysis
#Survival by Sex
ggplot(data,aes(x=Sex,fill=Survived))+geom_bar(position="dodge")+labs(title="Survival by Sex")
#Survival by Embarkation port
ggplot(data,aes(x=Embarked,fill=Survived))+geom_bar(position="dodge")+labs(title="Survival by Embarked")
install.packages("tidyverse")
library(dplyr)
#Survival by Pclass
ggplot(data,aes(x=Pclass,fill=Survived))+geom_bar(position="dodge")+labs(title="Survival by Passenger Class")
#Correlation between numerical variables
library(corrplot)
numeric_cols<-select(data,Age,Fare)
corrln_matrix<-cor(numeric_cols,use="complete.obs")
#plot the correlation matrix
corrplot(corrln_matrix,method="circle")
#Final summary after cleaning
summary(data)
#Boxplot to explore trends
#Boxplot of Fare by survival
ggplot(data,aes(x=Survived,y=Fare,fill=Survived))+geom_boxplot()+labs(title="Fare by Survival",x="Survived",y="Fare")
#Plotting Age,Passenger Class and Survival
ggplot(data,aes(x=Age,y=Pclass,color=Survived))+geom_point(alpha=0.7)+labs(title="Age vs Passanger Class by Survival",x="Age",y="Passenger Class")
#Cross-tab for Sex and Survived(Contingency table)
table(data$Sex,data$Survived)
#Cross-tab between Pclass and Survived
table(data$Pclass,data$Survived)