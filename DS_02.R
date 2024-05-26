#loading the CSV file
library(readr)
Titanic_Dataset <- read_csv("C:/Users/anany/Downloads/Titanic-Dataset.csv")
print(Titanic_Dataset)

#checking for missing values
sapply(Titanic_Dataset,function(x) sum(is.na(x)))

#loading necessary for analysis
library(tidyverse)

#filling missing 'Age' with median
median(Titanic_Dataset$Age)
Titanic_Dataset$Age[is.na(Titanic_Dataset$Age)]<-median(Titanic_Dataset$Age,na.rm=TRUE)
Titanic_Dataset$Age

#filling mmissing 'Embarked' with the mode
mode(Titanic_Dataset$Embarked)
Titanic_Dataset$Embarked[is.na(Titanic_Dataset$Embarked)]<-'S'
Titanic_Dataset$Embarked

#Droping 'Cabin' column for too many missing values
Titanic_Dataset<-Titanic_Dataset%>%select(-Cabin)

#converting appropriate columns to factors
Titanic_Dataset$Survived<-as.factor(Titanic_Dataset$Survived)
Titanic_Dataset$Pclass<-as.factor(Titanic_Dataset$Pclass)
Titanic_Dataset$Sex<-as.factor(Titanic_Dataset$Pclass)
Titanic_Dataset$Embarked<-as.factor(Titanic_Dataset$Embarked)

Titanic_Dataset$Survived
Titanic_Dataset$Pclass
Titanic_Dataset$Sex
Titanic_Dataset$Embarked

#summary analysis
summary(Titanic_Dataset)

#visualization of survival rate of sex
ggplot(Titanic_Dataset,aes(x=Sex,fill=Survived))+geom_bar(position="fill")+ylab("Proportion")

#visualization of survival rate of pclass
ggplot(Titanic_Dataset,aes(x=Pclass,fill=Survived))+geom_bar(position = "fill")+ylab("Proportion")

#visualization of age distribution by survival
ggplot(Titanic_Dataset,aes(x=Age,fill=Survived))+geom_histogram(bins=30,position = "identity",alpha=0.5)

#boxplot of Age by survived
ggplot(Titanic_Dataset,aes(x=Survived,y=Age))+geom_boxplot()

#barplot for survival rate by embarked location
ggplot(Titanic_Dataset,aes(x=Embarked,fill=Survived))+geom_bar(position = "fill")+ylab("Proportion")+ggtitle("Survival Rate by Embarked Location")

#correlation matrix for numerical variables
correlation_matrix<-cor(Titanic_Dataset%>% select(Age,SibSp,Parch,Fare))
print(correlation_matrix)

#pair plot
pairs(Titanic_Dataset%>%select(Age,SibSp,Parch,Fare),col=Titanic_Dataset$Survived)

#creating a new variable
Titanic_Dataset$FamilySize<-Titanic_Dataset$SibSp+Titanic_Dataset$Parch+1
Titanic_Dataset$FamilySize

#bar plot for survival rate by family size
ggplot(Titanic_Dataset,aes(x=FamilySize,fill=Survived))+
  geom_bar(position="fill")+ylab("Proportion")+ggtitle("Survival Rate by Family Size")

#survival rate by fare
ggplot(Titanic_Dataset,aes(x=Fare,fill=Survived))+geom_histogram(bins=30,position="identity",alpha=0.5)+ggtitle("Survival Rate by Fare")

#interaction effect between sex and pclass on survival
ggplot(Titanic_Dataset,aes(x=Pclass,fill=Survived))+geom_bar(position = "fill")+facet_wrap(~Sex)+ylab("Propostion")+ggtitle(("Survival Rate by Pclass and Sex"))

#logistic regression model
model<-glm(Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked,data=Titanic_Dataset,family=binomial)
summary(model)

#loading necessary library for decision tree
library(rpart)

#decision tree model
tree_model<-rpart(Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked,data=Titanic_Dataset)
plot(tree_model)
text(tree_model)

#loading necessary library for survival analysis model
library(survival)

#survival analysis model
surv_object<-Surv(time=Titanic_Dataset$Age,event=Titanic_Dataset$Survived==1)
fit<-survfit(surv_object~Pclass+Sex,data=Titanic_Dataset)
plot(fit)

