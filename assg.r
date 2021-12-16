 

# Ass 2  Linear Regression

# Loading the required Dataset
library("Metrics")
library("DAAG")
data1=read.csv(file = file.choose())
View(data1)

# Extracting the required data
data=data1[-1]
head(data)
dim(data)

# Dividing the dataset into training and testing dataset
train_dataset=data[1:140,]
head(train_dataset)

test_dataset=data[141:200,]
head(test_dataset)

# Using cbind function to bind all the columns
S=cbind("TV","Newspaper","Radio")

TV1=lm(Sales~TV,data=train_dataset)
TV1

Radio1=lm(Sales~Radio,data=train_dataset)
Radio1

Newspaper1=lm(Sales~Newspaper,data=train_dataset)
Newspaper1


# Plotting the graph
plot(train_dataset$Sales~train_dataset$TV,xlab="TV",ylab = "Sales")
abline(TV1)

plot(train_dataset$Sales~train_dataset$Radio,xlab="Radio",ylab = "Sales")
abline(Radio1)

plot(train_dataset$Sales~train_dataset$Newspaper,xlab="Newspaper",ylab="Sales")
abline(Newspaper1)

# Predicting the data
Tvp=predict(TV1,train_dataset)
Radiop=predict(Radio1,train_dataset)
Newspaperp=predict(Newspaper1,train_dataset)


Tvt=predict(TV1,test_dataset)
Radiot=predict(Radio1,test_dataset)
Newspapert=predict(Newspaper1,test_dataset)


TVtrain_mse=mse(train_dataset$Sales,Tvp)
TVtrain_mse

Radiotrain_mse=mse(train_dataset$Sales,Radiop)
Radiotrain_mse

Newspapertrain_mse=mse(train_dataset$Sales,Newspaperp)
Newspapertrain_mse

TVtest_mse=mse(test_dataset$Sales,Tvt)
TVtest_mse

Radiotest_mse=mse(test_dataset$Sales,Radiot)
Radiotest_mse

Newspapertest_mse=mse(test_dataset$Newspaper,Newspapert)
Newspapertest_mse

TrainMSE=c(TVtrain_mse,Radiotrain_mse,Newspapertrain_mse)  
TrainMSE

TestMSE=c(TVtest_mse,Radiotest_mse,Newspapertest_mse)
TestMSE

# Plotting the model for the training and testing dataset
barplot(TrainMSE,width = 0.02,xlab="data",ylab="error",main="Training Error")
barplot(TestMSE,width=0.02,xlab = "data",ylab="error",main="Testing Error")

model1=cv.lm(data,(Sales~TV),m=5)



#Ass__3


library(arules)
library(arulesViz)
library(datasets)

#4461 Soudip Das

#Dataset reading

data(Groceries)
#top 10 items .. dataset
itemFrequencyPlot(Groceries,topN=10,type="absolute")

#Apriori Algo

rules <- apriori(Groceries, parameter = list(supp = 0.01, conf = 0.5))
inspect(rules[1:6])
options(digits=2)
inspect(rules[1:6])

#removing the redundant rules

rules<-sort(rules, by="confidence", decreasing=TRUE)
inspect(rules[1:6])
rules
red_rules<-is.redundant(rules)
summary(red_rules)
rules<-rules[!red_rules]
rules
rules<-apriori(Groceries, parameter=list(supp=0.01,conf = 0.5), appearance = list(default="lhs",rhs="whole milk"))
inspect(rules[1:5])
plot(rules,method="graph",interactive=TRUE,shading=NA)




#Ass___8




#4461 - SOUDIP DAS


data <- read.csv("/uploads/wine.csv")
summary(data)
attach(data)
names(data)
X <- cbind(Wine,Alcohol,Malic.acid,Phenols)
summary(X)
cor(X)
pcal <- princomp(X,scores=TRUE,cor=TRUE)
summary(pcal)
loadings(pcal)
plot(pcal)

#Analysis

screeplot(pcal,type="line",main="Screen Plot")
biplot(pcal)
pcal$scores[1:8,]


