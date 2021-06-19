# Data Mining - Knn and Decision Tree
# Author: Kaiyang Liu
# Version: May-24-2021
# Data Used: combat_data.csv

# Part I - Data Preprocessing
library(class)
library(rpart)
library(gmodels)
library(caret)
library(lattice)
library(ggplot2)
library(tidyverse)
library(e1071)

comb_data = read.csv("comb_data.csv",header = T)
comb_data = comb_data[,-1]
comb_data$winner_first_label = as.factor(comb_data$winner_first_label)
comb_data[,1] = ifelse(comb_data[,1]=='yes',"first win","second win")
comb_data[,8] = comb_data$First_pokemon_legendary == 'True'
comb_data[,9] = comb_data$Second_pokemon_legendary == 'True'
View(comb_data)

#-------------------------------------------------------------------------------

# Part II - Knn

error=vector()
for (k in (1:300))
{
  test.error<-vector()
  a = sample(1:dim(comb_data)[1],0.1*dim(comb_data)[1])
  train = comb_data[-a,]
  test = comb_data[a,]
  predictions = knn(train[,2:10],test[,2:10],train[,1],k)
  m = table(test$winner_first_label,predictions)
  error[k]=1-(sum(diag(m))/sum(m))
}

kchoose=which.min(error)

plot(ktest.error,ylab="test misclassification rates",xlab="k")

# Evaluation
knn.opt = knn(train = train[,2:10],test = test[,2:10], cl = train[,1], k = 12)
table = CrossTable(x = test[,1],	
                   y = knn.opt,	
                   dnn = c("Actual", "Predicted"),	
                   prop.chisq = FALSE)
accuracy = sum(diag(table$t))/sum(table$t)

# Cross validation
error = rep(0,10)
for(i in 1:10)
{
  a = sample(1:dim(comb_data)[1],0.1*dim(comb_data)[1])
  train = comb_data[-a,]
  test = comb_data[a,]
  predictions = knn(train[,2:10],test[,2:10],train[,1],k=12)
  m = table(test$winner_first_label,predictions)
  error[i] = 1-(sum(diag(m))/sum(m))
}
mean(error)


#-------------------------------------------------------------------------------


# Part III - Decision Tree
a = sample(1:dim(comb_data)[1],0.1*dim(comb_data)[1])
train = comb_data[-a,]
test = comb_data[a,]



fit = rpart(winner_first_label~., data=train, minsplit = 10, cp=0.0003)
print(fit)	
plot(fit, uniform=TRUE, margin = 0.1, main="Classification Tree for combat")
text(fit, use.n=TRUE, all=TRUE, cex=.8)


fit2 = rpart(winner_first_label~., data=train, minsplit = 10, cp=0.0008)
print(fit)	
plot(fit2, uniform=TRUE, margin = 0.1, main="Classification Tree for combat")
text(fit2, use.n=TRUE, all=TRUE, cex=.8)


#prediction
pred = predict(fit, newdata=test,type='class')
summary(pred)
table = table(test$winner_first_label,pred)
accuracy = sum(diag(table))/sum(table)


#prediction
pred2 = predict(fit2, newdata=test,type='class')
summary(pred2)
table2 = table(test$winner_first_label,pred2)
accuracy2 = sum(diag(table2))/sum(table2)

#-----------------
table0 = CrossTable(x = test[,1],	
                   y = pred2,	
                   dnn = c("Actual", "Predicted"),	
                   prop.chisq = FALSE)
accuracy0 = sum(diag(table0$t))/sum(table0$t)

#-----------------
#prune

#choose the complex parameter
printcp(fit2) # display the cp results 
summary(fit2) # detailed summary of splits

#prune the tree
pfit =  prune(fit2, cp=fit2$cptable[which.min(fit2$cptable[,"xerror"]),"CP"])


#plot the pruned tree

plot(pfit, uniform=TRUE, margin = 0.1, main="Pruned Classification Tree for combat")
text(pfit, use.n=TRUE, all=TRUE, cex=1)

pred = predict(pfit, newdata=test,type='class')
summary(pred)
table = table(test$winner_first_label,pred)
accuracy3 = sum(diag(table))/sum(table)


# Cross validation

error = rep(0,100)

for(i in 1:100)
{
  a = sample(1:dim(comb_data)[1],0.1*dim(comb_data)[1])
  train = comb_data[-a,]
  test = comb_data[a,]
  fit = rpart(winner_first_label~., data=train, minsplit = 10, cp=0.0005)
  predictions = predict(fit,test,type = "class")
  m = table(test$winner_first_label,predictions)
  error[i] = 1-(sum(diag(m))/sum(m))
}
mean(error)
