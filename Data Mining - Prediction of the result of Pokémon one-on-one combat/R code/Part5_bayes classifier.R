
library(readr)
library(bnlearn)

### bayes network

info <- read_csv("info.csv")
bn <- hc(info) 
plot(bn)
modelstring(bn)

fit <- bn.fit(bn,data = info)

### naive bayes

s <- sample(1:50000,40000)
train <- info[s,]
test <- info[-s,]
f <- naiveBayes(label~.,data = train)
p <- predict(f,test)
t <-table(test$label,p)
t
sum(diag(t))/sum(t) 

library(pROC)
pred <- ordered(p)
pre <- roc(test$label, pred)

plot(pre,print.auc=T,auc.polygon=T,max.auc.polygon=T)