# Initialization
library(e1071)
library(MASS)
library(ggplot2)
library(tidyverse)   
library(kernlab)     
library(RColorBrewer)
SVM_accuracy = c()
LDA_accuracy = c()
SVM_accuracy_2 = c()
SVM_accuracy_3 = c()


for(i in 1:20){
  # Data preprocessing
  df_temp = read.csv("comb_data.csv",header=T)
  n = dim(df_temp)[1]
  p = dim(df_temp)[2]
  index = sample(n)
  df_temp = df_temp[-c(1)]
  df_temp[,1] = as.factor(df_temp[,1])
  df_temp[,8] = df_temp$First_pokemon_legendary == 'True'
  df_temp[,9] = df_temp$Second_pokemon_legendary == 'True'
  df_train = df_temp[index[1:round(9*n/10)],]
  df_test = df_temp[index[round(9*n/10):n],]
  
  # SVM
  SVM_model <- svm(winner_first_label~.,data = df_train,kernel="linear",cost=5,scale = T)
  SVM_test <- predict(SVM_model, df_test[-c(1)])
  table_SVM = table(SVM_test,df_test[,1])
  SVM_accuracy[i] = sum(diag(table_SVM))/dim(df_test)[1]
  
  # LDA
  LDA_model = lda(winner_first_label~.,data = df_train)
  LDA_test = predict(LDA_model, newdata = df_test[-c(1)])
  table_LDA = table(LDA_test$class, df_test[,1])
  LDA_accuracy[i] = sum(diag(table_LDA))/dim(df_test)[1]
}

# Find the mean accuracy of the SVM and LDA
mean(SVM_accuracy)
mean(LDA_accuracy)

# Visualization of SVM
plot(SVM_model,df_test,Diff_speed ~ Diff_attack,slice = list(type_relationship=1,Diff_defense = 0.09994,Diff_sp_defense = 0.05858,Diff_sp_attack=0.2297,Diff_HP=-0.03302,First_pokemon_legendary=FALSE,Second_pokemon_legendary=FALSE))

# Visualization of LDA
dataset = data.frame(x = LDA_test$x,class = LDA_test$class,y = as.factor('combat_result'))
ggplot(dataset, aes(x = LD1,y = y,color=class)) +geom_jitter(position=position_jitter(0.25),cex=1.4) +  theme(legend.title = element_text(size = 25), legend.text  = element_text(size = 20),legend.key.size = unit(0.5, "lines"))


# ROC curve of SVM
df_SVM_roc = data.frame(real_label = ifelse(df_test$winner_first_label=='yes',1,0),prediction = ifelse(SVM_test=='yes',1,0))
test_roc = roc(df_SVM_roc$real_label~df_SVM_roc$prediction, plot = TRUE,print.auc = TRUE)

# ROC curve of LDA
df_LDA_roc = data.frame(real_label = ifelse(df_test$winner_first_label=='yes',1,0),prediction = ifelse(LDA_test$class=='yes',1,0))
test_roc = roc(df_LDA_roc$real_label~df_LDA_roc$prediction, plot = TRUE,print.auc = TRUE)

# SVM model with polynomial kernel
SVM_model2 <- svm(winner_first_label~.,data = df_train,kernel="polynomial",cost=5,scale = T)
SVM_test2 <- predict(SVM_model2, df_test[-c(1)])
table_SVM2 = table(SVM_test2,df_test[,1])
SVM_accuracy_2[i] = sum(diag(table_SVM2))/dim(df_test)[1]
mean(SVM_accuracy_2)
plot(SVM_model2,df_test,Diff_speed ~ Diff_attack,slice = list(type_relationship=1,Diff_defense = 0.09994,Diff_sp_defense = 0.05858,Diff_sp_attack=0.2297,Diff_HP=-0.03302,First_pokemon_legendary=FALSE,Second_pokemon_legendary=FALSE))

# SVM model with radial kernel
SVM_model3 <- svm(winner_first_label~.,data = df_train,kernel="radial",cost=5,scale = T)
SVM_test3 <- predict(SVM_model3, df_test[-c(1)])
table_SVM3 = table(SVM_test3,df_test[,1])
SVM_accuracy_3[i] = sum(diag(table_SVM3))/dim(df_test)[1]
mean(SVM_accuracy_3)
plot(SVM_model3,df_test,Diff_speed ~ Diff_attack,slice = list(type_relationship=1,Diff_defense = 0.09994,Diff_sp_defense = 0.05858,Diff_sp_attack=0.2297,Diff_HP=-0.03302,First_pokemon_legendary=FALSE,Second_pokemon_legendary=FALSE))



