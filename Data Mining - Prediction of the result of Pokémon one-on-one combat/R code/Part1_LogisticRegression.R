#Analysis using Logistic Regression

#Loading required package
library(aod)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(corrplot)
library(caret)
library(ggthemes)
library(RColorBrewer)
library(fmsb)
library(rpart.plot)
library(ROCR)
library(caret)

#Read in data and do proper preprocessing
pokemon<-read.csv("/Users/jacobluke/Downloads/archive-3/pokemon.csv",sep=",",stringsAsFactors=F)
colnames(pokemon)<-c("id","Name","Type.1","Type.2","HP","Attack","Defense","Sp.Atk","Sp.Def","Speed","Generation","Legendary")
Type.1<-c("Dragon","Steel","Flying","Psychic","Rock" ,"Fire","Electric" ,"Dark","Ghost" ,"Ground","Ice", "Water","Grass","Fighting", "Fairy" ,"Poison","Normal","Bug")
color<-c("#6F35FC","#B7B7CE","#A98FF3","#F95587","#B6A136","#EE8130","#F7D02C","#705746","#735797","#E2BF65","#96D9D6","#6390F0","#7AC74C","#C22E28","#D685AD","#A33EA1","#A8A77A","#A6B91A")
COL<-data.frame(Type.1,color)

#This is the type relationship table for types affecting each other
atk<-c('Normal','Fire','Water','Electric','Grass','Ice','Fighting','Poison','Ground','Flying','Psychic','Bug','Rock','Ghost','Dragon','Dark','Steel','Fairy')
normal<-c(1,1,1,1,1,1,2,1,1,1,1,1,1,0,1,1,1,1)
fire<-c(1,0.5,2,1,0.5,0.5,1,1,2,1,1,0.5,2,1,1,1,0.5,0.5)
water<-c(1,0.5,0.5,2,2,0.5,1,1,1,1,1,1,1,1,1,1,0.5,1)
elec<-c(1,1,1,0.5,1,1,1,1,2,0.5,1,1,1,1,1,1,0.5,1)
grass<-c(1,2,0.5,0.5,0.5,2,1,2,0.5,2,1,2,1,1,1,1,1,1)
ice<-c(1,2,1,1,1,0.5,2,1,1,1,1,1,2,1,1,1,2,1)
fighting<-c(1,1,1,1,1,1,1,1,1,2,2,0.5,0.5,1,1,0.5,1,2)
poison<-c(1,1,1,1,0.5,1,0.5,0.5,2,1,2,0.5,1,1,1,1,1,0.5)
ground<-c(1,1,2,0,2,2,1,0.5,1,1,1,1,0.5,1,1,1,1,1)
flying<-c(1,1,1,2,0.5,2,0.5,1,0,1,1,0.5,2,1,1,1,1,1)
psychic<-c(1,1,1,1,1,1,0.5,1,1,1,0.5,2,1,2,1,2,1,1)
bug<-c(1,2,1,1,0.5,1,0.5,1,0.5,2,1,1,2,1,1,1,1,1)
rock<-c(0.5,0.5,2,1,2,1,2,0.5,2,0.5,1,1,1,1,1,1,2,1)
ghost<-c(0,1,1,1,1,1,0,0.5,1,1,1,0.5,1,2,1,2,1,1)
dragon<-c(1,0.5,0.5,0.5,0.5,2,1,1,1,1,1,1,1,1,2,1,1,2)
dark<-c(1,1,1,1,1,1,2,1,1,1,0,2,1,0.5,1,0.5,1,2)
steel<-c(0.5,2,1,1,0.5,0.5,2,0,2,0.5,0.5,0.5,0.5,1,0.5,1,0.5,0.5)
fairy<-c(1,1,1,1,1,1,0.5,2,1,1,1,0.5,1,1,0,0.5,2,1)
mytable<-data.frame(Attacking=atk,Normal=normal,Fire=fire,Water=water,Electric=elec,Grass=grass,Ice=ice,Fighting=fighting,Poison=poison,Ground=ground,Flying=flying,Psychic=psychic,Bug=bug,Rock=rock,Ghost=ghost,Dragon=dragon,Dark=dark,Steel=steel,Fairy=fairy)

#Calculate the relative difference and put all the information into one table
test_combats<-read.csv('/Users/jacobluke/Downloads/archive-3/combats.csv',sep=",",stringsAsFactors=F)
names <- pokemon %>% dplyr::select(id, Name)
#test_combats<-sample_n(test_combats_all, 5e4)
test_combats$First_pokemon_name<-sapply(test_combats$First_pokemon, function(x) names$Name[match(x, names$id)])
test_combats$Second_pokemon_name<-sapply(test_combats$Second_pokemon, function(x) names$Name[match(x, names$id)])

test_combats$First_pokemon_attack<-sapply(test_combats$First_pokemon_name, function(x) pokemon$Attack[match(x, pokemon$Name)])
test_combats$Second_pokemon_attack<-sapply(test_combats$Second_pokemon_name, function(x) pokemon$Attack[match(x, pokemon$Name)])
test_combats$Diff_attack<-test_combats$First_pokemon_attack - test_combats$Second_pokemon_attack

test_combats$winner_first_label<-ifelse(test_combats$Winner==test_combats$First_pokemon,'yes','no')

test_combats$First_pokemon_defense<-sapply(test_combats$First_pokemon_name, function(x) pokemon$Defense[match(x, pokemon$Name)])
test_combats$Second_pokemon_defense<-sapply(test_combats$Second_pokemon_name, function(x) pokemon$Defense[match(x, pokemon$Name)])
test_combats$Diff_defense<-test_combats$First_pokemon_defense - test_combats$Second_pokemon_defense

test_combats$First_pokemon_sp_defense<-sapply(test_combats$First_pokemon_name, function(x) pokemon$Sp.Def[match(x, pokemon$Name)])
test_combats$Second_pokemon_sp_defense<-sapply(test_combats$Second_pokemon_name, function(x) pokemon$Sp.Def[match(x, pokemon$Name)])
test_combats$Diff_sp_defense<-test_combats$First_pokemon_sp_defense - test_combats$Second_pokemon_sp_defense

test_combats$First_pokemon_sp_attack<-sapply(test_combats$First_pokemon_name, function(x) pokemon$Sp.Atk[match(x, pokemon$Name)])
test_combats$Second_pokemon_sp_attack<-sapply(test_combats$Second_pokemon_name, function(x) pokemon$Sp.Atk[match(x, pokemon$Name)])
test_combats$Diff_sp_attack<-test_combats$First_pokemon_sp_attack - test_combats$Second_pokemon_sp_attack

test_combats$First_pokemon_speed<-sapply(test_combats$First_pokemon_name, function(x) pokemon$Speed[match(x, pokemon$Name)])
test_combats$Second_pokemon_speed<-sapply(test_combats$Second_pokemon_name, function(x) pokemon$Speed[match(x, pokemon$Name)])
test_combats$Diff_speed<-test_combats$First_pokemon_speed - test_combats$Second_pokemon_speed

test_combats$First_pokemon_HP<-sapply(test_combats$First_pokemon_name, function(x) pokemon$HP[match(x, pokemon$Name)])
test_combats$Second_pokemon_HP<-sapply(test_combats$Second_pokemon_name, function(x) pokemon$HP[match(x, pokemon$Name)])
test_combats$Diff_HP<-test_combats$First_pokemon_HP - test_combats$Second_pokemon_HP

test_combats$First_pokemon_type<-sapply(test_combats$First_pokemon_name, function(x) pokemon$Type.1[match(x, pokemon$Name)])
test_combats$Second_pokemon_type<-sapply(test_combats$Second_pokemon_name, function(x) pokemon$Type.1[match(x, pokemon$Name)])
test_combats$First_pokemon_legendary<-sapply(test_combats$First_pokemon_name, function(x) pokemon$Legendary[match(x, pokemon$Name)])
test_combats$Second_pokemon_legendary<-sapply(test_combats$Second_pokemon_name, function(x) pokemon$Legendary[match(x, pokemon$Name)])
for(i in 1:50000){
  test_combats$type_relationship[i] = mytable[which(mytable$Attacking == test_combats$First_pokemon_type[i]),c(test_combats$Second_pokemon_type[i])]
}
temp<- data.frame(test_combats %>% dplyr::select(winner_first_label,Diff_attack ,Diff_defense, Diff_sp_defense,Diff_sp_attack,Diff_speed ,Diff_HP, First_pokemon_legendary, Second_pokemon_legendary,type_relationship))

#Turning categorical data into numerical data and factorize them
temp$winner_first_label = ifelse(temp$winner_first_label == 'no', 0, 1)
temp$First_pokemon_legendary = ifelse(temp$First_pokemon_legendary == 'False', 0, 1)
temp$Second_pokemon_legendary = ifelse(temp$Second_pokemon_legendary == 'False', 0, 1)
temp$winner_first_label = as.factor(temp$winner_first_label)
temp$First_pokemon_legendary = as.factor(temp$First_pokemon_legendary)
temp$Second_pokemon_legendary = as.factor(temp$Second_pokemon_legendary)

#create training and testing set and build model
set.seed(1234)
split <- createDataPartition(y=temp$winner_first_label, p = 0.75, list = FALSE)
train <- temp[split,]
test <- temp[-split,]
mylogit = glm(winner_first_label ~ Diff_attack + Diff_defense + Diff_sp_defense + Diff_sp_attack + Diff_speed + Diff_HP + First_pokemon_legendary+Second_pokemon_legendary + type_relationship, data = train, family = "binomial")
a = predict(mylogit,newdata = test,type ="response")
test$predicted = ifelse(a>0.5,1,0)
sum(diag(table(test$winner_first_label,test$predicted)))/nrow(test)
mylogit = glm(winner_first_label ~ Diff_attack + Diff_defense + Diff_sp_defense + Diff_sp_attack + Diff_speed + Diff_HP + First_pokemon_legendary+Second_pokemon_legendary + type_relationship, data = train, family = "binomial")
mylogit_one = glm(winner_first_label ~ Diff_attack + Diff_defense + Diff_sp_defense + Diff_sp_attack + Diff_speed + Diff_HP + type_relationship, data = train, family = "binomial")

#Evaluating the reduced model with the original model
anova(mylogit_one,mylogit,test = "Chisq")

#Calculate the accuracy
a = predict(mylogit_one,newdata = test,type ="response")
test$predicted = ifelse(a>0.5,1,0)
sum(diag(table(test$winner_first_label,test$predicted)))/nrow(test)


#Evaluating the significance of each variables
library(survey)
regTermTest(mylogit,"Diff_attack")
regTermTest(mylogit,"Diff_defense")
regTermTest(mylogit,"Diff_sp_defense")
regTermTest(mylogit,"Diff_sp_attack")
regTermTest(mylogit,"Diff_speed")
regTermTest(mylogit,"Diff_HP")
regTermTest(mylogit,"First_pokemon_legendary")
regTermTest(mylogit,"Second_pokemon_legendary")
regTermTest(mylogit,"type_relationship")
varImp(mylogit)

prob = predict(mylogit_one,newdata = test, type ="response")
pred = prediction(prob,test$winner_first_label)
perf = performance(pred,measure = "tpr",x.measer = "fpr")

#Making the ROC curve and give out the value of AUC
library(pROC)
test_roc = roc(test$winner_first_label~prob, plot = TRUE,print.auc = TRUE)


#Cross validation
ctrl = trainControl(method = "repeatedcv",number = 10, savePredictions = TRUE)
mylogit_n = train(winner_first_label ~ Diff_attack + Diff_defense + Diff_sp_defense + Diff_sp_attack + Diff_speed + Diff_HP + type_relationship, data = train,method = "glm", family = "binomial",trControl = ctrl,tuneLength = 100)
pred1 = predict(mylogit_n,newdata = test)
confusionMatrix(data = pred1,test$winner_first_label)

