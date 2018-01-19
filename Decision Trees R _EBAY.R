install.packages("caret")
install.packages("class")
install.packages("caTools")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("rattle")
install.packages("ROCR")
install.packages("tree")
library(xlsx)
install.packages("xlsxjars")
install.packages("rJava")
install.packages("lattice")
library(lattice)
install.packages("ggplot2")
library(ggplot2)
library(xlsxjars)
library(rpart.plot)
library(rJava)
library(rpart.plot)
train<-read.csv("ebay_test.csv")

 View(train)
 names(train)
 
 str(train)
 table(train$Competitive)  #use in report
 
 library(caret)
 set.seed(1982)
 library(rpart)
 library(party)
 install.packages("party")
 
 paste(names(train),collapse = "+")  #creating formula
 
 frm1<-(Competitive~Category+currency+sellerRating+Duration+endDay+ClosePrice+OpenPrice) 
 
 #1st model
 tree1<-rpart(frm1, data=train, method="class")
 
 
 png("t6.png", width = 1080, height =868) #to save as png file
 t1<-prp(tree1, 
     type=2, #type of the plot
     extra=3, #display extra information 
     faclen=4,# lenght of the factor variable to be shown 
     main="Decision tree for eBay auctions")
 dev.off()   #to save as png
 
 library(rattle())
 
 png("t7.png", width = 1600, height =1080,pointsize=48) #to save as png file
 fancyRpartPlot(tree1)
 dev.off()
 
 # Decision tree as set of rules
 asRules(tree1)
 
 #prune the tree
 set.seed(1600)

printcp(tree1) #to check cross validated errors and to choose min error to prune the tree

plotcp(tree1) 

pruned_tree12<-prune.rpart(tree1,tree1$cptable[which.min(tree1$cptable[,"xerror"]),"CP"])
printcp(pruned_tree12)

prp(pruned_tree12)


t2<-prp(pruned_tree12, 
        type=1, #type of the plot
        extra=1, #display extra information 
        faclen=1,# lenght of the factor variable to be shown 
        main="Decision tree for eBay auctions")

fit1<-rpart(frm1, data=train, method="class",
            control= rpart.control(minsplit=50, minbucket=25))
fit1<-rpart(frm1, data=train, method="class",
            control= rpart.control(minsplit=36, minbucket=12))
          printcp(fit1)
            
            t1<-prp(fit1, 
                    type=1, 
                    extra=1, 
                    faclen=1)
  prp(fit1)
  
#new formula and model
 frm2<-(Competitive~sellerRating+Duration+ClosePrice+OpenPrice)          
model2<-  rpart(frm2,data=train, method = "class")
printcp(model2)
prp(model2,type=1,extra=1,faclen =1)
 prp(model2)
Model2<- rpart(frm2,data=train, method = "class", 
               control = rpart.control(minsplit = 30,minbucket = 10))
prp(Model2, 
    type=1, 
    extra=1, 
    faclen=1)
printcp(Model2)

pruned_tree2<-prune.rpart(model2,model2$cptable[which.min(model2$cptable[,"xerror"])])
prp(pruned_tree2)

pruned_tree2<-prune.rpart(Model2,Model2$cptable[which.min(Model2$cptable[,"xerror"])])
prp(pruned_tree2)

# Making predictions/ class
test<-read.csv('ebay_test.csv')
predict<-predict(Model2, test, type="class")

###Confusion Matrix
table(Actual=test$Competitive, Predicted=predict)
confusionMatrix(predict, test$Competitive, positive="More")
### Roc-Curve and AUC
pred_prob<-predict(Model2, test, type="prob")

library(ROCR)
pred <- prediction(pred_prob[,2],
                   test$Competitive) 
perf <- performance(pred,"tpr","fpr")
performance(pred, "auc")@y.values
plot(perf)

#Random forest

install.packages("randomForest")
library(randomForest)
set.seed(415)
fit.rf<-randomForest(frm1,data=train,importance=T)
print(fit.rf)
importance(fit.rf)
plot(fit.rf)

t<-varImp(fit.rf)

#Testing
predict1<-predict(fit.rf, test, type="class")
table(Actual=test$Competitive, Predicted=predict1)
confusionMatrix(predict1, test$Competitive, positive="More")
pred_prob1<-predict(fit.rf, test, type="prob")

pred1 <- prediction(pred_prob1[,2],
                   test$Competitive) 
perf1 <- performance(pred1,"tpr","fpr")
performance(pred1, "auc")@y.values
plot(perf1)


     