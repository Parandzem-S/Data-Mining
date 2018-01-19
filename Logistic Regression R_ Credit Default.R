install.packages("caret")
install.packages("ROCR",dependencies = TRUE)
install.packages("caTools")
install.packages("ggplot2")
install.packages("lattice")
install.packages("gplots")
library(caret)
library(gplots)
library(ROCR)
library(caTools)
library(ggplot2)
library("lattice")

credit<-read.csv("Credit.csv")
View(credit)
summary<-summary(credit)
names(credit)

credit$ed<-ordered(credit$ed, levels=c("no high school", "high school", "college degree", "undergraduate", "postgraduate"), labels=c(1, 2, 3, 4, 5))
credit$ed<-ordered(credit$ed, levels=c(1,2,3,4,5), labels=c("no high school", "high school", "college degree", "undergraduate", "postgraduate"))

summary

write.csv(summary,"summur.csv")

str(credit)
str(credit$default)
str(credit$ed)
levels(credit$ed)

#describing data via plots and pivot table
barplot(table(credit$default))

qplot(data = credit,x=default,y=address,geom = "violin")
qplot(data = credit,x=default,y=age,geom = "violin")
qplot(data = credit, x=default,y=employ,geom="boxplot")

library(devtools)
install_github("ramnathv/htmlwidgets") 
install_github("smartinsightsfromdata/rpivotTable")

library(rpivotTable)

rpivotTable(credit, rows = "default", cols ="ed", aggregatorName="average", 
                          vals="income", sorter ="Stacked Bar Chart")


rpivotTable(credit, rows = "default", cols ="ed", aggregatorName="average", 
            vals="debtinc", sorter ="Stacked Bar Chart")


rpivotTable(credit, rows = "default", cols ="ed", aggregatorName="average", 
            vals="othdebt", sorter ="Stacked Bar Chart")

# Create training and testing sets
set.seed(1982)
# Create an index vector sampling, 80% goes to training, 20% to testing sets
trainIndex <- createDataPartition(credit$default, p = .8, list = FALSE)                
# Create the dataframes
Train<-credit[trainIndex,]
Test<-credit[-trainIndex,]

#Logistic Model
logit1<-glm(default~.,data=Train, family ="binomial")
summary(logit1)

deviance<-anova(logit1, test="Chisq")
write.csv(deviance, "anova.csv")

#calculating VIF criteria to find out existence of multicollinearity 
library(car)
vif(logit1)

#including interaction of creddebt and other debt, excluding education and income
logit2<-glm(default~age+employ+address+debtinc+creddebt+creddebt*othdebt,data=Train, family ="binomial")
summary(logit2)

#excluding othdebt and creddebt*othdebt as they were not significant
logit3<-glm(default~age+employ+address+debtinc+creddebt,data=Train, family ="binomial")
summary(logit3)
names(logit3)

#exponential of coefficients
coef<-coef(logit3)
coef
expon<-exp(coef)
write.csv(expon,"expon.csv")

# Prediction
PredTest<-predict(logit3,newdata = Test, type="response")

table(PredTest>0.5, Test$default)

# Predict the class 
pr_label <-ifelse(PredTest>0.5, "Yes", "No")

#confusion matrix
install.packages('caret', dependencies = TRUE)
library("caret")

con<-confusionMatrix(pr_label,Test$default, positive="Yes")
 

# ROC curve and AUC

P_Test <- prediction(PredTest, Test$default) 

perf <- performance(P_Test,"tpr","fpr")
# The ROC curve
plot(perf)
# AUC
performance(P_Test, "auc")@y.values

#predicting for credit scoring data
creditscor<-read.csv("Credit scoring.csv")

predscore<-predict(logit3, newdata = creditscor, type="response")

credit_sc<-data.frame(creditscor[,-10],predscore=predscore,default)

creditscor$default <-ifelse(predscore>0.5, "Yes", "No")
default
View(credit_sc)
options(scipen = 999)

top_10<-head(credit_sc[order(credit_sc$predscore,decreasing = TRUE),],n=nrow(credit_sc)/10)
View(top_10)
bottom_10<-tail(credit_sc[order(credit_sc$predscore,decreasing = TRUE),],n=nrow(credit_sc)/10)
View(bottom_10)
sumtop<-summary(bottom_10)             
sumbot<-summary(top_10)
write.csv(sumtop,"top-b.csv")
write.csv(sumbot,"bot.csv")

rpivotTable(top_10, rows = "address", cols ="age", aggregatorName="average", 
            vals="othdebt", sorter ="Stacked Bar Chart")
rpivotTable(bottom_10, rows = "address", cols ="age", aggregatorName="average", 
            vals="othdebt", sorter ="Stacked Bar Chart")
install.packages("pastecs")
library(pastecs)
stat.desc(top_10, basic=F)

qplot(data = top_10,x=creddebt,y=debtinc,geom = "violin")
qplot(data = bottom_10,x=creddebt,y=debtinc,geom = "violin")

