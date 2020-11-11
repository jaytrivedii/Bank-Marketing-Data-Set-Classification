#install.packages("C50")
library(C50)
library(gmodels)
library(ROSE)
library(rpart)

#Loading File
df=read.csv("C:/Users/Admin/Desktop/MOM/bank1_039.csv")
View(df)
summary(df)

#Dimensions of Data
dim(df)

df$balance.z=df$balance-mean(df$balance)/sd(df$balance)

#Standardizing
df$job=as.integer(factor(df$job))
df$marital=as.integer(factor(df$marital))
df$education=as.integer(factor(df$education))
df$default=ifelse(df$default=="yes",1,0)
df$housing.loan=ifelse(df$housing.loan=="yes",1,0)
df$personal.loan=ifelse(df$personal.loan=="yes",1,0)
df$subscribed39 = as.factor(df$subscribed)

#Create Training and Test set
set.seed(123)
train_sample <- sample(seq_len(nrow(df)),size=floor(0.80*nrow(df))) 
df_train <- df[train_sample,]
df_test <- df[-train_sample,]

#proportion of outcome categories
prop.table(table(df_train$subscribed39))

train1=ROSE(subscribed39~age+job+marital+education+default+balance.z+housing.loan+personal.loan+previous.campaign+current.campaign,data=df_train,seed=1)$data

#Building Model
cmodel <- C5.0(subscribed39~., data=train1,trails=15)
cmodel

#Evaluate model performance(Test)
cmodel_pred <- predict(cmodel, df_test)
cmodel_pred

#Cross table validation(Test)
CrossTable(df_test$subscribed39, cmodel_pred, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('actual default', 'predicted default'))

#Evaluate model performance(Train)
cmodel_pred <- predict(cmodel, df_train)
cmodel_pred

#Cross table validation(Train)
CrossTable(df_train$subscribed39, cmodel_pred, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('actual default', 'predicted default'))


#Building Model
mod1<-rpart(subscribed39 ~age+marital+education+default+balance+housing.loan+personal.loan+current.campaign+previous.campaign , method = "class", data = df_train)
predict(mod1,df_test,type = "class")->result1
table(df_test$subscribed39,result1)

#Evaluating Model
library(caret)
confusionMatrix(table(df_test$subscribed39,result1))



           