
fraud <- read.csv("C:\\Users\\Lenova\\Desktop\\creditcard.csv")

plot(fraud)



set.seed(144)

#install.packages("caTools", lib = "C:\Users\Lenova\AppData\Local\Temp\Rtmp4S8ni2\downloaded_packages", encoding = "UTF-8")

require(caTools)

split <- sample.split(fraud$Class, SplitRatio=1/5)   

training<-subset(fraud, split == T)
testing<-subset(fraud, split == F)

model<-glm(Class~., data = training, family = binomial("logit"))

summary(model)

prediction<-predict(model, newdata = testing, type = "response")

confMat<- table(testing$Class, prediction>0.25)   
confMat  

#    FALSE  TRUE
# 0 71064    15
# 1    49    74

model2<-glm(Class~ Time + V1 + V4 + V8 + V9 + V10 + V13 + V14 + V20 + V21 + V22 + V26 + V27 + V28 + Amount, data = training, family = binomial("logit"))

summary(model2)

prediction2<-predict(model2, newdata = testing, type = "response")

confMat2<- table(testing$Class, prediction2>0.25)   
confMat2
cov(fraud)

install.packages("e1071")

#SVM:

library(e1071)

model_svm <- svm(Class~., data = training) 
summary(model_svm)
pred <- predict(model_svm, testing)
tablo <- table(Hesaplanan = pred, Gercek = testing$Class)
tablo

#Decision Trees:

library(rpart)
model_dt <- rpart(Class~., data = training)
summary(model_dt)
plot(model_dt)