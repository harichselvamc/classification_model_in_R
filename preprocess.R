df<-read.csv("data.csv",header=TRUE)
View(df)
summary(df)
str(df)
#generalized linear model-logistic regression
glm.fit=glm(Sold~price,data=df,family=binomial)
summary(glm.fit) 



glm.probs=predict(glm.fit,type='response')
str(glm.probs)
glm.probs[1:10]


glm.pred=rep("NO",506)
glm.pred[glm.probs>0.5]="YES"
glm.pred



table(glm.pred,df$Sold)




#LDA

#install.packages("MASS")
library("MASS")


lda.fit=lda(Sold~.,data=df)

lda.fit

lda.pred=predict(lda.fit,df)

lda.pred$posterior

lda.class=lda.pred$class
lda.class


table(lda.class,df$Sold )


 sum(lda.pred$posterior[,1]>0.8)



qda.fit=qda(Sold~.,data=df)

qda.fit

qda.pred=predict(qda.fit,df)

qda.pred$posterior

qda.class=qda.pred$class
qda.class


table(qda.class,df$Sold )


sum(qda.pred$posterior[,1]>0.8)






#kmeans

#install.packages("caTools")
library("caTools")
set.seed(0)
split=sample.split(df,SplitRatio = 0.8)
train_set=subset(df,split==TRUE)
test_set=subset(df,split==FALSE)

train.fit=glm(Sold~.,data=train_set,family=binomial)
test.probs=predict(train.fit,test_set,type="response")

test.pred=rep("NO",120)
test.pred[test.probs>0.5]="YES"
table(test.pred,test_set$Sold)


#install.packages("class")
library("class")
trainx=train_set[,-16]
testx=test_set[,-16]

trainy=train_set$Sold
testy=test_set$Sold


k=3

trainx_s=scale(trainx)
testx_s=scale(testx)


set.seed(0)


knn.pred=knn(trainx_s,testx_s,trainy,k=k)
table(knn.pred,testy)
