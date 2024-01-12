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


table(lda.class,df$Sold)