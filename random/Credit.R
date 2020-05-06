library(ISLR)

data=Credit[,c(-1)]
M=cor(data[,c(-7,-8,-9,-10)])
library(corrplot)
corrplot(M,'number')

#response-Balance

#Balance and Rating:
r=seq(min(Rating),max(Rating))
modR1=glm(Balance~Rating) # lin Reg 20-fold CV=54205
modRq=lm(Balance~Rating+I(Rating^2))
modR2=glm(Balance~Rating+I(Rating^2)+I(Rating^3)) #Cubic Poly cv=53044
modR3=glm(Balance~bs(Rating,knots=c(200))) # Spline with one knot at 200 cv=51104
modR4=glm(Balance~cut(Rating,breaks = c(0,200,650,1000))) # Stepwise
lhs=function(x) ifelse(x<230,1,0)
rhs=function(x) ifelse(x>230,x,0)
modR5=lm(Balance~lhs(Rating)+rhs(Rating)+I(rhs(Rating)^2)+I(rhs(Rating)^3)) 
#This is the Best fit CV=48371 
pred=predict(modRq,newdata=list(Rating=r))
lines(r,pred)

#Balance and LImit
l=seq(min(Limit),max(Limit))
lhs=function(x) ifelse(x<3000,1,0)
rhs=function(x) ifelse(x>3000,x,0)
modL5=lm(Balance~lhs(Limit)+rhs(Limit)+I(rhs(Limit)^2))
predL5=predict(modL5,newdata=list(Limit=l))

# Balance and Limit are collinear. cor(,)~1.
#Hence the model below can't be computed.
modLR=lm(Balance~lhs(Rating)+rhs(Rating)+I(rhs(Rating)^2)+I(rhs(Rating)^3)+lhs(Limit)+rhs(Limit)+I(rhs(Limit)^2))


# Balance and Income
# scatterplot indicates Linear modelling would be bad idea
# rather use kNN
train=sample(1:400,350,replace=F);test=-train
BalInc=data.frame(Balance,Income)
Balance.test=Balance[test]
library(FNN)
predkNN=knn.reg(BalInc[train,],BalInc[test,],Balance[train],k=10)
mean((predkNN$pred-Balance.test)^2) # test error=218.8
# MSE(training error) from the Lin Reg model=166354

# Balance and Cards
# Cards possibly have no effect on Balance

# Balance and Age
# scatterplot indicates Linear modelling would be bad idea
# rather use kNN

# Categorical Vars
# only Student has some effect 


# Comparisons

m1=lm(Balance~.,data=dt1[train,])


cor(data[,-c(7,8,9,10)])

## TASK: Feature Selection. Make the best model.



dt1=data.frame(Balance,Income,Rating)
lhs=function(x) ifelse(x<230,1,0)
rhs=function(x) ifelse(x>230,x,0)


plot(Rating,Balance,col='dodgerblue',pch=20)
pred=predict(m,newdata=list(Rating=r))
lines(r,pred)


train=sample(1:400,390,replace=F)
mod=lm(Balance~lhs(Rating)+rhs(Rating)+I(rhs(Rating)^2),data=d1[train,])

m=lm(Balance~Income,data=d1[train,])
m=lm(Balance~(Rating)+(Rating)+I((Rating)^2),data=dt1[train,])

predl=predict(mod,dt1[-train,])
predk=FNN::knn.reg(dt1[train,],dt1[-train,],Balance[train],k=10)
predk=FNN::knn.reg(dt1[train,-2],dt1[-train,-2],Balance[train],k=10)
mean((Balance[-train]-predl)^2)
mean((Balance[-train]-predk$pred)^2)

train=matrix(ncol=40,nrow=390)
mse=matrix(ncol=40,nrow=20)
for(i in 1:40){
  train[,i]=sample(1:400,390,replace=F)
  error=rep(0,20)
  for(k in 1:20){
   predk=FNN::knn.reg(dt1[train[,i],],dt1[-train[,i],],Balance[train[,i]],k=k)
   error[k]=mean((Balance[-train[,i]]-predk$pred)^2)
  }
  mse[,i]=error
}



train=matrix(ncol=40,nrow=382)
mse=matrix(ncol=40,nrow=20)
for(i in 1:40){
  train[,i]=sample(1:400,390,replace=F)
  
  error1=rep(0,20)
  for(k in 1:20){
    predk=FNN::knn.reg(dt1[train[,i],],dt1[-train[,i],],Balance[train[,i]],k=k)
    error1[k]=mean((Balance[-train[,i]]-predk$pred)^2)
  }
  mse[,i]=error1
  
}

library(tree)
library(randomForest)
library(gbm)

dt1=data.frame(Balance,Income,Rating)
dt1=data.frame(Balance,Income)
train=sample(1:400,300,replace=F);test=-train

m1=lm(Balance~(Rating)+(Income)+I((Rating)^2),data=dt1[train,])
m1=lm(Balance~(Income),data=dt1[train,])
pLin=predict(m1,dt1[test,])

pkNN=FNN::knn.reg(dt1[train,],dt1[test,],Balance[train],k=10)

m3=tree(Balance~.,data=dt1[train,])
pTree=predict(m3,dt1[test,])

m4=randomForest(Balance~.,data=dt1,subset=train,mtry=1,importance=T)
pRf=predict(m4,newdata=dt1[test,])

m5=gbm(Balance~.,data=dt1[train,],distribution = 'gaussian',n.trees=5000,interaction.depth=2)
pBoost=predict(m5,newdata=dt1[test,],n.trees=5000)

mean((Balance[test]-pLin)^2)
mean((Balance[test]-pkNN$pred)^2)
mean((Balance[test]-pTree)^2)
mean((Balance[test]-pRf)^2)
mean((Balance[test]-pBoost)^2)








train=createDataPartition(data$Balance,p=0.7,list=F)

fitcontrol=trainControl(method='repeatedcv',
                        number=10,
                        repeats=5)

mod.lm1=train(Balance~Rating,
              data=data[train,],
              method='lm',
              trControl=fitcontrol,
              verbose=F)







































