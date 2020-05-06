## Analysis of the 'College' dataset available in ISLR.
## Basic feature engg and randomForest


library(ISLR)
library(randomForest)

str(College) #gives the structure of the dataset

# to check the correlations of numeric predictors
cor=cor(College[,-1])
corrplot::corrplot(cor)

# many of the predictors are correlated
# we should explore some of them before modelling

# Apps,Accept, Enroll are highly pairwise correlated
summary(College[,2:4])
# few colleges with very high roll strength
# for these 3 vars, no. of colleges more than 3Q are 194 (no. of applicants)
# the same for three vars,which is very helpful


# we define new variable strength, which indicates whether a college has 
# high or low student strength
Strength=rep(0,length(College$Enroll))
Strength[College$Enroll>902]=1
Strength=as.factor(Strength)
# Thus we can work with only enrolled and strength as more no of applications
# and acceptance is due to more strength

college=cbind(College[,-c(2,3)],Strength)

#Top10perc, Top25perc
plot(College$Top10perc[Strength==0],College$Top25perc[Strength==0])
# this relationship is quite interesting
# 
college=college[,-4]

#F.Undergrad, P.Undergrad
plot(college$F.Undergrad[Strength==0],college$P.Undergrad[Strength==0],
     xlim=c(0,3000),ylim=c(0,2000))
plot(college$Enroll,college$F.Undergrad)
plot(college$Enroll,college$P.Undergrad)
# no. of new students enrolled are mostly for full time undergrad
# very  few atudents are enrolling for part time
# and full time and part time has no relationship
# remove No of full times, as enrollment is a proxy for it 

college=college[,-4]


# Expenses
totalExp=college[,'Outstate']+college[,'Room.Board']+college[,'Books']+
  college[,'Personal']
cor(totalExp,college[,c(5,6,7,8)])
# many things to explore, instead let the models decide
college=cbind(college,totalExp)

#PhD, Terminal
plot(college$PhD,college$Terminal)
plot(college$PhD,college$S.F.Ratio,xlim=c(60,110))
plot(college$Terminal,college$S.F.Ratio)

summary(college[,c('PhD','Terminal','S.F.Ratio')])


###################################
# Modelling
###################################

# try to understand variable importance
mod.lm=lm(Grad.Rate~.,college)
mod.rf=randomForest(Grad.Rate~.,college,mtry=4,importance=T)
summary(mod.lm)
varImpPlot(mod.rf)
# similar results

train=sample(1:777,600,replace=F)
college.test=college[-train,]
grad_rate.test=college.test[,'Grad.Rate']

# residual analysis
mod.lm=lm(Grad.Rate~.,college.train)
plot(mod.lm)
mod.rf=randomForest(Grad.Rate~.,college,subset=train,mtry=4,importance=T)


pred.lm=predict(mod.lm,newdata=college.test)
pred.rf=predict(mod.rf,newdata=college.test)

sqrt(mean((pred.lm-grad_rate.test)^2)) #13.69
sqrt(mean((pred.rf-grad_rate.test)^2))

# training using caret 

train=createDataPartition(college$Grad.Rate,p=0.7,list=F)

college.train=college[train,]
college.test=college[-train,]
grad_rate.test=college.test[,'Grad.Rate']

tic()
mod.rf=randomForest(Grad.Rate~.,college.train,mtry=4,importance=T)
toc()

pred.rf=predict(mod.rf,newdata=college.test)
sqrt(mean((pred.rf-grad_rate.test)^2)) #13.39


fitControl=trainControl(method='repeatedcv',
                        number=10,
                        repeats=5)
mod.rf.caret=train(Grad.Rate~.,
                   data=college.train,
                   method='rf',
                   trControl=fitControl,
                   verbose=F)


pred.rf=predict(mod.rf.caret,newdata=college.test)
sqrt(mean((pred.rf-grad_rate.test)^2)) 


hyperp=expand.grid(mtry=seq(2,10,2))
mod.rf.caret=train(Grad.Rate~.,
                   data=college.train,
                   method='rf',
                   trControl=fitControl,
                   verbose=F,
                   tuneGrid=hyperp) # chooses the best parameter as 4.







