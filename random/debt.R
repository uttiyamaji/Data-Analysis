library(faraway)
str(debt)
attach(debt)
Debt=debt
## Data Cleaning

# incomegroup, ordered factor
incGrp=factor(incomegp,ordered = T)
summary(lm(prodebt~incGrp)) # p-values of Q & C are small, hance keep it numerical.

# house, ->factor
Debt['house']=as.factor(house)

# single parant, singlepar -> factor
Debt$singpar=factor(singpar)

# age group, ordered factor, p values of Q,C are small. leave as it is.
# bank acc, bsocacc->factor
Debt$bankacc=factor(bankacc)
Debt$bsocacc=factor(bsocacc)

Debt['cigbuy']=factor(cigbuy)
Debt['xmasbuy']=factor(xmasbuy)

# missing value imputation

i=which(is.na(incomegp)==T)
incomegp[sample(i,10,replace=F)]=2
i=which(is.na(incomegp)==T)
incomegp[i]=3
Debt$incomegp=incomegp

pred=FNN::knn.reg(data.frame(children[-(which(is.na(house)==T))]),data.frame(children[(which(is.na(house)==T))],house[-(which(is.na(house)==T))],k=10))

i=which(is.na(house)==T)                                    
Debt$house=house
agegp[which(is.na(agegp)==T)]=2
bankacc[sample(i,6,replace = F)]=0


i=which(is.na(ccarduse)==T)   
i=which(is.na(cigbuy)==T)
cigbuy[i]=0
Debt$cigbuy=factor(cigbuy)

var=c('incomegp','house','children','singpar','agegp','bankacc','bsocacc','manage','ccarduse','cigbuy','xmasbuy','locintrn','prodebt')


## EDA

plot(prodebt~incomegp,Debt,pch=20) # chances of having debt is same across all incomegroups

plot(prodebt~house,Debt) # seems chances of debt is same across all housing conditions
summary(lm(prodebt~house,Debt)) # coeff of house3 is significant. ie chances of debt of people who owns house is relatively lower then who rents or pays mortgage
house[house==1 | house==2]=0;house[house==3]=1
house[is.na(house)==T]=0;Debt$house=house # rent and mortgage are one class, private house another class
# running the summary of model, coeff of owned house is significant 

plot(prodebt~children,Debt) # chances of having debt is irrespective of no of children.

plot(prodebt~singpar,Debt) # doesn't depend
anova(lm(prodebt~singpar,Debt)) pvalue=.8

plot(prodebt~agegp,Debt) # doesn't depend
anova(lm(prodebt~agegp,Debt)) # pvalue< 0.05. Still not considering it.

anova(lm(prodebt~bankacc,Debt)) # pvalue=.23

anova(lm(prodebt~bsocacc,Debt)) # pvalue=.16

manageGrp=rep(1,464)
manageGrp[manage==1 | manage==2]=0
anova(lm(prodebt~manageGrp)) # highly significant 
Debt$manage=manageGrp

c=rep(0,464)
c[ccarduse==3]=1
ccarduse=factor(c)
Debt['ccarduse']=ccarduse
summary(lm(prodebt~ccarduse,Debt)) # significant

# both cigbuy and xmasbuy are insignificant

# with locintrn, there is a slight neg correlation, which is significant.


## modelling

rmse=function(x){
  mean((prodebt[test]-x)^2)
}

train=sample(1:464,350,replace=F);test=-train

# Benchmark model: predict prodebt by mean prodebt
rmse(mean(prodebt)) # 0.5053063

# Linear model using all the variables 
modLM1=lm(prodebt~.,Debt,subset=train)
predLM1=predict(modLM1,Debt[test,])
rmse(predLM1) # 0.4110126

# Linear model using selected variables
modLM2=lm(prodebt~house+manage+ccarduse+locintrn,data=Debt,subset=train)
predLM2=predict(modLM2,Debt[test,])
rmse(predLM2) # 0.4120473


# tree 

modTree=tree(prodebt~.,Debt,subset=train)
predTree=predict(modTree,Debt[test,])
rmse(predTree) # 0.5167082

# random Forest

modRF=randomForest(prodebt~.,data=Debt,subset=train,mtry=6)
predRF=predict(modRF,Debt[test,])
rmse(predRF) # 0.428916

# Boosting

modBoost=gbm(prodebt~.,Debt[train,],distribution = 'gaussian',n.trees=5000, interaction.depth = 6)
predBoost=predict(modBoost,Debt[test,],n.trees = 5000)
rmse(predBoost) # 0.427645


library(xgboost)
xgb <- xgboost(data = data.matrix(Debt[,-13]), 
               label = prodebt, 
               eta = 0.1,
               max_depth = 8, 
               nround=25, 
               subsample = 0.5,
               colsample_bytree = 0.5,
               seed = 1,
               eval_metric = "rmse",
               objective = "reg:linear",
               num_class = 12,
               nthread = 3
)


predxgb=predict(xgb,data.matrix(Debt[test,]))

rmse(predxgb)
















