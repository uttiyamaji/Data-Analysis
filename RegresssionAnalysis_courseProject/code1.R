library(AER)
data(CPS1985)
str(CPS1985)
attach(CPS1985)
library(ggplot2)

# age,edu, exp
plot(CPS1985[,1:4])

plot(wage~education)

ggplot(CPS1985,aes(y=wage,x=education))+geom_point()+geom_smooth(method='lm',formula=y~poly(x,2))


plot(wage~education,pch=20)
mols=lm(wage~education)
mwls=lm(wage~education,weights = 1/abs(mols$residuals)^2)
abline(mols,lty=10)
abline(mwls,col='blue')
x=unique(education)
ypred=predict(mols,data.frame(education=x))
points(x,ypred,col='red',pch=20)
plot(education,mols$residuals) # heteroskedasticity
plot(experience~age)
plot(wage~(age+experience))

# obs :
# age and experience are highly correlated. And this correlation is the same 
# across all categorical variables

ggplot(CPS1985,aes(x=wage,y=experience))+
  geom_point()+
  facet_grid(~ethnicity)

ggplot(CPS1985,aes(y=wage,x=education,col=ethnicity))+
  geom_point()+
  geom_smooth(method='lm',se=F)

ggplot(CPS1985,aes(x=experience,y=education))+
  geom_point()+
  facet_grid(~union)

# too many categorical variables. should reduce them

ggplot(CPS1985,aes(x=gender,y=wage))+
  geom_boxplot()
anova(lm(wage~gender)) # significant

ggplot(CPS1985,aes(x=married,y=wage))+
  geom_boxplot()
anova(lm(wage~married)) # signficant

ggplot(CPS1985,aes(x=gender,y=wage))+
  geom_boxplot()

ggplot(CPS1985,aes(x=ethnicity,y=wage))+
  geom_boxplot()
summary(lm(wage~ethnicity)) # significant

ggplot(CPS1985,aes(x=region,y=wage))+
  geom_boxplot()
summary(lm(wage~region)) # significant

ggplot(CPS1985,aes(x=occupation,y=wage))+
  geom_boxplot()
summary(lm(wage~occupation)) # all significant except one

ggplot(CPS1985,aes(x=sector,y=wage))+
  geom_boxplot()
summary(lm(wage~sector)) # not significant

ggplot(CPS1985,aes(x=union,y=wage))+
  geom_boxplot()
summary(lm(wage~union)) # significant

par(mfrow=c(1,2))
ggplot(CPS1985,aes(x=education,y=wage,col=married,shape=gender))+
  geom_point()+
  geom_smooth(method='lm',se=F)


summary(lm(wage~gender+married+gender*married+occupation+occupation*gender+occupation*married))





summary(lm(wage~married+gender+married*gender+occupation+union+region+ethnicity))

summary(lm(wage~occupation+gender))
summary(lm(wage~gender+occupation))


# occupation has 6 levels

summary(lm(wage~occupation))


job[as.numeric(occupation)%in%c(1,5)]=2
job[as.numeric(occupation)%in%c(2,6)]=3
job[as.numeric(occupation)%in%c(3,4)]=1   
job=factor(job)


eth=ifelse(as.numeric(ethnicity)==1,0,1)

# plausible interactions 
# gender and married
# region and eth
# all with job

# union is interesting

summary(lm(wage~union))
boxplot(wage~union)
# clear that people who do union are getting higher salary
table(job,union) # no such dependence

summary(lm(wage~region)) # South guys have sig low income

summary(lm(wage~gender+married+job+union+eth+region))
# married, job class 2 is not sig, eth and region sig but seems to be insig
# lets add the contnuous ones
summary(lm(wage~age+education+gender+married+job+union+eth+region))
# bang,eth and region are insig

ggplot(newData,aes(x=education,y=wage))+geom_point()+
  facet_wrap(gender~married)+
  geom_smooth(method='lm',se=F)
ggplot(newData,aes(x=wage,fill=married))+geom_histogram(alpha=0.8)

mod.all=lm(wage~.,newData)

# interaction bw gender and marriage

summary(lm(wage~gender+married+gender:married)) # wage of a bachelor man and 
#bachelor woman are same, wage of a married man is higher whereas that of a woman
# is lower

gm=c()
gm[ as.numeric(married)==1]=1
gm[as.numeric(gender)==1 & as.numeric(married)==2]=2
gm[as.numeric(gender)==2 & as.numeric(married)==2]=3
gm=as.factor(gm)

summary(lm(wage~gm)) # the last group is not significantly different from base

gm2=c()
gm2[as.numeric(gm)==1 | as.numeric(gm)==3]=1
gm2[as.numeric(gm)==2]=2
gm2=as.factor(gm2)
summary(lm(wage~gm2)) # both of them are significantly different but adj R decr


# interaction bw job and gender
summary(lm(wage~job+gender+job:gender))
plot(wage~job+gender)
summary(lm(wage~job+gm+job:gm))

# job can further be reduced to two classes.

ggplot(newData,aes(x=age,y=wage,col=job,shape=gender))+geom_point()+
  geom_smooth(method='lm',se=F)


mod.full=lm(wage~.,newData)

mod1=lm(log(wage)~education) 



set.seed(1234)
coef=rep(0,100)
for(i in 1:100){
  train=sample(1:534,300,replace = F)
  model=lm(wage~age,newData,subset=train)
  coef[i]=coef(model)[2]
}


lm(wage~age,newData)

g=ggplot(newData,aes(x=age,y=wage))+
  geom_point()+
  geom_smooth(method='lm',se=F)+
  theme_bw()
ggMarginal(g,type='histogram',fill='transparent')
# an outlier - clear from the plot 171th obs
# suggests heteroskedasticity
# slight increasing behaviour with age

ggplot(newData,aes(x=age,y=wage,col=gender))+
  geom_point()+
  geom_smooth(method='lm',se=F)+
  theme_bw()
lm(wage~age+gender+gender:age,newData) #interaction is signif, mean is not

ggplot(newData,aes(x=age,y=wage,col=married))+
  geom_point()+
  geom_smooth(method='lm',se=F)+
  theme_bw()+
  facet_grid(~gender)
# the coolest plot yet
# must include this
#has a lot to tell- specially male part.
ggplot(newData,aes(x=age,y=wage,col=gender))+
  geom_point()+
  geom_smooth(method='lm',se=F)+
  theme_bw()+
  facet_grid(~married)
# for married peoples- ,males earn significantly more than females
# but its independent of age
# for unmarried: initially, for younger peoples, females earn more
# but after age 30, males earn more
# here males income is correlated with age
ggplot(newData,aes(x=age,y=wage,col=gender))+
  geom_point()+
  geom_smooth(method='lm',se=F)+
  theme_bw()+
  facet_grid(~job)
# in level 1 jobs: mostly females, not sig different wage 
# in level 2: huge difference in wages, mostly males work here
# in level 3:mostly equal males and females but wage gap is sig
ggplot(newData,aes(x=age,y=wage,col=married))+
  geom_point()+
  geom_smooth(method='lm',se=F)+
  theme_bw()+
  facet_grid(~job)
# in level 1: yes>no
# 2: yes>no
# 3: no>yes 
ggplot(newData,aes(x=age,y=wage,col=married))+
  geom_point()+
  geom_smooth(method='lm',se=F)+
  theme_bw()+
  facet_grid(gender~job)
# reveals a lot-most of the differences in the prev plot was due to gender diff
ggplot(newData,aes(x=age,y=wage,col=gender))+
  geom_point()+
  geom_smooth(method='lm',se=F)+
  theme_bw()+
  facet_grid(married~job)





g=ggplot(newData,aes(x=education,y=wage))+
  geom_point()+
  geom_smooth(method='lm',se=F)+
  theme_bw()
ggMarginal(g,type='histogram',fill='transparent')
# heteroskedasticity



psych::pairs.panels(newData[1:4],ellipses=F,smooth=F,hist.col='transparent')


model=lm(log(wage)~age+education+gender+married+job,newData)



## Modelling

# Multicollinearity

mod1=lm(wage~age+experience+education,newData[-171,])

library(car)
vif(mod1) #4595.7342 5130.2542 229.5207 unusually high
library(perturb)
colldiag(mod1,add.intercept = F)

Condition
Index	Variance Decomposition Proportions
    CI    age   experience education
1   1.000 0.000 0.000      0.000    
2   3.436 0.000 0.004      0.007    
3  80.913 1.000 0.996      0.993    

# corr of wage with education is higher than others
# vif of experience is highest so drop experience

mod21=lm(wage~age+education,newData[-171,]) # adj R2=.2241
vif(mod21) #ok
mod22=lm(wage~experience+education,newData[-171,]) # adj R2=.2242
vif(mod22) #ok
mod23=lm(wage~age+experience,newData[-171,])
summary(mod23)
vif(mod23) # high

# first we build a exploratory model
# consider age and education in the model

mod2=lm(wage~age+education,newData[-171,])
model=lm(wage~age+education+gender+married+job,newData[-171,])


# Residual Analysis
# Model Assumptions

# res vs fitted
plot(mod2$fitted.values,mod2$residuals,xlab='fitted',ylab='residuals',pch=20)
# suggests heteroskedasticity
# heteroskedasticity should be removes first since an outlier in this model might not be an
# outlier in the transformed model

# transformation of response
boxcox(mod2,data=newData[-171,],lambda=seq(-2,2,0.1))
# gives lambda=0
# log transformation is required

mod3=lm(log(wage)~age+education,newData[-171,])
model=lm(log(wage)~age+education+gender+married+job,newData[-171,])
mod=lm(log(wage)~age+education+gender+married+job
                  gender:job+married:job,newData[-171,])


plot(mod3$fitted.values,mod3$residuals,xlab='fitted',ylab='residuals')
plot(model$fitted.values,model$residuals)
#heteroskedasticity is removed 

hist(mod3$residuals,breaks=25)
hist(model$residuals,breaks=25)
hist(mod3$residuals[-199]) # fairly normal

# studentised Res
res.stud=rstudent(mod3)
plot(res.stud,pch=20,ylab='Studentised Residuals')
abline(h=c(0,-3,+3),col='red')
outliers=which(res.stud<-3)
# only one outlier compared to the 7 in the previous model
res.stud=rstudent(model)
plot(res.stud,pch=20,ylab='Studentised Residuals')
abline(h=c(0,-3,+3),col='red')
outliers=which(res.stud< -3)
# only one outlier

# leverage points
x=model.matrix(mod3)
lev=hat(x)
plot(lev,ylab='leverages')
abline(h=3*3/nrow(newData[-171,])) #too many leverage points with 2*p, lets try 3*p
highl=which(lev>0.01688) # 8 points

x=model.matrix(model)
lev=hat(x)
plot(lev,ylab='leverages')
abline(h=2*7/nrow(newData[-171,])) #too many leverage points with 2*p, lets try 3*p
highl=which(lev>0.02626642) 

# cooks distance
cooksd=cooks.distance(mod3)
plot(cooksd) # none is significant
# thus although we have one outlier and 8 high leverage points, none of them is influential
# this point is very crucial
cooksd=cooks.distance(model2)
plot(cooksd)
# normal qq-departure from normality at high values
# include the qq plot
newdata=newData[-171,c(1,2,3,4,6,8,9)]
model2=lm(log(wage)~age+education+gender+married+job,newdata[-199,])
res.stud=rstudent(model2)
hist(model2$residuals,breaks=30)
qqnorm(res.stud)

qqline(res.stud)
shapiro.test(res.stud)

model=lm(log(wage)~experience+education+gender+married+job+gender:married+gender:job+
           married:job+gender:experience+gender:education,newData[-171,])


model2=lm(log(wage)~experience+education+married+job+gender+gender:job,newData[-171,])

model3=lm(log(wage)~experience+education+married+job+gender:job,data=newData[-171,])


library(boot)
m1=glm(log(wage)~experience+education+gender+married+job+gender:married+gender:job+
         gender:experience+gender:education,data=newData[-171,])
m2=glm(log(wage)~experience+education+married+job+gender+gender:job,data=newData[-171,])
m3=glm(log(wage)~experience+education+married+job+gender:job,data=newData[-171,])


cv1=cv.glm(newData[-171,],m1,K=10)
cv2=cv.glm(newData[-171,],m2,K=10)
cv3=cv.glm(newData[-171,],m3,K=10)


# include the factors 

# backward selection

mod4=lm(log(wage)~age+education+gender+married+job+
                  gender:married+gender:job+married:job+
                  gender:age+gender:education+
                  married:age+married:education+
                  job:age+job:education,newData[-171,])


step(mod4,method='backward')

mod42=lm(log(wage)~age+education+gender+job+
          gender:job+
          gender:age+gender:education+
          
          job:age+job:education,newData[-171,])



mod41=lm(log(wage)~age+education+gender,newData[-171,])
mod42=update(mod3,.~.+married)
mod43=update(mod3,.~.+job)

# how to choose between these two models?
# one has two levels and another has one level
# say, one level is significantly more than the other

mod5=mod41

mod51=update(mod5,.~.+married)
mod52=update(mod5,.~.+job)


mod11=lm(log(wage)~age,newdata[-199,]) #.044
mod12=lm(log(wage)~education,newdata[-199,]) #0.1465
mod13=lm(log(wage)~gender,newdata[-199,]) #0.055
mod14=lm(log(wage)~married,newdata[-199,]) #.0218
mod15=lm(log(wage)~job,newdata[-199,]) #0.17
mod16=lm(log(wage)~female_job2,newdata[-199,])#0.063
mod17=lm(log(wage)~married_job3,newdata[-199,])# 0.09

mod1=lm(log(wage)~job,newdata[-199,])

mod21=update(mod1,.~.+age,newdata[-199,]) #0.211
mod22=update(mod1,.~.+gender,newdata[-199,])#0.214
mod23=update(mod1,.~.+married,newdata[-199,])#0.185
mod24=update(mod1,.~.+female_job2,newdata[-199,])#0.226
mod25=update(mod1,.~.+education,newdata[-199,])#0.211
mod26=update(mod1,.~.+married_job3,newdata[-199,])#0.1706


mod2=mod24

mod31=update(mod2,.~.+gender,newdata[-199,]) #0.2338
mod32=update(mod2,.~.+married,newdata[-199,])#0.2419
mod33=update(mod2,.~.+age,newdata[-199,])#0.27
mod34=update(mod2,.~.+education,newdata[-199,])#0.2649
mod35=update(mod2,.~.+married_job3,newdata[-199,])#0.2252




mod3=mod33

mod41=update(mod3,.~.+gender,newdata[-199,]) #0.28
mod42=update(mod3,.~.+education,newdata[-199,])#0.332
mod43=update(mod3,.~.+married,newdata[-199,])#0.27
mod44=update(mod3,.~.+married_job3,newdata[-199,])#0.26

mod4=mod42
   
mod51=update(mod4,.~.+married,newdata[-199,])
mod52=update(mod4,.~.+gender,newdata[-199,])
mod53=update(mod4,.~.+married_job3,newdata[-199,])

mod5=mod52

mod61=update(mod5,.~.+married,newdata[-199,])#0.3462
mod62=update(mod5,.~.+married_job3,newdata[-199,])#0.3404

mod6=mod61


r2=c(0.17,0.226,0.27,0.332,0.341)
plot(r2,type='b')

model.full=lm(log(wage)~age+education+gender+married+job+
                        gender:married+gender:job+) 



ggplot(CPS1985,aes(x=gender,y=wage))+geom_boxplot()+theme_bw()
ggplot(CPS1985,aes(x=married,y=wage))+geom_boxplot()+theme_bw()
ggplot(CPS1985,aes(x=occupation,y=wage))+geom_boxplot()+theme_bw()

ggplot(newData[-171,],aes(x=education,y=wage,col=job))+
  geom_point()+
  geom_smooth(method='lm',se=F)+
  theme_bw()


ggplot(newData[-171,],aes(x=education,y=wage,col=gender))+
  geom_point()+
  geom_smooth(method='lm',se=F)+
  theme_bw()+
  facet_grid(married~job)




Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)   0.705887   0.139724   5.052 6.05e-07 ***
  age           0.010796   0.001644   6.567 1.24e-10 ***
  education     0.061695   0.008610   7.165 2.66e-12 ***
  genderfemale -0.146229   0.047459  -3.081  0.00217 ** 
  marriedyes    0.146621   0.046675   3.141  0.00178 ** 
  job2          0.189667   0.057162   3.318
F-statistic: 36.96 on 8 and 523 DF,  p-value: < 2.2e-16

> 

> 



  ggplot(newData,aes(x=job,y=wage))+geom_boxplot()+theme_bw()



library(ggplot2)

str(wage_data)
View(wage_data)


ggplot(wage_data,aes(x=gender,y=wage))+geom_boxplot()+theme_bw()
ggplot(wage_data,aes(x=married,y=wage))+geom_boxplot()+theme_bw()
ggplot(wage_data,aes(x=occupation,y=wage))+geom_boxplot()+theme_bw()

married_male=rep(0,533)
married_male[wage_data$gender=='male' & wage_data$married=='yes']=1


summary(lm(wage~occupation,wage_data))

job=rep(0,533)
job[as.numeric(wage_data$occupation)%in%c(1,5)]=2
job[as.numeric(wage_data$occupation)%in%c(2,6)]=3
job[as.numeric(wage_data$occupation)%in%c(3,4)]=1   
job=factor(job)


wage_data=cbind(wage_data,job)
wage_data=wage_data[,-4]

ggplot(wage_data,aes(x=job,y=wage))+geom_boxplot()+theme_bw()
summary(lm(wage~job,wage_data))

ggplot(wage_data,aes(x=age,y=wage))+
  geom_point(col='coral3')+
  geom_smooth(method='lm',se=F,col='coral3')+
  theme_bw()

ggplot(wage_data,aes(x=age,y=wage,col=gender))+
  geom_point()+
  geom_smooth(method='lm',se=F)+
  theme_bw()

ggplot(wage_data,aes(x=age,y=wage,col=married))+
  geom_point()+
  geom_smooth(method='lm',se=F)+
  theme_bw()


ggplot(wage_data,aes(x=age,y=wage,col=job))+
  geom_point()+
  geom_smooth(method='lm',se=F)+
  theme_bw()

ggplot(wage_data,aes(x=age,y=wage,col=married))+
  geom_point()+
  geom_smooth(method='lm',se=F)+
  theme_bw()+
  facet_grid(~gender)

ggplot(wage_data,aes(x=age,y=wage,col=gender))+
  geom_point()+
  geom_smooth(method='lm',se=F)+
  theme_bw()+
  facet_grid(~married)


ggplot(wage_data,aes(x=age,y=wage,col=gender))+
  geom_point()+
  geom_smooth(method='lm',se=F)+
  theme_bw()+
  facet_grid(~job)

ggplot(wage_data,aes(x=age,y=wage,col=married))+
  geom_point()+
  geom_smooth(method='lm',se=F)+
  theme_bw()+
  facet_grid(~job)

ggplot(wage_data,aes(x=age,y=wage,col=married))+
  geom_point()+
  geom_smooth(method='lm',se=F)+
  theme_bw()+
  facet_grid(gender~job)



ggplot(wage_data,aes(x=age,y=wage,col=gender))+
  geom_point()+
  geom_smooth(method='lm',se=F)+
  theme_bw()+
  facet_grid(married~job)




Coefficients:
                    Estimate   Std. Error t value Pr(>|t|)    
(Intercept)            8.4265     0.3575  23.568  < 2e-16 ***
occupationtechnical    3.5210     0.5637   6.246 8.67e-10 ***
occupationservices    -1.8890     0.6067  -3.114  0.00195 ** 
occupationoffice      -1.0039     0.5774  -1.739  0.08269 .  
occupationsales       -0.8338     0.8078  -1.032  0.30246    
occupationmanagement   3.6887     0.7051   5.232 2.43e-07 ***
  ---
  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 4.466 on 527 degrees of freedom
Multiple R-squared:   0.18,	Adjusted R-squared:  0.1722 
F-statistic: 23.13 on 5 and 527 DF,  p-value: < 2.2e-16


Coefficients:
            Estimate   Std. Error t value Pr(>|t|)    
(Intercept)   7.0144     0.3328  21.077  < 2e-16 ***
job2          1.2487     0.4621   2.702  0.00711 ** 
job3          4.9900     0.4860  10.268  < 2e-16 ***
  ---
  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 4.465 on 530 degrees of freedom
Multiple R-squared:  0.1755,	Adjusted R-squared:  0.1724 
F-statistic: 56.41 on 2 and 530 DF,  p-value: < 2.2e-16


library(tree)
tree.model=tree(wage~.,wage_data,subset=train)
summary(tree.model)
plot(tree.model)
text(tree.model,pretty=0)
pred.tree=predict(tree.model,newdata=wage_data.test)
sqrt(mean((pred.tree-wage.test)^2))


train=sample(1:nrow(wage_data),400,replace=F)
wage_data.test=wage_data[-train,]
wage.test=wage_data.test[,'wage']



rf.model=randomForest(wage~.,wage_data,subset=train,mtry=2,importance=T,ntree=500)
pred.rf=predict(rf.model,newdata=wage_data.test)
sqrt(mean((pred.rf-wage.test)^2))


