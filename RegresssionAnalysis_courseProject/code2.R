mod.full=lm(wage~age+education+gender+married+job+
          gender:married+gender:job+married:job+
          gender:age+gender:education+
          married:age+married:education+
          job:age+job:education,newData[-171,])
plot(mod.full$fitted.values,mod.full$residuals,xlab='Fitted',ylab='Residuals',pch=20,main='Residual Plot')

boxcox(mod.full,data=newData[-171,],lambda=seq(-2,2,0.1))

mod.full=lm(log(wage)~age+education+gender+married+job+
              gender:married+gender:job+married:job+
              gender:age+gender:education+
              married:age+married:education+
              job:age+job:education,newData[-171,])

plot(mod.full$fitted.values,mod.full$residuals,xlab='Fitted',ylab='Residuals',pch=20,
     main='Residuals vs Fitted for the log transformed model')

res.stud=rstudent(mod.full)
plot(res.stud,pch=20,ylab='Studentised Residuals',main='Studentized Residual Plot')
abline(h=c(0,-3,+3),col='red')
outliers=which(res.stud< -3 | res.stud>3)


x=model.matrix(mod.full)
lev=hat(x)
plot(lev,ylab='leverages',pch=20,main='leverages')
abline(h=2*20/nrow(newData[-171,]),lty=2,col='red') #too many leverage points with 2*p, lets try 3*p
highl=which(lev>0.075) # 8 points

cooksd=cooks.distance(mod.full)
plot(cooksd,pch=20,main='cooks distance')
which(cooksd==max(cooksd))

newdata=newData[-171,c(1,2,3,4,6,8,9)]
mod.full=lm(log(wage)~age+education+gender+married+job+
              gender:married+gender:job+married:job+
              gender:age+gender:education+
              married:age+married:education+
              job:age+job:education,newdata[-199,])



res.stud=rstudent(mod.full)
hist(mod.full$residuals,breaks=30,main='Histogram of residuals',xlab='Residuals',freq=FALSE)
lines(density(mod.full$residuals))
qqnorm(res.stud)
qqline(res.stud)
shapiro.test(res.stud)


summary(mod.full)

mod1=update(mod.full,.~.-job,newdata[-199,])

ndata=cbind(newdata[-199,-c(2,3)],female_job2,married_job3)
female_job2=rep(0,532)
female_job2[as.numeric(newdata$gender[-199])==2 & as.numeric(newdata$job[-199])==2]=1
married_job3=rep(0,532)
married_job3[as.numeric(newdata$married[-199])==2 & as.numeric(newdata$job[-199])==3]=1

mod2=lm(wage~age+gender+married+job+female_job2+married_job3,ndata,subset=train)
pred.lm=predict(mod2,newdata=ndata.test)
sqrt(mean((pred.lm-wage.test.ndata)^2))
ndata.test=ndata[-train,]
wage.test.ndata=ndata[-train,'wage']

pred=(pred.lm+pred.rf)/2















mod31=update(mod2,.~.-married_job3,newdata[-199,])

library(boot)
mod2=glm(log(wage)~age+education+gender+married+job+female_job2+married_job3,data=dt)
mod3=glm(log(wage)~age+education+gender+married+job+female_job2,data=dt)
mod4=glm(log(wage)~age+education+gender+job+female_job2,data=dt)
mod5=glm(log(wage)~age+education+married+job+female_job2,data=dt)


cv1=cv.glm(dt,mod2,K=10)
cv2=cv.glm(dt,mod3,K=10)
cv3=cv.glm(dt,mod4,K=10)
cv4=cv.glm(dt,mod5,K=10)
dt=cbind(newdata[-199],female_job2,married_job3)





plot(mod.full$fitted.values,mod.full$residuals)

mod.wls=lm(log(wage)~)

removed=c(outliers,highl)
female_job2new=rep(0,494)
female_job2new[as.numeric(newdata$gender[-removed])==2 & as.numeric(newdata$job[-removed])==2]=1
married_job3new=rep(0,494)
married_job3new[as.numeric(newdata$married[-removed])==2 & as.numeric(newdata$job[-removed])==3]=1

mod.wls=lm(log(wage)~age+education+gender+married+job+female_job2new+married_job3new,
           weights = 1/(education^3),newdata[-removed,])

mod22=lm(log(wage)~age+education+gender+married+job+female_job2new+married_job3new,
          newdata[-removed,])



mod.wls=lm(log(wage)~age+education+gender+married+job+female_job2+married_job3,
           weights = 1/(age+exp(education)),newdata[-199,])


age_job2=rep(0,532)
age_job2[as.numeric(job)]


























