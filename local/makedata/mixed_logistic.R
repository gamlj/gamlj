data<-data.frame()
for (i in 1:35) {
q=FALSE
while (q==TRUE) {
n<-rbinom(1,size = 100,prob = .5)
q=(n %% 2)
}
intercept <- rnorm(1,-2,2)
beta <- c(3.5,rnorm(1,2,3),2)
x <-rnorm(n,0,1)
w <-rep(c(-1,1),each=n/2)

linpred <- intercept  +x * beta[1]+w * beta[2]+x*w * beta[3]+rnorm(n,0,4)
prob <- exp(linpred)/(1 + exp(linpred))
y <- ifelse(prob< .5,1,0)
x<-round((x*5/sd(x))+35,digits = 1)
dat<-as.data.frame(cbind(y,x,w))
names(dat)<-c("y","x","w")
dat$y<-factor(dat$y)
dat$w<-factor(dat$w)
dat$i<-i
data<-rbind(data,dat)
}

head(data)
data$i<-factor(data$i)
library(lme4)
data$z<-rep(1,nrow(data))
mod<-glmer(y~z+x*w+(1|i),data=data,family=binomial())
fixef(mod,add.dropped=T)
mod<-glm(y~1,data=data,family=binomial())
ss<-summary(mod)

ss$coefficients
terms.formula(mod@call[[2]])
mod@flist
ss<-summary(mod)
coef(ss)
ss$optinfo
coef(mod)
ss$coefficients
summary(mod)
gamlj::gamljGlmMixed(
  formula=y~z+x*w+(1|i),
  data=data,
  plotHAxis = x,
  plotSepLines = w,
  modelSelection = "logistic"
  
)
fixef(mod,add.dropped=T)
confint(mod,method="Wald",1:5)
write.csv(data,"extdata/pregnant.csv",row.names = F)
