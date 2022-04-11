mod
p<-.30
N<-40
b=.1
x<-c(rep(-1,N*p),rep(1,N*(1-p)))
length(x)
log<-b*x+rnorm(N)
.y<-exp(log)/(1+exp(log))
y<-as.numeric(.y>.50)
data<-data.frame(y,x)
tab<-table(data)
tab
DE<-tab[2,2]
DN<-tab[2,1]
NE<-tab[1,2]
NN<-tab[1,1]
(E<-(DE+NE))
(N<-(DN+NN))
RR<-(DE/(DE+NE))/(DN/(DN+NN))
RR
data$id<-1:length(data$x)
data$x<-factor(data$x)

form<-y~x
mod<-geepack::geeglm(form, family = poisson(link = "log"), id = id, corstr = "exchangeable", data = data)
summary(mod)
exp(mod$coefficients)
pres<-glm(form,data=data,family = poisson())
summary(pres)
exp(pres$coefficients)
library(geepack)
data$ny<-as.numeric(data$y)
data$nx<-as.numeric(data$x)
data$nid<-as.numeric(data$id)
r<-relRisk(ny~nx,nid,data=data)
exp(r$beta)

#######
data<-read.csv2(file="extdata/relativerisk1.356.csv")
data$x<-factor(data$x)
contrasts(data$x)<--contr.sum(2)/2
contrasts(data$x)
levels(data$x)<-c("asad f","saaaa")
data$yn<-as.numeric(data$y)
form<-yn~x
mod<-geepack::geeglm(form, family = poisson(link = "log"), id = id, corstr = "exchangeable", data = data)
exp(mod$coefficients)
data$y<-factor(data$y)
data$x<-factor(data$x)
lev<-levels(data$x)
data[["x"]]==lev[2]
gmod<-gamljGzlm(formula=y~x,data=data,modelSelection = "logistic",effectSize = "RR")
gmod