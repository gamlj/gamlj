k<-30
N<-50
a<-rnorm(k,10,4)
b<-rnorm(k,0,4)
.data<-list()
for (i in 1:k) {
  x<-rnorm(N)
  z<-rnorm(N)^3
  y<-a[i]+b[i]*x+x*z+rnorm(100)
  d<-cbind(i,y,x,z)  
  .data[[length(.data)+1]]<-d
}

data<-as.data.frame(do.call(rbind,.data))
data$i<-factor(data$i)

head(data)
library(lmerTest)
f<-y~x*z+(1+x|i)
mod<-lmer(f,data=data)
class(mod)
bootres<-parameters::bootstrap_model(mod)

opts<-list(formula=f,data=data)
mod<-do.call(lmer,opts)
mod

class(mod)
packageVersion("parameters")

mod@call$control<-lme4::lmerControl(optimizer="bobyqa")
#mod@frame<-data.frame(a=1)
as.data.frame(parameters::parameters(mod,ci=NULL,effects="fixed"))

.coefficients<-as.data.frame(parameters::parameters(mod,ci=NULL))
.coefficients[.coefficients$Effects=="fixed",1:6]

parameters::parameters(bootres,ci_method="quantile")

afex::mixed(f,data=data,method = "LRT")
car::Anova(mod,type=3,test="F")

data$x<-data$x-mean(data$x)
data$z<-data$z-mean(data$z)
mod<-lmer(y~x*z+(1+x|i),data=data)
mod
afex::mixed(f,data=data,method = "LRT")
car::Anova(mod,type=3)

a<-y~x+z+(1+x|i)

f<-paste(tob64("x"),"~ 1+",tob64("v"))
fromb64(f,"x")

model1<-lmer(y~x*z+(1+x|i),data=data)
model0<-lmer(y~1+(1+x|i),data=data)
model00<-lm(y~1,data=data)

stats::anova(model1,model00)

afex::mixed(f,data=data,method = "LRT")
car::Anova(mod,type=3)
