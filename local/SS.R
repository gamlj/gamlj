library(testthat)
N=100
b=0
tol=.00001

one<-function(N=100,b=2,tol=.0001) {

z<- -rep(c(1,2),each=N/2)
a<-  rep(c(1,2),N/2)
x<-  a+rnorm(N)
y<-  a+z+x+rnorm(N)


data<-as.data.frame(cbind(y,x,z,a))
data$z<-factor(data$z)
#data$a<-factor(data$a)
write.csv(data,file="extdata/sstest.csv")
form<-y~a*z
model<-lm(form,data=data)
sumr<-summary(model)
f<-sumr$fstatistic[[1]]
dfres<-sumr$fstatistic[[3]]
dfmod<-sumr$fstatistic[[2]]
ssmod<-f*sigma(model)^2*dfmod/dfres
ca<-car::Anova(model,type=3)
gmod<-gamlj::gamljGlm(formula = form,data,effectSize = c("eta","etap","omega","omegap","epsilon","epsilonp"))

ganov<-gmod$main$anova$asDF
ganeff<-ganov[ganov$name!="Residuals" & ganov$name!="Model" & ganov$name!="Total",]
SS<-ganeff$ss
ssres<-ganov$ss[ganov$name=="Residuals"]
sstot<-ganov$ss[ganov$name=="Total"]
ssmod<-ganov$ss[ganov$name=="Model"]
df<-ganeff$df
dfres<-ganov$df[ganov$name=="Residuals"]
yss<-sum((data$y-mean(data$y))^2)
p<-predict(model)
sshat<-sum((p-mean(p))^2)

test_that("tot", {
  expect_equal(sstot,yss)
  expect_equal(sshat,ssmod)
})


test_that("app", {
  expect_equal(sstot,sum(SS)+ssres)
})

form<-y~a*z+x
model<-lm(form,data=data)
sumr<-summary(model)
f<-sumr$fstatistic[[1]]
dfres<-sumr$fstatistic[[3]]
dfmod<-sumr$fstatistic[[2]]
ssmod<-f*sigma(model)^2*dfmod/dfres
ca<-car::Anova(model,type=3)
gmod<-gamlj::gamljGlm(formula = form,data,effectSize = c("eta","etap","omega","omegap","epsilon","epsilonp"))

ganov<-gmod$main$anova$asDF
ganeff<-ganov[ganov$name!="Residuals" & ganov$name!="Model" & ganov$name!="Total",]
SS<-ganeff$ss
ssres<-ganov$ss[ganov$name=="Residuals"]
sstot<-ganov$ss[ganov$name=="Total"]
ssmod<-ganov$ss[ganov$name=="Model"]
df<-ganeff$df
dfres<-ganov$df[ganov$name=="Residuals"]
yss<-sum((data$y-mean(data$y))^2)
p<-predict(model)
sshat<-sum((p-mean(p))^2)

test_that("tot", {
  expect_equal(sstot,yss)
  expect_equal(sshat,ssmod)
})


test_that("app", {
  expect_equal(sstot,sum(SS)+ssres)
})

}

one(N=20,b=0)


4.273+13.103+0.113

1.499+1.035+25.044+.201
