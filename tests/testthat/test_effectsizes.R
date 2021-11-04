context("effectsizes")
tol<-.0001
N<-dim(iris)[1]
form<-Sepal.Length~Sepal.Width+Petal.Width+Species
mod<-lm(form,data=iris)
sumr<-summary(mod)
f<-sumr$fstatistic[[1]]
edf<-sumr$fstatistic[[3]]
mdf<-sumr$fstatistic[[2]]
modss<-f*sigma(mod)^2*mdf/edf
.anova<-car::Anova(mod,type=3)


gmod<-gamlj::gamljGlm(formula = form,iris,effectSize = c("eta","etap","omega","omegap","epsilon","epsilonp"))
gmod
ganov<-gmod$main$anova$asDF
ganeff<-ganov[ganov$name!="Residuals" & ganov$name!="Model" & ganov$name!="Total",]
resss<-ganov$ss[ganov$name=="Residuals"]
totss<-ganov$ss[ganov$name=="Total"]
modss<-ganov$ss[ganov$name=="Model"]
df<-ganeff$df
dfe<-ganov$df[ganov$name=="Residuals"]
yss<-sum((iris$Sepal.Length-mean(iris$Sepal.Length))^2)
p<-predict(mod)
hatss<-sum((p-mean(p))^2)
etap<-ganeff$ss/(ganeff$ss+resss)

test_that("tot", {
  expect_equal(totss,yss)
  expect_equal(hatss,modss)
})
test_that("eta", {
  expect_equal(etap,ganeff$etaSqP)
})

test_that("app", {
  expect_true(totss>sum(ganeff$ss)+resss)
})

eta<-ganeff$ss/(totss)
test_that("eta", {
  expect_equal(eta,ganeff$etaSq)
})

#omega<-(ganeff$ss-(resss*df/dfe))/(totss+(resss/dfe))
omega<-(ganeff$ss-(resss*df/dfe))/(modss+(resss*(dfe+1)/dfe))

test_that("omega", {
  expect_equal(omega,ganeff$omegaSq,tol=tol)
})

omegap<-(ganeff$ss-(resss*df/dfe))/(ganeff$ss+(resss*(N-df)/dfe))

test_that("omegap", {
  expect_equal(omegap,ganeff$omegaSqP)
})

epsilonp<-(ganeff$ss-(resss*df/dfe))/(ganeff$ss+resss)
test_that("ciao", {
  expect_equal(epsilonp,ganeff$epsilonSqP)
})

epsilon<-(ganeff$ss-(resss*df/dfe))/(modss+resss)
test_that("ciao", {
  expect_equal(epsilonp,ganeff$epsilonSqP)
})


