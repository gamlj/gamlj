testthat::context("effectsizes")
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


gmod<-GAMLj3::gamlj_lm(formula = form,
                      iris,
                      es = c("eta","etap","omega","omegap","epsilon","epsilonp"))

ganov<-gmod$main$anova$asDF
ganeff<-ganov[ganov$source!="Residuals" & ganov$source!="Model" & ganov$source!="Total",]
resss<-ganov$ss[ganov$source=="Residuals"]
totss<-ganov$ss[ganov$source=="Total"]
modss<-ganov$ss[ganov$source=="Model"]
df<-ganeff$df
dfe<-ganov$df[ganov$name=="Residuals"]
yss<-sum((iris$Sepal.Length-mean(iris$Sepal.Length))^2)
p<-predict(mod)
hatss<-sum((p-mean(p))^2)
etap<-ganeff$ss/(ganeff$ss+resss)

testthat::test_that("tot", {
  testthat::expect_equal(totss,yss)
  testthat::expect_equal(hatss,modss)
})
testthat::test_that("eta", {
  testthat::expect_equal(etap,ganeff$etaSqP)
})

testthat::test_that("app", {
  testthat::expect_true(totss>sum(ganeff$ss)+resss)
})

eta<-ganeff$ss/(totss)
testthat::test_that("eta", {
  testthat::expect_equal(eta,ganeff$etaSq)
})

#omega<-(ganeff$ss-(resss*df/dfe))/(totss+(resss/dfe))
omega<-(ganeff$ss-(resss*df/edf))/(modss+(resss*(edf+1)/edf))

testthat::test_that("omega", {
  testthat::expect_equal(omega,ganeff$omegaSq,tol=tol)
})

omegap<-(ganeff$ss-(resss*df/edf))/(ganeff$ss+(resss*(N-df)/edf))

testthat::test_that("omegap", {
  testthat::expect_equal(omegap,ganeff$omegaSqP)
})

epsilonp<-(ganeff$ss-(resss*df/edf))/(ganeff$ss+resss)
testthat::test_that("ciao", {
  testthat::expect_equal(epsilonp,ganeff$epsilonSqP)
})

epsilon<-(ganeff$ss-(resss*df/edf))/(modss+resss)
testthat::test_that("ciao", {
  testthat::expect_equal(epsilonp,ganeff$epsilonSqP)
})


