library(gamlj)
testthat::context("model comparisons")
tol=.001
data("glmmixeddata")
data<-glmmixeddata

data$cluster<-factor(data$cluster)
data$w<-factor(data$w)
data$yord<-factor(data$yord)
data$ybin<-factor(data$ybin)


mod0 <- gamlj::gamljGlm(
  formula = ycont~x,
  data = data
)

mod1 <- gamlj::gamljGlm(
  formula = ycont~x*w,
  data = data,
)

a<-anova(mod1)

testthat::test_that("test glm anova", {
  testthat::expect_equal(a[[1]]$asDF$r2,.3941,tol)
  testthat::expect_equal(a[[2]]$asDF$f[3],106.018,tol)
})

a<-anova(mod1,mod0)

testthat::test_that("test glm anova comparison", {
  testthat::expect_equal(a$asDF$f[3],179.192,tol)
  testthat::expect_equal(a$asDF$r2[3],.1266,tol)
})

mod <- gamlj::gamljGlm(
  formula = ycont~x*w,
  data = data,
  nested_terms = ~x
)

testthat::test_that("test glm anova comparison option", {
  testthat::expect_true(all(mod$main$r2$asDF$ar2==a$asDF$ar2))
})

