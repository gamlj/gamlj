testthat::context("gzlmixed")
tol=.001
data("glmmixeddata")
data<-glmmixeddata

names(data)
data$cluster<-factor(data$cluster)
data$w<-factor(data$w)
data$yord<-factor(data$yord)
data$ybin<-factor(data$ybin)


mod0 <- gamlj::gamljGlmMixed(
  formula = ybin~x*w+(1+x|cluster),
  data = data,
  model_type = "logistic"
)


mod1 <- gamlj::gamljGlmMixed(
  data=data,
  model_type = "logistic",
  dep = "ybin",
  factors = "w",
  covs = "x",
  model_terms = ~ x*w,
  re=list(list(list("Intercept","cluster"),list("x","cluster")))
)

mod2 <- gamlj::gamljGlmMixed(
  data=data,
  model_type = "logistic",
  dep = "ybin",
  factors = "w",
  covs = "x",
  model_terms = ~ x*w,
  re=~(1+x|cluster)
)

testthat::test_that("equivalent model input", {
  testthat::expect_equal(mod0$info$asDF$specs[2], mod1$info$asDF$specs[2])
  testthat::expect_equal(mod0$main$anova$asDF$f[3], mod1$main$anova$asDF$f[3])
  testthat::expect_equal(mod0$main$coefficients$asDF$label[2], mod0$main$coefficients$asDF$label[2])
  testthat::expect_equal(mod0$main$coefficients$asDF$est.ci.lower[1], mod0$main$coefficients$asDF$est.ci.lower[1])
  testthat::expect_equal(mod0$main$coefficients$asDF$est.ci.lower[1], mod2$main$coefficients$asDF$est.ci.lower[1])
  testthat::expect_equal(mod1$main$anova$asDF$f[3], mod2$main$anova$asDF$f[3])
})



model <- gamlj::gamljGlmMixed(
  formula = ybin~x*w*z+(1+x|cluster),
  data = data,
  model_type = "logistic"
)
testthat::test_that("info is ok", {
  testthat::expect_equal(as.numeric(infotable$value[7]),3000)
  testthat::expect_equal(infotable$info[8],"Converged")
})

testthat::test_that("R2 is ok", {
  testthat::expect_equal(model$main$r2$asDF[1,2],.4563,tol)
  testthat::expect_equal(model$main$r2$asDF[2,4],242.719,tol)
  
})

testthat::test_that("fit is ok", {
  testthat::expect_equal(model$main$fit$asDF[1,2],-850.528,tol)
  testthat::expect_equal(model$main$fit$asDF[4,2],1701.056,tol)
  
})
