testthat::context("glmixed")
tol=.001
data("glmmixeddata")
data<-glmmixeddata

names(data)
data$cluster<-factor(data$cluster)
data$w<-factor(data$w)
data$yord<-factor(data$yord)
data$ybin<-factor(data$ybin)


mod0 <- GAMLj3::gamlj_gmixed(
  formula = ybin~x*w+(1+x|cluster),
  data = data,
  model_type = "logistic"
)


mod1 <- GAMLj3::gamlj_gmixed(
  data=data,
  model_type = "logistic",
  dep = "ybin",
  factors = "w",
  covs = "x",
  model_terms = ~ x*w,
  re=list(list(list("Intercept","cluster"),list("x","cluster")))
)

mod2 <- GAMLj3::gamlj_gmixed(
  data=data,
  model_type = "logistic",
  dep = "ybin",
  factors = "w",
  covs = "x",
  model_terms = ~ x*w,
  re=~(1+x|cluster)
)
mod2$main$coefficients$asDF
testthat::test_that("equivalent model input", {
  testthat::expect_equal(mod0$info$asDF$specs[2], mod1$info$asDF$specs[2])
  testthat::expect_equal(mod0$main$anova$asDF$f[3], mod1$main$anova$asDF$f[3])
  testthat::expect_equal(mod0$main$coefficients$asDF$label[2], mod0$main$coefficients$asDF$label[2])
  testthat::expect_equal(mod0$main$coefficients$asDF$expb.ci.lower[1], mod0$main$coefficients$asDF$expb.ci.lower[1])
  testthat::expect_equal(mod0$main$coefficients$asDF$expb.ci.lower[1], mod2$main$coefficients$asDF$expb.ci.lower[1])
  testthat::expect_equal(mod1$main$anova$asDF$f[3], mod2$main$anova$asDF$f[3])
})



model <- GAMLj3::gamlj_gmixed(
  formula = ybin~x*w*z+(1+x|cluster),
  data = data,
  model_type = "logistic"
)
testthat::test_that("info is ok", {
  testthat::expect_equal(as.numeric(model$info$asDF$value[6]),1718)
  testthat::expect_equal(model$info$asDF$value[7],"yes")
})

testthat::test_that("R2 is ok", {
  testthat::expect_equal(model$main$r2$asDF[1,2],.3361,tol)
  testthat::expect_equal(model$main$r2$asDF[2,4],82.1,tol)
  
})

testthat::test_that("fit is ok", {
  testthat::expect_equal(model$main$fit$asDF[1,2],-931,tol)
  testthat::expect_equal(model$main$fit$asDF[4,2],1861.65,tol)
  
})

mod <- GAMLj3::gamlj_gmixed(
  formula = ypoi ~ x * w +(1+w|cluster),
  data = data,
  model_type = "poisson",
  simple_x = x,
  simple_mods = w,
  posthoc = ~w,
  emmeans = ~x,
  es=c("expb","marginals")
)

testthat::test_that("Poisson works", {
  testthat::expect_equal(mod$main$coefficients$asDF$expb[1], .8715, tol)
  testthat::expect_equal(mod$main$anova$asDF$test[1], 346.35, tol)
  testthat::expect_equal(mod$main$coefficients$asDF$expb.ci.lower[2], 1.085, tol)
  testthat::expect_equal(mod$main$r2$asDF$r2[1], .2769, tol)
  testthat::expect_equal(mod$main$fit$asDF$value[4], 4618.847, tol)
  testthat::expect_equal(mod$emmeans[[1]]$asDF$est.ci.upper[2], .9495, tol)
  testthat::expect_equal(mod$simpleEffects$anova$asDF$test[2], 184.78, tol)
  testthat::expect_equal(mod$simpleEffects$coefficients$asDF$se[1], .0087, tol)
  testthat::expect_equal(mod$simpleEffects$coefficients$asDF$contrast[1], "x")
  testthat::expect_equal(mod$posthoc[[1]]$asDF$estimate[1], .9767, tol)
  testthat::expect_equal(mod$main$marginals$asDF[2,3],-.0227,tol)
  testthat::expect_equal(mod$main$marginals$asDF[2,5],-.160,tol)
})


