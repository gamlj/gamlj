library(GAMLj3)
tol <- .001
data("clustermanymodels")
data <- clustermanymodels

mod0 <- GAMLj3::gamlj_lm(
  formula = ycont ~ x ,
  data = data)
mod1 <- GAMLj3::gamlj_lm(
  formula = ycont ~ x ,
  data = data,
  se_method="HC3")


testthat::test_that("test lm robust SE", {
  testthat::expect_equal(mod1$main$coefficients$asDF[2,4], 0.0644, tolerance = tol)
  testthat::expect_gt(mod1$main$coefficients$asDF[2, 4],mod0$main$coefficients$asDF[2, 4])
})

data$ybin<-factor(data$ybin)
mod0 <- GAMLj3::gamlj_glm(
  formula = ybin ~ x ,
  data = data,
  model_type = "logistic")

mod1 <- GAMLj3::gamlj_glm(
  formula = ybin ~ x ,
  data = data,
  model_type = "logistic",
  se_method="HC3")


testthat::test_that("test glm robust SE", {
  testthat::expect_equal(mod1$main$coefficients$asDF[2,4], 0.00982, tolerance = tol)
  testthat::expect_gt(mod1$main$coefficients$asDF[2, 4],mod0$main$coefficients$asDF[2, 4])
})

testthat::test_that("GLM robust CIs use the robust covariance matrix", {
  mod <- suppressWarnings(GAMLj3::gamlj_glm(
    formula = ybin ~ x,
    data = data,
    model_type = "logistic",
    se_method = "HC3",
    estimates_ci = TRUE,
    es = "expb",
    ci_method = "wald"
  ))

  tab <- mod$main$coefficients$asDF
  ci <- parameters::ci(mod$model, method = "wald", ci = .95, vcov = "HC3")

  testthat::expect_equal(tab$est.ci.lower, ci$CI_low, tolerance = 1e-10)
  testthat::expect_equal(tab$est.ci.upper, ci$CI_high, tolerance = 1e-10)
  testthat::expect_equal(tab$expb.ci.lower, exp(ci$CI_low), tolerance = 1e-10)
  testthat::expect_equal(tab$expb.ci.upper, exp(ci$CI_high), tolerance = 1e-10)
})


data$ybeta<-data$ypoi/55
mod0 <- GAMLj3::gamlj_glm(
  formula = ybeta ~ x ,
  data = data,
  model_type = "beta")

mod1 <- GAMLj3::gamlj_glm(
  formula = ybeta ~ x ,
  data = data,
  model_type = "beta",
  se_method="HC3")

testthat::test_that("test beta no robust", {
  testthat::expect_equal(mod1$main$coefficients$asDF[2, 4],mod0$main$coefficients$asDF[2, 4])
})
