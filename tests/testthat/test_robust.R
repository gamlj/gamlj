library(GAMLj3)
testthat::context("model comparisons")
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
  testthat::expect_equal(mod1$main$coefficients$asDF[2,4], 0.0098, tolerance = tol)
  testthat::expect_gt(mod1$main$coefficients$asDF[2, 4],mod0$main$coefficients$asDF[2, 4])
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
