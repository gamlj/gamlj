tol<-.01

data <- readRDS("data/wlogistic_long.rds")

mod1<-GAMLj3::gamlj_glm(Y~X,model_type="logistic",data=data, weights=Counts)

testthat::test_that("weights", {
  testthat::expect_equal(mod1$main$r2$asDF[1,1],.11412,tol)
  testthat::expect_equal(mod1$main$anova$asDF$test,870.92,tol)
  testthat::expect_equal(mod1$main$coefficients$asDF$estimate[2],.1357,tol)

})

data <- readRDS("data/wlogistic_wide.rds")

mod2<-GAMLj3::gamlj_glm(cbind(Counts.1,Counts.0)~X,model_type="logistic",data=data)

testthat::test_that("cbind", {
  testthat::expect_equal(mod2$main$r2$asDF[1,1],.11412,tol)
  testthat::expect_equal(mod2$main$anova$asDF$test,870.92,tol)
  testthat::expect_equal(mod2$main$coefficients$asDF$estimate[2],.1357,tol)

})


mod3<-GAMLj3::gamlj_glm(p / tot ~X,model_type="logistic",data=data)

testthat::test_that("slash", {
  testthat::expect_equal(mod3$main$r2$asDF[1,1],.11412,tol)
  testthat::expect_equal(mod3$main$anova$asDF$test,870.92,tol)
  testthat::expect_equal(mod3$main$coefficients$asDF$estimate[2],.1357,tol)

})

data$X2<-data$X^2
mod4<-GAMLj3::gamlj_glm(p / tot ~X+X2,model_type="logistic",data=data,nested_terms=~1+X)

testthat::test_that("model comparison", {
  testthat::expect_true(!is.na(mod4$main$r2$asDF[3,2]))
})

