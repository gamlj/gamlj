testthat::context("contrasts")
tol<-.001

data("clustermanymodels")
data<-clustermanymodels
data$cat2<-factor(data$cat2)
data$cat3<-factor(data$cat3)
data$ybin<-factor(data$ybin)
data$ycat<-factor(data$ycat)

mod<-GAMLj3::gamlj_lm(
  data = data,
  formula=ycont~cat3*cat2+x,
  contrasts=list(cat3="custom"),
  contrast_custom_values=list(cat3=c(2,-1,-1))
)

testthat::test_that("constrast in lm makes sense", {
  testthat::expect_equal(mod$main$contrasts$asDF[["se"]][1], 0.1993333 ,tolerance=tol)
  testthat::expect_equal(mod$main$contrasts$asDF[["label"]][1],"{ 2*-1, -1*0, -1*1 }")
})


mod<-GAMLj3::gamlj_glm(
  data = data,
  formula=ybin~cat3*cat2+x,
  model_type="logistic",
  contrasts=list(cat3="custom"),
  contrast_custom_values=list(cat3=c(2,-1,-1)),
  simple_x="cat3",
  simple_mods="cat2"

)

testthat::test_that("constrast in glm makes sense", {
  testthat::expect_equal(mod$main$contrasts$asDF[["se"]][1], 0.03414783,tolerance=tol)
  testthat::expect_equal(mod$main$contrasts$asDF[["label"]][1],"{ 2*-1, -1*0, -1*1 }")
  testthat::expect_equal(mod$simpleEffects$coefficients$asDF$contrast[1],"{ 2*-1, -1*0, -1*1 }")
  testthat::expect_equal(mod$simpleEffects$coefficients$asDF$estimate[1],0.07203, tol)
})


mod<-GAMLj3::gamlj_glm(
  data = data,
  formula=ycat~cat3*cat2+x,
  model_type="multinomial",
  contrasts=list(cat3="custom"),
  contrast_custom_values=list(cat3=c(2,-1,-1)),
  contrast_custom_focus=T,
  simple_x="cat3",
  simple_mods="cat2"
)


testthat::test_that("constrast in glm multinomial makes sense", {
  testthat::expect_equal(mod$main$contrasts$asDF[["se"]][1], 0.04190,tolerance=tol)
  testthat::expect_equal(mod$main$contrasts$asDF[["label"]][1],"{ 2*-1, -1*0, -1*1 }")
})



mod<-GAMLj3::gamlj_mixed(
  data = data,
  formula=ycont~cat3*cat2+x+(1|cluster),
  contrasts=list(cat3="custom"),
  contrast_custom_values=list(cat3=c(2,-1,-1))
)

testthat::test_that("constrast in mixed makes sense", {
  testthat::expect_equal(mod$main$contrasts$asDF[["se"]][1], 0.19644,tolerance=tol)
  testthat::expect_equal(mod$main$contrasts$asDF[["label"]][1],"{ 2*-1, -1*0, -1*1 }")
})




library(lmerTest)

mod<-GAMLj3::gamlj_gmixed(
  data = data,
  formula=ycat~cat3*cat2+(1|cluster),
  model_type="multinomial",
  contrasts=list(cat3="custom"),
  contrast_custom_values=list(cat3=c(2,-1,-1))
)

testthat::test_that("constrast in gmixed makes sense", {
  testthat::expect_equal(mod$main$contrasts$asDF[["se"]][1], 0.04200,tolerance=tol)
  testthat::expect_equal(mod$main$contrasts$asDF[["label"]][1],"{ 2*-1, -1*0, -1*1 }")
})

