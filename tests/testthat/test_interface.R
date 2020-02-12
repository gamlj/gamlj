context("R interface")

data("qsport")
obj<-gamlj::gamljGLM(
    formula = performance ~ hours,
    data = qsport)
preds<-gamlj_predict(obj)
n<-dim(gamlj_data(obj))[1]

testthat::test_that("test glm", {
  testthat::expect_equal(round(mean(preds),2),37.88)
  testthat::expect_equal(n,100)
  
})


data("schoolexam")

mod<-gamlj::gamljGlmMixed(
  formula = pass ~ 1 + math+( 1 | school ),
  data = schoolexam,
  plotHAxis = math,
  correlatedEffects = "nocorr",
  cimethod = "wald")

preds<-gamlj_predict(mod)
n<-dim(gamlj_data(mod))[1]
testthat::test_that("glmixed predict", {
  testthat::expect_equal(round(mean(preds),2),0.04)
  testthat::expect_equal(n,5041)
  
})

data("beers_bars")


data("subjects_by_stimuli")

mod<-gamlj::gamljMixed(
  formula =y ~ 1 + cond+( 1|subj ),
  data = subjects_by_stimuli
)

preds<-gamlj_predict(mod)
n<-dim(gamlj_data(mod))[1]

testthat::test_that("mixed predict", {
  testthat::expect_equal(round(mean(preds),2),19.6)
  testthat::expect_equal(n,3000)
  
})


data("hsbdemo")
mod<-gamlj::gamljGzlm(
  formula = schtyp ~ write + honors + honors:write,
  data = hsbdemo,
  showParamsCI = TRUE,
  modelSelection = "logistic",
  plotHAxis = write)

preds<-gamlj_predict(mod)
dd<-gamlj_data(mod)


testthat::test_that("mixed predict", {
  testthat::expect_equal(round(mean(preds),2),1.78)
})
