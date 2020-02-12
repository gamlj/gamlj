context("R interface")

data("qsport")
obj<-gamlj::gamljGLM(
    formula = performance ~ hours,
    data = qsport)
preds<-gamlj_predict(obj)

testthat::test_that("glm predict", {
  testthat::expect_equal(round(mean(preds),2),37.88)
})


data("schoolexam")

mod<-gamlj::gamljGlmMixed(
  formula = pass ~ 1 + math+( 1 | school ),
  data = schoolexam,
  plotHAxis = math,
  correlatedEffects = "nocorr",
  cimethod = "wald")

preds<-gamlj_predict(mod)

testthat::test_that("glmixed predict", {
  testthat::expect_equal(round(mean(preds),2),0.04)
})

data("subjects_by_stimuli")

mod<-gamlj::gamljMixed(
  formula =y ~ 1 + cond+( 1|subj ),
  data = subjects_by_stimuli
)

preds<-gamlj_predict(mod)

testthat::test_that("mixed predict", {
  testthat::expect_equal(round(mean(preds),2),19.6)
})


data("hsbdemo")
mod<-gamlj::gamljGzlm(
  formula = schtyp ~ write + honors + honors:write,
  data = hsbdemo,
  showParamsCI = TRUE,
  modelSelection = "logistic")
preds<-gamlj_predict(mod)

testthat::test_that("mixed predict", {
  testthat::expect_equal(round(mean(preds),2),1.78)
})
