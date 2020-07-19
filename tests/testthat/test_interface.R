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

newopt<-list(list(
    var="hours",
    type="standardized") 
    )
qsport$z<-as.numeric(scale(qsport$performance))
zobj<-gamlj::gamljGLM(
  formula = z ~ hours,
  data = qsport,
  scaling=newopt)
cc<-zobj$main$fixed$asDF[2,]  
testthat::test_that("standardizing", {
  testthat::expect_equal(cc[[2]],cc[[6]])
})

upd<-gamlj_update(obj,scaling=newopt,effectSize = c("beta", "partEta", "omega"))
res1<-upd$main$fixed$asDF
res2<-upd$main$anova$asDF
testthat::test_that("updating", {
  testthat::expect_equal(round(res1[2,2],2),4.42)
  testthat::expect_equal(round(res2[1,7],2),0.39)
  
})



data("schoolexam")
mod<-gamlj::gamljGlmMixed(
  formula = pass ~ 1 + math+( 1 | school ),
  data = schoolexam,
  plotHAxis = math,
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
  modelSelection = "logistic")

preds<-gamlj_predict(mod)
dd<-gamlj_data(mod)

testthat::test_that("mixed ", {
  testthat::expect_equal(round(mean(preds),2),1.78)
  testthat::expect_equal(round(mean(dd$write),2),0)
  
})


se<-gamlj_simpleEffects(mod,variable="write",moderator="honors")
se
res<-se$simpleEffects$Anova$asDF
testthat::test_that("simple effects ", {
  testthat::expect_equal(round(res[2,2],2),6.64)
  testthat::expect_equal(round(res[2,3],2),1)
})
