context("gzlm")
data("hsbdemo")
tol<-0.001

testthat::test_that("gzlm logistic coherence",{
  testthat::expect_error(gamlj::gamljGzlm(
    formula = ses ~ 1,
    data = hsbdemo,
   modelSelection = "logistic")
)}
)
mod<-gamlj::gamljGzlm(
  formula = schtyp ~ write + honors + honors:write,
  data = hsbdemo,
  showParamsCI = TRUE,
  modelSelection = "logistic")

res<-mod$main$fixed$asDF
testthat::test_that("glm estimates are correct", {
  testthat::expect_equal(as.character(res[3,1]), "honors1")
  testthat::expect_equal(res$cilow[2],-0.20408,tolerance=tol)
  testthat::expect_equal(res$cihig[2],.1703,tolerance=tol)
  testthat::expect_equal(res$p[2],0.8079,tolerance=tol)
  testthat::expect_equal(as.numeric(as.character(mod$info$asDF[[2]][[6]])),0.0459,tolerance=tol)
})

mod<-gamlj::gamljGLM(
  data = hsbdemo,
  formula=science~math+schtyp+math:schtyp,
  ciWidth=90,
  simpleVariable = "math",
  simpleModerator = "schtyp",
  plotHAxis = "math"
)

mod<-gamlj::gamljGzlm(
  formula = schtyp ~ write + honors + honors:write,
  data = hsbdemo,
  modelSelection = "logistic",
  ciWidth=90,
  simpleVariable = "write",
  simpleModerator = "honors",
  plotHAxis = "write",
  showContrastCode=T
  )

r.anova<-mod$main$anova$asDF
r.show<-as.character(mod$main$contrastCodeTables[[1]]$asDF[1,3])
testthat::test_that("gzlm anova is correct", {
  testthat::expect_equal(as.character(r.anova[3,1]),"write:honors")
  testthat::expect_equal(round(r.anova[1,4],3),0.809)
})
testthat::test_that("contrasts are correct", {
  testthat::expect_equal(r.show,"-0.5")
})

se.params<-mod$simpleEffects$Params$asDF

mod<-gamlj::gamljGLM(
  formula = science ~ math + schtyp + schtyp:math,
  data = hsbdemo,
  contrasts = list(list(
    var="schtyp",
    type="deviation"))
)

res<-mod$main$fixed$asDF
library(testthat)
test_that("glm contrasts", {
  expect_equal(round(res[3,3],2),-0.11)
  expect_equal(round(res[1,3],2),51.96)
})
test_that("gzlm anova simple effects", {
  expect_equal(as.character(se.params[1,1]),"enrolled")
  expect_equal(round(se.params[2,4],3),0.915)
})

test_that("gzlm plot", {
  expect_is(mod$descPlot,"Image")
})

test_that("gzlm CI width", {
  expect_equal(round(mod$main$fixed$asDF[2,5],3),0.468)
})

mod<-gamlj::gamljGzlm(
  formula = schtyp ~ write + honors + honors:write,
  data = hsbdemo,
  modelSelection = "logistic",
  postHoc =  "honors"
)


test_that("gzlm posthoc", {
   expect_equal(round(mod$postHocs[[1]]$asDF[[5]],3),1.635)
   expect_equal(as.character(mod$postHocs[[1]]$asDF[[3]]),"not enrolled")
})


mod<-gamlj::gamljGzlm(showParamsCI = F,
  formula = schtyp ~ 1,
  data = hsbdemo,
  modelSelection = "logistic")

testthat::test_that("intercept only works",
          testthat::expect_equal(round(mod$main$fixed$asDF[1,2],digits=3),1.658)
)



data<-hsbdemo
names(data)[3]<-c("Gender (test ?)")
mod<-gamlj::gamljGzlm(
  formula=schtyp~math+`Gender (test ?)`+math:`Gender (test ?)`,
  data=data,
  modelSelection = "logistic",
  eDesc = T)

test_that("glm weird names", {
  expect_equal(as.character(mod$main$fixed$asDF[3,1]),"Gender (test ?)1")
})


res<-mod$emeansTables[[1]]$asDF
test_that("glm EMM", {
  expect_equal(round(res[1,2],2),0.84)
})

names(hsbdemo)
mod<-gamlj::gamljGzlm(
  formula=prog~math+ses*female,
  data=hsbdemo,
  modelSelection = "multinomial",
  eDesc = T)
mod
res1<-mod$main$fixed$asDF
res2<-mod$main$anova$asDF

testthat::test_that("Multinomial works", {
  testthat::expect_equal(round(res1[2,6],2),.92)
  testthat::expect_equal(res2[4,2],1.5950,tolerance=tol)
  
})

mod2<-gamlj::gamljGzlm(
  formula=prog~ses*female+math,
  data=hsbdemo,
  modelSelection = "multinomial")

res<-mod$main$anova$asDF
res2<-mod2$main$anova$asDF
testthat::test_that("glm order does not count", {
  testthat::expect_equal(res[1,2],res2[3,2])
  testthat::expect_equal(as.character(res[1,1]),as.character(res2[3,1]))
})


data("poissonacts")
data<-poissonacts
mod<-gamlj::gamljGzlm(
  formula=acts~agg_test,
  data=data,
  modelSelection = "poisson",
  eDesc = T)

res<-mod$main$anova$asDF$test
testthat::test_that("Poisson works", {
  testthat::expect_equal(round(res,2),85.92)
  testthat::expect_equal(res,85.9161,tolerance=tol)
  
})

data$q<-data$acts+1
mod<-gamlj::gamljGzlm(
  formula=q~agg_test,
  data=data,
  modelSelection = "custom",
  custom_family = "Gamma",
  custom_link = "inverse",
  eDesc = T)

res<-mod$main$fixed$asDF$expb[1]
testthat::test_that("Custom model works", {
  testthat::expect_equal(round(res,2),2.15)
})
