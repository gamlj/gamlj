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
data("hsbdemo")
mod<-gamlj::gamljGzlm(
  formula = schtyp ~ write + honors + honors:write,
  data = hsbdemo,
  showParamsCI = TRUE,
  modelSelection = "logistic")

res<-mod$main$coefficients$asDF
testthat::test_that("glm estimates are correct", {
  testthat::expect_equal(as.character(res[3,1]), "honors1")
  testthat::expect_equal(res$est.ci.lower[2],-0.20408,tolerance=tol)
  testthat::expect_equal(res$est.ci.upper[2],.1703,tolerance=tol)
  testthat::expect_equal(res$p[2],0.8079,tolerance=tol)
  testthat::expect_equal(mod$main$fit$asDF$value[2],175.787,tolerance=tol)
})


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
r.show<-as.character(mod$main$contrastCodeTables[[1]]$asDF[1,1])
testthat::test_that("gzlm anova is correct", {
  testthat::expect_equal(as.character(r.anova[3,1]),"write:honors")
  testthat::expect_equal(round(r.anova[1,4],3),0.809)
})
testthat::test_that("contrasts are correct", {
  testthat::expect_equal(r.show,"-0.5")
})

se.params<-mod$simpleEffects$coefficients$asDF


mod<-gamlj::gamljGzlm(
  formula = schtyp ~ write + honors + honors:write,
  data = hsbdemo,
  modelSelection = "logistic",
  posthoc =  "honors",
  plotHAxis = "write"
)


testthat::test_that("gzlm posthoc", {
  testthat::expect_equal(round(mod$posthoc[[1]]$asDF[[5]],3),1.635)
  testthat::expect_equal(as.character(mod$posthoc[[1]]$asDF[[3]]),"not enrolled")
})

res<-mod$main$coefficients$asDF

testthat::test_that("glm contrasts", {
  testthat::expect_equal(round(res[3,3],2),0.19)
  testthat::expect_equal(round(res[1,3],2),1.36)
})

testthat::test_that("gzlm plot", {
  testthat::expect_is(mod$mainPlots,"Image")
})

testthat::test_that("gzlm CI width", {
  testthat::expect_equal(mod$main$coefficients$asDF$est.ci.lower[2],-.204,tol)
})


mod<-gamlj::gamljGzlm(showParamsCI = F,
  formula = schtyp ~ 1,
  data = hsbdemo,
  modelSelection = "logistic")

testthat::test_that("intercept only works",
          testthat::expect_equal(round(mod$main$coefficients$asDF[1,3],digits=3),1.658)
)



data<-hsbdemo
names(data)[3]<-c("Gender (test ?)")
mod<-gamlj::gamljGzlm(
  formula=schtyp~math+`Gender (test ?)`+math:`Gender (test ?)`,
  data=data,
  modelSelection = "logistic",
  emmeans = ~`Gender (test ?)`)

testthat::test_that("glm weird names", {
  testthat::expect_equal(as.character(mod$main$coefficients$asDF[3,1]),"Gender (test ?)1")
})


res<-mod$emmeans[[1]]$asDF
testthat::test_that("glm EMM", {
  testthat::expect_equal(round(res[1,2],2),0.84)
})

mod<-gamlj::gamljGzlm(
  formula=prog~math+ses*female,
  data=hsbdemo,
  modelSelection = "multinomial",
  posthoc = ~ses:female)



testthat::test_that("Multinomial posthoc works", {
  testthat::expect_equal(mod$posthoc[[1]]$asDF$response[[1]],"academic")
  testthat::expect_equal(mod$posthoc[[1]]$asDF$estimate[8],.2101,tol)
  testthat::expect_equal(mod$posthoc[[1]]$asDF$response[[42]],"vocation")
  testthat::expect_equal(mod$posthoc[[1]]$asDF[42,1],"vocation")
  testthat::expect_equal(mod$posthoc[[1]]$asDF[42,2],"low")
  testthat::expect_equal(mod$posthoc[[1]]$asDF[42,3],"female")
  
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
data$age<-factor(data$age)

mod<-gamlj::gamljGzlm(
  formula=acts~agg_test*age,
  data=data,
  modelSelection = "poisson",
  simpleVariable = agg_test,
  simpleModerators = age,
  posthoc = ~age,
  emmeans =  ~agg_test)

mod
testthat::test_that("Poisson works", {

  testthat::expect_equal(mod$main$coefficients$asDF$expb[1],.1064,tol)
  testthat::expect_equal(mod$main$anova$asDF$test[1],68.38,tol)
  testthat::expect_equal(mod$main$coefficients$asDF$est.ci.lower[1],-3.578,tol)
  testthat::expect_equal(mod$main$r2$asDF$r2,.898,tol)
  testthat::expect_equal(mod$main$fit$asDF$value[4],9.82,tol)
  testthat::expect_equal(mod$emmeans[[1]]$asDF$est.ci.upper[2],.338,tol)
  testthat::expect_equal(mod$simpleEffects$anova$asDF$test[2],14.048,tol)
  testthat::expect_equal(mod$simpleEffects$coefficients$asDF$se[1],.0428,tol)
  testthat::expect_equal(mod$simpleEffects$coefficients$asDF$contrast[1],"agg_test")
  testthat::expect_equal(mod$posthoc[[1]]$asDF$estimate[2],.630,tol)
})

data$q<-data$acts+1
#contrasts(data$age)<-contr.sum(3)
#data$x<-data$agg_test-mean(data$agg_test)
#rmod<-glm(formula=q~x*age,data=data,family = Gamma())
mod<-gamlj::gamljGzlm(
  formula=q~agg_test*age,
  data=data,
  modelSelection = "custom",
  custom_family = "Gamma",
  custom_link = "inverse",
  simpleVariable = agg_test,
  simpleModerators = age,
  posthoc = ~age,
  emmeans =  ~agg_test
)

testthat::test_that("Custom model works", {
  testthat::expect_equal(mod$main$coefficients$asDF$expb[1],2.151,tol)
  testthat::expect_equal(mod$main$anova$asDF$test[1],168.42,tol)
  testthat::expect_equal(mod$main$coefficients$asDF$est.ci.lower[1],.695,tol)
  testthat::expect_equal(mod$main$r2$asDF$r2,.870,tol)
  testthat::expect_equal(mod$main$fit$asDF$value[4],2.538,tol)
  testthat::expect_equal(mod$emmeans[[1]]$asDF$est.ci.upper[2],1.448,tol)
  testthat::expect_equal(mod$simpleEffects$anova$asDF$test[2],75.937,tol)
  testthat::expect_equal(mod$simpleEffects$coefficients$asDF$se[1],.001,tol)
  testthat::expect_equal(mod$simpleEffects$coefficients$asDF$contrast[1],"agg_test")
  testthat::expect_equal(mod$posthoc[[1]]$asDF$estimate[2],.010,tol)
  testthat::expect_equal(mod$posthoc[[1]]$asDF$age_lev1[2],"1")
  
})


### negative binomial
data$q<-as.integer(data$q)
testthat::expect_warning({
  
mod<-gamlj::gamljGzlm(
  formula=q~agg_test*age,
  data=data,
  modelSelection = "nb",
  simpleVariable = agg_test,
  simpleModerators = age,
  posthoc = ~age,
  emmeans =  ~agg_test
  )
})

testthat::test_that("negative binomial model works", {
  testthat::expect_equal(mod$main$coefficients$asDF$expb[1],1.425,tol)
  testthat::expect_equal(mod$main$anova$asDF$test[1],22.6,tol)
  testthat::expect_equal(mod$main$coefficients$asDF$est.ci.lower[1],-.0845,tol)
  testthat::expect_equal(mod$main$r2$asDF$r2,.777,tol)
  testthat::expect_equal(mod$main$fit$asDF$value[4],8.89,tol)
  testthat::expect_equal(mod$emmeans[[1]]$asDF$est.ci.upper[2],2.08,tol)
  testthat::expect_equal(mod$simpleEffects$anova$asDF$test[2],5.91,tol)
  testthat::expect_equal(mod$simpleEffects$coefficients$asDF$se[1],.010,tol)
  testthat::expect_equal(mod$simpleEffects$coefficients$asDF$contrast[1],"agg_test")
  testthat::expect_equal(mod$posthoc[[1]]$asDF$estimate[2],1.056,tol)
  testthat::expect_equal(mod$posthoc[[1]]$asDF$age_lev1[2],"1")
  
})


### quasi poisson
data$q<-as.integer(data$q)
mod<-gamlj::gamljGzlm(
  formula=q~agg_test*age,
  data=data,
  modelSelection = "poiover",
  simpleVariable = agg_test,
  simpleModerators = age,
  posthoc = ~age,
  emmeans =  ~agg_test
)

testthat::test_that("quasi poisson binomial works", {
  testthat::expect_equal(mod$main$coefficients$asDF$expb[1],1.425,tol)
  testthat::expect_equal(mod$main$anova$asDF$test[1],105.641,tol)
  testthat::expect_equal(mod$main$coefficients$asDF$est.ci.lower[1],.167,tol)
  testthat::expect_equal(mod$main$r2$asDF$r2,.777,tol)
  testthat::expect_equal(mod$main$fit$asDF$value[4],8.893,tol)
  testthat::expect_equal(mod$emmeans[[1]]$asDF$est.ci.upper[2],1.698,tol)
  testthat::expect_equal(mod$simpleEffects$anova$asDF$test[2],27.64,tol)
  testthat::expect_equal(mod$simpleEffects$coefficients$asDF$se[1],.00475,tol)
  testthat::expect_equal(mod$simpleEffects$coefficients$asDF$contrast[1],"agg_test")
  testthat::expect_equal(mod$posthoc[[1]]$asDF$estimate[2],1.056,tol)
  
  })

