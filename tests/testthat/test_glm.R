context("glm")
tol<-.001

mod<-gamlj::gamljGlm(
  data = ToothGrowth,
  dep = "len",
  factors = "supp")
mod
res<-mod$main$coefficients$asDF
params<-res$estimate
testthat::test_that("glm estimates are correct", {
  testthat::expect_equal(params[2], -3.70)
  testthat::expect_equal(round(res$est.ci.upper[2],2), 0.17)
  testthat::expect_equal(round(res$est.ci.lower[2],2),-7.57)
  testthat::expect_equal(round(res$p[2],2),0.06)
  testthat::expect_equal(as.character(mod$info$asDF[[2]][[3]]),"Gaussian")
})

a<-mod$main$anova$asDF
resid<-a$ss[a$source=="Residuals"]
eff<-a$ss[a$source=="supp"]
peta<-eff/(eff+resid)

test_that("glm p eta2 are correct", {
  expect_equal(a$etaSqP[a$source=="supp"],peta)
})



data("hsbdemo")

mod<-gamlj::gamljGlm(
  data = hsbdemo,
  formula=science~1
)

testthat::test_that("intercept only works",
                    testthat::expect_equal(round(mod$main$coefficients$asDF[1,3],digits=2),51.85)
          )

mod<-gamlj::gamljGlm(
  data = hsbdemo,
  formula=science~math+schtyp+math:schtyp,
  ciWidth=90,
  simpleVariable = "math",
  simpleModerator = "schtyp",
  plotHAxis = "math",
  effectSize = c("eta","etap","omega","omegap")
)
mod$simpleEffects$anova$asDF
r.anova<-mod$main$anova$asDF

test_that("glm anova is correct", {
  expect_equal(as.character(r.anova[3,1]),"schtyp")
  expect_equal(round(r.anova[4,4],3),0.276)
  expect_equal(round(r.anova[3,6],5),7e-05)
  expect_equal(round(r.anova[3,8],5),-0.00299)
  expect_equal(round(r.anova[1,9],5),.38828)
  
})

se.params<-mod$simpleEffects$coefficients$asDF

test_that("glm anova simple effects", {
  expect_equal(as.character(se.params[1,1]),"private")
  expect_equal(round(se.params[2,5],3),0.574)
})

test_that("glm plot", {
  expect_is(mod$mainPlots[[1]],"Image")
})

test_that("glm CI width", {
  expect_equal(round(mod$main$coefficients$asDF[2,5],3),0.495)
})

mod<-gamlj::gamljGlm(
  data = hsbdemo,
  formula=science~math*schtyp*write,
  ci_width=90,
  simple_effects = "math",
  simple_moderators = list("schtyp","write"),
)

testthat::test_that("SE names are fine",{
         testthat::expect_equal(mod$simpleEffects$coefficients$asDF$mod_write[3],"Mean")
         testthat::expect_equal(mod$simpleEffects$anova$asDF$mod_write[3],"Mean")
}
)

mod<-gamlj::gamljGlm(
  data = hsbdemo,
  formula=science~math*schtyp*write,
  ciWidth=90,
  simpleVariable = "schtyp",
  simpleModerator = list("math","write"),
  simpleInteractions = T
)

testthat::test_that("SE multiple moderators iv=categorical",{
  testthat::expect_equal(mod$simpleEffects$coefficients$asDF$contrast[1],"public - private")
  testthat::expect_equal(mod$simpleEffects$anova$asDF$test[1],1.469,tol)
}
)
mod$simpleInteractions[[1]]$coefficients$asDF

testthat::test_that("simple interaction",{
  testthat::expect_equal(mod$simpleInteractions[[1]]$anova$asDF$effect[1],"schtyp:math")
  testthat::expect_equal(mod$simpleInteractions[[1]]$anova$asDF$test[3],.911,tol)
  testthat::expect_equal(mod$simpleInteractions[[1]]$coefficients$asDF$effect[1],"(public-private):math")
  testthat::expect_equal(mod$simpleInteractions[[1]]$coefficients$asDF$estimate[2],-.0543,tol)
  
}
)

hsbdemo$c1<-factor(rep(c(1,0),length(hsbdemo$id)/2))
hsbdemo$c2<-factor(rep(c(1,0),each=length(hsbdemo$id)/2))

mod<-gamlj::gamljGlm(
  data = hsbdemo,
  formula=science~c1*c2,
  posthocCorr = "bonf",
  posthoc =  list("c1",c("c1","c2"))
)

test_that("glm labels do not square", {
  expect_equal(as.character(mod$main$coefficients$asDF[4,1]),"c11:c21")
})

ph<-mod$posthoc
ph1<-ph[[1]]$asDF
ph2<-ph[[2]]$asDF

testthat::test_that("postoh in glm", {
  testthat::expect_equal(as.character(ph1[1,1]),"0")
  testthat::expect_equal(ph1[1,4],0.52)
  testthat::expect_equal(as.character(ph2[4,1]),"0")
  testthat::expect_equal(ph2[6,6],13.46)
  
})


mod<-gamlj::gamljGlm(
  data = hsbdemo,
  formula=science~math+schtyp+math:schtyp,
  posthoc  = list("schtyp")
)
mod$posthoc[[1]]$asDF
test_that("glm posthoc", {
  expect_equal(round(mod$posthoc[[1]]$asDF[[5]],3),1.528)
  expect_equal(as.character(mod$posthoc[[1]]$asDF[[3]]),"public")
})


mod<-gamlj::gamljGlm(
  data = hsbdemo,
  formula=science~math+schtyp+math:schtyp,
  effectSizeInfo = T
)
tab<-mod$main$effectsizes$asDF

testthat::test_that("glm effectsize", {
  testthat::expect_equal(tab[4,3],.21724,tol=.0001)
  testthat::expect_equal(tab[10,5],.0,tol=.00001)
})


data<-hsbdemo

names(data)[3]<-c("Gender (test ?)")
mod<-gamlj::gamljGlm(
  data = data,
  formula=science~math+`Gender (test ?)`+math:`Gender (test ?)`,
  simpleModerator = `Gender (test ?)`,
  simpleVariable = math,
  posthoc = "Gender (test ?)"
)

se.params<-mod$simpleEffects$coefficients$asDF


testthat::test_that("glm weird names", {
  testthat::expect_equal(as.character(mod$main$coefficients$asDF[3,1]),"Gender (test ?)1")
  testthat::expect_equal(as.character(se.params[2,1]),"male")
  testthat::expect_equal(round(se.params[1,6],digits=5),0.80708)
})

data$sex<-factor(data$`Gender (test ?)`,levels=c("male","female"))

mod2<-gamlj::gamljGlm(
  data = data,
  formula=science~math+sex+math:sex,
  simpleVariable = math,
  simpleModerator = sex
)

se.params2<-mod2$simpleEffects$coefficients$asDF
testthat::test_that("glm weird names", {
  testthat::expect_equal(as.character(se.params2[2,1]),"female")
  testthat::expect_equal(round(se.params2[1,5],digits=5),round(se.params[2,5],digits=5))
})

mod3<-gamlj::gamljGlm(
  data = data,
  formula=science~math+math:`Gender (test ?)`+`Gender (test ?)`,
  simpleModerator = `Gender (test ?)`,
  simpleVariable = math
)

res<-mod$main$anova$asDF
res3<-mod3$main$anova$asDF

testthat::test_that("glm order does not count", {
  testthat::expect_equal(res[2,4],res3[2,4])
  testthat::expect_equal(as.character(res[4,1]),as.character(res3[4,1]))
})


data("hsbdemo")
data<-hsbdemo
levels(data$ses)<-c("s1","s2","s3")
levels(data$female)<-c("f1","f2")

ok<-gamlj::gamljGlm(
  data = data,
  formula=science~math*ses*female,
  posthoc = list(c("ses","female")),
  posthocEffsize = c("dm")
  
)
ok$posthoc[[1]]
ok$posthoc[[1]]$asDF
ok$posthocEffsize[[1]]$asDF

data$`weird ?`<-hsbdemo$ses
data$`weird !`<-hsbdemo$female
levels(data$`weird !`)<-c("a!","b!")
levels(data$`weird ?`)<-c("a?","b?","c?")

weird<-gamlj::gamljGlm(
  data = data,
  formula=science~math*`weird !`*`weird ?`,
  simpleModerator = list("weird !","weird ?"),
  simpleVariable = math,
  posthoc = list(c("weird !","weird ?"))
)

wres<-weird$posthoc[[1]]$asDF

testthat::test_that("weird posthoc", {
  testthat::expect_equal(wres[2,4],"c?")
  testthat::expect_equal(as.character(wres[10,1]),as.character(wres[15,4]))
})



mod<-gamlj::gamljGlm(
  data = hsbdemo,
  formula=science~math+schtyp+math:schtyp, 
  emmeans = ~schtyp,
  homoTest = T,
  normTest = T
)
res<-mod$emmeans[[1]]$asDF
testthat::test_that("glm EMM", {
  testthat::expect_equal(round(res[1,2],2),52.07)
})

res1<-mod$assumptions$homotest$asDF
res2<-mod$assumptions$normtest$asDF

testthat::test_that("glm assumptions", {
  testthat::expect_equal(round(res1[1,5],2),0.13)
  testthat::expect_equal(round(res2[1,3],2),0.86)
})


mod<-gamlj::gamljGlm(
  formula = science ~ math + schtyp + schtyp:math,
  data = hsbdemo,
  contrasts = list(list(
      var="schtyp",
      type="deviation")),
  qqplot = T
)
res<-mod$main$coefficients$asDF
testthat::test_that("glm contrasts", {
  testthat::expect_equal(round(res[3,3],2),-0.11)
  testthat::expect_equal(round(res[1,3],2),51.96)
})

plot<-mod$assumptions$qqplot$plot$fun()
testthat::test_that("glm assumptions plot", {
  testthat::expect_true(ggplot2::is.ggplot(plot))
})



mod<-gamlj::gamljGlm(
  formula = science ~ math + I(math^2),
  data = data
)

res<-mod$main$anova$asDF

testthat::test_that("glm contrasts", {
  testthat::expect_equal(as.character(res$source[3]),"math²")
  testthat::expect_equal(round(res[1,4],2),65.07)
})


mod<-gamlj::gamljGlm(
  formula = read ~ 1,  data = data
)
res<-mod$main$anova$asDF
testthat::test_that("glm intercept only model", {
  testthat::expect_equal(as.character(res$source[1]),"Residuals")
  testthat::expect_equal(round(res[2,2],2),20919.42)
})



mod<-gamlj::gamljGlm(
  formula = read ~ 1,  data = data,
  fixedIntercept=FALSE
)

res<-mod$main$anova$asDF
testthat::test_that("glm zero-intercept model", {
  testthat::expect_equal(as.character(res$source[1]),"Residuals")
  testthat::expect_equal(round(res[2,2],2),566514)
})


### bootstrap
options("digits"=2)
mod<-gamlj::gamljGlm(
  data = hsbdemo,
  formula=science~math+prog+math:prog, 
  emmeans = ~prog,
  betas_ci=T,
  ci_method = "bcai"
)


testthat::test_that("bootstrap ci make sense", {
  testthat::expect_true(mod$main$coefficients$asDF[["est.ci.lower"]][3]<0)
  testthat::expect_true(mod$main$coefficients$asDF[["est.ci.lower"]][1]>0)
})

testthat::test_that("glm zero-intercept model", {
  testthat::expect_true(abs(mod$main$coefficients$asDF[["beta.ci.lower"]][3])<1)
  testthat::expect_true(abs(mod$main$coefficients$asDF[["beta.ci.lower"]][1])<1)
})

