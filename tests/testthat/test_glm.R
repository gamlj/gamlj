context("glm")
mod<-gamlj::gamljGlm(
  data = ToothGrowth,
  dep = "len",
  factors = "supp",
  modelTerms = ~ supp,
)

res<-mod$main$fixed$asDF
params<-res$estimate
test_that("glm estimates are correct", {
  expect_equal(params[2], -3.70)
  expect_equal(round(res$cihig[2],2), 0.17)
  expect_equal(round(res$cilow[2],2),-7.57)
  expect_equal(round(res$p[2],2),0.06)
  expect_equal(round(as.numeric(as.character(mod$info$asDF[[2]][[3]])),3),0.059)
})

data("hsbdemo")

mod<-gamlj::gamljGlm(
  data = hsbdemo,
  formula=science~1
)

test_that("intercept only works",
          expect_equal(round(mod$main$fixed$asDF[1,2],digits=2),51.85)
          )

mod<-gamlj::gamljGlm(
  data = hsbdemo,
  formula=science~math+schtyp+math:schtyp,
  ciWidth=90,
  simpleVariable = "math",
  simpleModerator = "schtyp",
  plotHAxis = "math",
)

r.anova<-mod$main$anova$asDF

test_that("glm anova is correct", {
  expect_equal(as.character(r.anova[3,1]),"schtyp")
  expect_equal(round(r.anova[4,4],3),0.276)
})

se.params<-mod$simpleEffects$Params$asDF

test_that("glm anova simple effects", {
  expect_equal(as.character(se.params[1,1]),"private")
  expect_equal(round(se.params[2,4],3),0.554)
})

test_that("glm plot", {
  expect_is(mod$descPlot,"Image")
})

test_that("glm CI width", {
  expect_equal(round(mod$main$fixed$asDF[2,5],3),0.468)
})

hsbdemo$c1<-factor(rep(c(1,0),length(hsbdemo$id)/2))
hsbdemo$c2<-factor(rep(c(1,0),each=length(hsbdemo$id)/2))

mod<-gamlj::gamljGlm(
  data = hsbdemo,
  formula=science~c1*c2,
  postHoc = list("c1",c("c1","c2"))
)

test_that("glm labels do not square", {
  expect_equal(as.character(mod$main$fixed$asDF[4,1]),"c11:c21")
})

ph<-mod$postHocs
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
  postHoc = list("schtyp")
)

test_that("glm posthoc", {
   expect_equal(round(mod$postHocs[[1]]$asDF[[5]],3),1.528)
  expect_equal(as.character(mod$postHocs[[1]]$asDF[[3]]),"public")
})



mod<-gamlj::gamljGlm(
  data = hsbdemo,
  formula=science~math+schtyp+math:schtyp,
  effectSizeInfo = T
)
tab<-mod$main$effectSizeTable$asDF

testthat::test_that("glm effectsize", {
  testthat::expect_equal(tab[4,3],.2198,tol=.0001)
  testthat::expect_equal(tab[10,5],.029999,tol=.00001)
})



data<-hsbdemo
names(data)[3]<-c("Gender (test ?)")
mod<-gamlj::gamljGlm(
  data = data,
  formula=science~math+`Gender (test ?)`+math:`Gender (test ?)`,
  simpleModerator = `Gender (test ?)`,
  simpleVariable = math
)
se.params<-mod$simpleEffects$Params$asDF
testthat::test_that("glm weird names", {
  testthat::expect_equal(as.character(mod$main$fixed$asDF[3,1]),"Gender (test ?)1")
  testthat::expect_equal(as.character(se.params[2,1]),"male")
  testthat::expect_equal(round(se.params[1,5],digits=5),0.80708)
})

data$sex<-factor(data$`Gender (test ?)`,levels=c("male","female"))

mod2<-gamlj::gamljGlm(
  data = data,
  formula=science~math+sex+math:sex,
  simpleVariable = math,
  simpleModerator = sex
)

se.params2<-mod2$simpleEffects$Params$asDF
test_that("glm weird names", {
  expect_equal(as.character(se.params2[2,1]),"female")
  expect_equal(round(se.params2[1,5],digits=5),round(se.params[2,5],digits=5))
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


expect_warning(
  mod<-gamlj::gamljGlm(
  data = hsbdemo,
  formula=science~math+schtyp+math:schtyp,
  eDesc = T,
  normTest = T,
  homoTest = T
)
)
res<-mod$emeansTables[[1]]$asDF
test_that("glm EMM", {
  expect_equal(round(res[1,2],2),52.07)
})

res1<-mod$assumptions$homoTest$asDF
res2<-mod$assumptions$normTest$asDF

test_that("glm assumptions", {
  expect_equal(round(res1[1,4],2),0.13)
  expect_equal(round(res2[1,3],2),0.86)
})

mod<-gamlj::gamljGlm(
  formula = science ~ math + schtyp + schtyp:math,
  data = hsbdemo,
  contrasts = list(list(
      var="schtyp",
      type="deviation")),
  qq=T
)
res<-mod$main$fixed$asDF

testthat::test_that("glm contrasts", {
  testthat::expect_equal(round(res[3,3],2),-0.11)
  testthat::expect_equal(round(res[1,3],2),51.96)
})

plot<-mod$assumptions$qq$plot$fun()

testthat::test_that("glm assumptions plot", {
  testthat::expect_true(ggplot2::is.ggplot(plot))
})



mod<-gamlj::gamljGlm(
  formula = science ~ math + I(math^2),
  data = data
)

res<-mod$main$anova$asDF

test_that("glm contrasts", {
  expect_equal(as.character(res$name[3]),"math²")
  expect_equal(round(res[1,4],2),65.07)
})


mod<-gamlj::gamljGlm(
  formula = read ~ 1,  data = data
)
res<-mod$main$anova$asDF
test_that("glm intercept only model", {
  expect_equal(as.character(res$name[1]),"Residuals")
  expect_equal(round(res[2,2],2),20919.42)
})



mod<-gamlj::gamljGlm(
  formula = read ~ 1,  data = data,
  fixedIntercept=FALSE
)

res<-mod$main$anova$asDF
test_that("glm zero-intercept model", {
  expect_equal(as.character(res$name[1]),"Residuals")
  expect_equal(round(res[2,2],2),566514)
})




