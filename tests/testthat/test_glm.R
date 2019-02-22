context("glm")
gamlj_options("debug",FALSE)
names(ToothGrowth)
mod<-gamlj::gamljGLM(
  data = ToothGrowth,
  dep = "len",
  factors = "supp",
  modelTerms = ~ supp,
  
)

res<-mod$main$fixed$asDF
params<-res$estimate
test_that("glm estimates are correct", {
  expect_equal(params[2], -1.85)
  expect_equal(round(res$cilow[2],2),-3.78)
  expect_equal(round(res$cilow[2],2),-3.78)
  expect_equal(round(res$p[2],2),0.06)
  expect_equal(round(as.numeric(as.character(mod$info$asDF[[2]][[3]])),3),0.059)
})

data("hsbdemo")
hsbdemo$schtyp
mod<-gamlj::gamljGLM(
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

mod<-gamlj::gamljGLM(
  data = hsbdemo,
  formula=science~math+schtyp+math:schtyp,
  postHoc = list("schtyp")
)

test_that("glm posthoc", {
   expect_equal(round(mod$postHocs[[1]]$asDF[[5]],3),1.528)
  expect_equal(as.character(mod$postHocs[[1]]$asDF[[3]]),"public")
})

data<-hsbdemo
names(data)[3]<-c("Gender (test ?)")
mod<-gamlj::gamljGLM(
  data = data,
  formula=science~math+`Gender (test ?)`+math:`Gender (test ?)`,
)

test_that("glm weired names", {
  expect_equal(as.character(mod$main$fixed$asDF[3,1]),"Gender (test ?)1")
})


expect_warning(mod<-gamlj::gamljGLM(
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

mod<-gamlj::gamljGLM(
  formula = science ~ math + schtyp + schtyp:math,
  data = data,
  contrasts = list(list(
      var="schtyp",
      type="deviation"))
)

res<-mod$main$fixed$asDF

test_that("glm contrasts", {
  expect_equal(round(res[3,3],2),-2.45)
  expect_equal(round(res[1,3],2),18.55)
})
