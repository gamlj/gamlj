context("gzlm")
gamlj_options("debug",FALSE)
data("hsbdemo")
data<-hsbdemo
test_that("gzlm logistic coherence",{
expect_error(gamlj::gamljGzlm(
  formula = ses ~ 1,
  data = data,
  modelSelection = "logistic")
)}
)
mod<-gamlj::gamljGzlm(
  formula = schtyp ~ write + honors + honors:write,
  data = data,
  modelSelection = "logistic")

res<-mod$main$fixed$asDF
test_that("glm estimates are correct", {
  expect_equal(as.character(res[3,1]), "honors1")
  expect_equal(round(res$cilow[2],2),-0.2)
  expect_equal(round(res$cihig[2],2),.17)
  expect_equal(round(res$p[2],2),0.81)
  expect_equal(round(as.numeric(as.character(mod$info$asDF[[2]][[6]])),3),0.046)
})

mod<-gamlj::gamljGLM(
  data = hsbdemo,
  formula=science~math+schtyp+math:schtyp,
  ciWidth=90,
  simpleVariable = "math",
  simpleModerator = "schtyp",
  plotHAxis = "math",
)

mod<-gamlj::gamljGzlm(
  formula = schtyp ~ write + honors + honors:write,
  data = data,
  modelSelection = "logistic",
  ciWidth=90,
  simpleVariable = "write",
  simpleModerator = "honors",
  plotHAxis = "write",
  )

r.anova<-mod$main$anova$asDF

test_that("gzlm anova is correct", {
  expect_equal(as.character(r.anova[3,1]),"honors:write")
  expect_equal(round(r.anova[1,4],3),0.809)
})

se.params<-mod$simpleEffects$Params$asDF
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

test_that("gzlm anova simple effects", {
  expect_equal(as.character(se.params[1,1]),"enrolled")
  expect_equal(round(se.params[2,4],3),-0.157)
})

test_that("gzlm plot", {
  expect_is(mod$descPlot,"Image")
})

test_that("gzlm CI width", {
  expect_equal(round(mod$main$fixed$asDF[2,5],3),0.468)
})

mod<-gamlj::gamljGzlm(
  formula = schtyp ~ write + honors + honors:write,
  data = data,
  modelSelection = "logistic",
  postHoc =  "honors"
)


test_that("glm posthoc", {
   expect_equal(round(mod$postHocs[[1]]$asDF[[5]],3),1.635)
  expect_equal(as.character(mod$postHocs[[1]]$asDF[[3]]),"not enrolled")
})

data<-hsbdemo
names(data)
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


