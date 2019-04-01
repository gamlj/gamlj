context("mixed")
gamlj_options("debug",FALSE)

data("subjects_by_stimuli")
data<-subjects_by_stimuli
data$cond<-factor(data$cond)
formula<-y~1+cond+(1|subj)+(1|stimulus)
model<-gamlj::gamljMixed(
  formula =y ~ 1 + cond+( 1|subj ),
  data = data, plotHAxis = cond
)

model$postHocs
infotable<-model$info$asDF

test_that("r-squared is ok", {
  expect_equal(round(as.numeric(as.character(infotable[3,2])),digits = 2),16556.46)
  expect_equal(round(as.numeric(as.character(infotable[4,2])),digits = 2),0.01)
})

ftable<-model$main$anova$asDF

test_that("f-table is ok", {
  expect_equal(ftable[1,4],2949)
})

ptable<-model$main$fixed$asDF

test_that("p-table is ok", {
  expect_equal(round(ptable[1,4],digits = 2),0.31)
  expect_equal(as.character(ptable[2,2]),"1 - -1")
})

rtable<-model$main$random$asDF

test_that("p-table is ok", {
  expect_equal(round(rtable[1,4],digits = 2),4.49)
  expect_equal(as.character(rtable[1,"groups"]),"subj")
})

test_that("a plot is produced", {
  expect_equal(class(model$descPlot$plot)[1],"gg")
})

model<-gamlj::gamljMixed(
  dep=y,
  factors = "cond",
  modelTerms = "cond",
  cluster = "subj",
  randomTerms = list(list(c("Intercept","subj"))),
  data = data
)


ftable<-model$main$anova$asDF

test_that("list interface is ok", {
  expect_equal(ftable[1,4],2949)
})

model1<-gamlj::gamljMixed(
  dep=y,
  factors = "cond",
  modelTerms = "cond",
  cluster = "subj",
  randomTerms = list(list(c("Intercept","subj")),list(c("cond","subj"))),
  data = data
)

model<-gamlj::gamljMixed(
  dep=y,
  factors = "cond",
  modelTerms = "cond",
  cluster = "subj",
  randomTerms = list(list(c("Intercept","subj"),c("cond","subj"))),
  correlatedEffects = "nocorr",
  data = data
)
q<-list(list(c("Intercept","subj"),c("cond","subj")))

model$info$asDF

model2<-gamlj::gamljMixed(
  formula = y ~ 1 + cond+( 1 | subj )+( 0+cond | subj ),
  data = data
)

test_that("uncorrelated error", {
  expect_error(  
    model<-gamlj::gamljMixed(
      dep=y,
      factors = "cond",
      modelTerms = "cond",
      cluster = list("subj","stimulus"),
      randomTerms = list(list(list("Intercept","subj")),list(list("cond","subj"),list("Intercept","stimulus"))),
      data = data
    )
  ,"Correlated*")
})


test_that("uncorrelated works", {
  expect_equal(model1$info$asDF[2,2],model2$info$asDF[2,2])
})

data("beers_bars")
data<-beers_bars

model<-gamlj::gamljMixed(
  formula = smile ~ 1 + beer + I(beer^2)+( 1 + beer + I(beer^2) | bar ),
  data = data)
gamlj::gamljGLM(
  formula = smile ~ beer + I(beer^2),
  data = data)


# model<-gamlj::gamljMixed(
#   formula =y ~ 1 + cond+( 1|subj ),
#   data = data,  postHoc = list("cond")
# )
# potable<-model$postHocs[[1]]$asDF
# 
# test_that("posthoc are produced", {
#   expect_equal(round(potable[1,5],digits = 2),0.14)
# })
# 
