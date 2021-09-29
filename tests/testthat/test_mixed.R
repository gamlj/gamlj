context("mixed")

data("subjects_by_stimuli")

subjects_by_stimuli$cond<-factor(subjects_by_stimuli$cond)
formula<-y~1+cond+(1|subj)+(1|stimulus)
model<-gamlj::gamljMixed(
  formula =y ~ 1 + cond+( 1|subj ),
  data = subjects_by_stimuli,
  plotHAxis = cond,
  residPlot=T, randHist = T, clusterBoxplot = T
)
infotable<-model$info$asDF

testthat::test_that("info is ok", {
  testthat::expect_equal(round(as.numeric(as.character(infotable[3,2])),digits = 2),16561.78)
  testthat::expect_equal(round(as.numeric(as.character(infotable[6,2])),digits = 2),0.01)
  testthat::expect_equal(round(as.numeric(as.character(infotable[5,2])),digits = 2),-8278.23)
  
})

ftable<-model$main$anova$asDF

testthat::test_that("f-table is ok", {
  testthat::expect_equal(ftable[1,4],2949)
})

ptable<-model$main$fixed$asDF

testthat::test_that("p-table is ok", {
  testthat::expect_equal(round(ptable[1,4],digits = 2),0.31)
  testthat::expect_equal(round(ptable[2,5],digits = 3),0.703)
  testthat::expect_equal(as.character(ptable[2,2]),"1 - -1")
})

rtable<-model$main$random$asDF

testthat::test_that("p-table is ok", {
  testthat::expect_equal(round(rtable[1,4],digits = 2),4.49)
  testthat::expect_equal(as.character(rtable[1,"groups"]),"subj")
})

testthat::test_that("a descplot is produced", {
  testthat::expect_true(ggplot2::is.ggplot(plot(model)))
})

testthat::test_that("a  residplot is produced", {
  testthat::expect_true(ggplot2::is.ggplot(model$assumptions$residPlot$plot$fun()))
})

testthat::test_that("a  resid boxplot is produced", {
  testthat::expect_true(ggplot2::is.ggplot(model$assumptions$clusterBoxplot[[1]]$plot$fun()))
})

testthat::test_that("a randhist is produced", {
  testthat::expect_true(ggplot2::is.ggplot(model$assumptions$randHist[[1]]$plot$fun()))
})



model<-gamlj::gamljMixed(
  dep=y,
  factors = "cond",
  modelTerms = "cond",
  cluster = "subj",
  randomTerms = list(list(c("Intercept","subj"))),
  data = subjects_by_stimuli
)


ftable<-model$main$anova$asDF

testthat::test_that("list interface is ok", {
  testthat::expect_equal(ftable[1,4],2949)
})


testthat::test_that("uncorrelated error", {
  testthat::expect_error(  
    model<-gamlj::gamljMixed(
      dep=y,
      factors = "cond",
      modelTerms = "cond",
      cluster = list("subj","stimulus"),
      randomTerms = list(list(list("Intercept","subj")),list(list("cond","subj"),list("Intercept","stimulus"))),
      data = data
    )
    )
})


model1<-gamlj::gamljMixed(
  dep=y,
  factors = "cond",
  modelTerms = "cond",
  cluster = "subj",
  randomTerms = list(list(c("Intercept","subj")),list(c("cond","subj"))),
  data = subjects_by_stimuli
)

model2<-gamlj::gamljMixed(
   formula = y ~ 1 + cond+( 1 | subj )+( 0+cond | subj ),
   data = subjects_by_stimuli
 )

testthat::test_that("uncorrelated works", {
  testthat::expect_equal(as.character(model1$info$asDF[2,2]),as.character(model2$info$asDF[2,2]))
})

formula<-y~1+cond+(1+cond|subj)+(1|stimulus)
model<-gamlj::gamljMixed(
  formula =formula,
  data = subjects_by_stimuli, plotHAxis = cond,
  lrtRandomEffects=T  
)
testthat::test_that("ranova works",
                    testthat::expect_equal(model$main$lrtRandomEffectsTable$asDF[2,2],6)
)

testthat::test_that("mixed plot works",
                    testthat::expect_equal(model$main$lrtRandomEffectsTable$asDF[2,2],6)
)


adddata<-subjects_by_stimuli
adddata$x<-rnorm(length(adddata$nrow))

formula<-y~1+cond+x+(1+cond|subj)+(1|stimulus)
adddata$cond<-factor(adddata$cond)
model<-gamlj::gamljMixed(
  formula =formula,
  data = adddata, 
  scaling = c(x="standardized")
  
)

testthat::test_that("standardizing with more clusters",
                    testthat::expect_equal(as.character(model$main$fixed$asDF$source[3]),"x")
)

model<-gamlj::gamljMixed(
  formula =formula,
  data = adddata, 
  scaling = c("x"="clusterbasedstandardized")
)

testthat::test_that("standardizing with more clusters",
                    testthat::expect_equal(as.character(model$main$fixed$asDF$source[3]),"x")
)


data("beers_bars")
data<-beers_bars

model<-gamlj::gamljMixed(
  formula = smile ~ 1 + beer + I(beer^2)+( 1 + beer + I(beer^2) | bar ),
  data = data)
###### this has changed with lme4 1.1 
testthat::test_that("some poly", {
  testthat::expect_lt(model$main$anova$asDF[2,2],0.43)
  testthat::expect_gt(model$main$anova$asDF[2,2],0.31)
})


model<-gamlj::gamljMixed(
  formula = smile ~ 1 + beer +( 1 + beer  | bar ),
  data = data,
  scaling = list(list(
    var="beer",
    type="standardized")))

testthat::test_that("standardizing", {
  testthat::expect_equal(model$main$fixed$asDF[2,2],.8506,tolerance = .002)
})

model<-gamlj::gamljMixed(
  formula = smile ~ 1 + beer +( 1 + beer  | bar ),
  data = data,
  scaling = list(list(
    var="beer",
    type="clusterbasedstandardized")))

testthat::test_that("cluster-based-standardizing", {
  testthat::expect_equal(model$main$fixed$asDF[2,2],.6111,tolerance = .002)
})

model<-gamlj::gamljMixed(
  formula = smile ~ 1 + beer +( 1 + beer  | bar ),
  data = data,
  scaling = list(list(
    var="beer",
    type="clusterbasedcentered")))

testthat::test_that("cluster-based-centering", {
  testthat::expect_equal(model$main$fixed$asDF[2,2],.6070,tolerance = .002)
})


model<-gamlj::gamljMixed(
   formula =smile ~ 1 +(1|bar),
   data = data
 )
testthat::test_that("intercept only works",
   expect_equal(round(model$main$random$asDF[1,3],digits = 2),1.74)
)

data("subjects_by_stimuli")
subjects_by_stimuli$cond<-factor(subjects_by_stimuli$cond)
formula<-y~1+cond+(1+cond|subj)+(1|stimulus)
model<-gamlj::gamljMixed(
  formula =formula,
  data = subjects_by_stimuli, 
  lrtRandomEffects=T , 
  plotHAxis=cond,
  plotRandomEffects = T
)
testthat::test_that("ranova works",
                    testthat::expect_equal(model$main$lrtRandomEffectsTable$asDF[2,2],6)
)

testthat::test_that("plot works",{
                    testthat::expect_equal(model$main$lrtRandomEffectsTable$asDF[2,2],6)
                    testthat::expect_true(ggplot2::is.ggplot(plot(model)))
}
)


## simple effects and polynomial
data("wicksell")
data<-wicksell
data$group<-factor(data$group)
data$time<-factor(data$time)

testthat::expect_warning(
  gobj<-gamlj::gamljMixed(
  formula = dv ~ 1 + group + time + group:time+( 1 | subj ),
  data = data,
  contrasts = c("group"="simple","time"="polynomial"),
  simpleVariable = "time",
  simpleModerator = "group")
)

es.params<-gobj$simpleEffects$Params$asDF

testthat::test_that("simple effects", {
   testthat::expect_equal(as.character(es.params$contrast[[1]]),"linear")
  testthat::expect_equal(as.character(es.params$contrast[[2]]),"quadratic")
  testthat::expect_equal(round(es.params$estimate[3],digits=5),-7.32312)
 })

es.anova<-gobj$main$anova$asDF

gobj2<-gamlj::gamljMixed(
  formula = dv ~ 1 +group+ time:group+ time+( 1 | subj ),
  data = data, postHoc = list(c("time","group")))


ph<-gobj2$postHocs[[1]]$asDF
test<-ph[ph$c1=="0" & ph$c2=="1" & ph$c3=="6" & ph$c4=="1", ]

testthat::test_that("posthoc in mixed", {
  testthat::expect_equal(as.numeric(test[6]),155.5,tol=.00001)
  testthat::expect_equal(as.numeric(test[9]),66,tol=.00001)
})

es2.anova<-gobj2$main$anova$asDF

testthat::test_that("order does not count", {
  testthat::expect_equal(es2.anova[1,2],es.anova[1,2])

})



