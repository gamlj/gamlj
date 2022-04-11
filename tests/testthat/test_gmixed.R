context("gzlmixed")
data("schoolexam")
mod<-gamlj::gamljGlmMixed(
  formula = pass ~ 1 + math*activity+( 1 +math| school ),
  data = schoolexam,
  plotHAxis = math,
  correlatedEffects = "nocorr",
  cimethod = "wald")


model<-gamlj_model(mod)

val1<-round(as.numeric(as.character(mod$info$asDF$value[6])),digits = 3)
val2<-round(as.numeric(as.character(mod$info$asDF$value[9])),digits = 3)
val3<-as.numeric(as.character(mod$info$asDF$value[7]))
val4<-as.numeric(as.character(mod$info$asDF$value[14]))

testthat::test_that("info table is fine",{
                    testthat::expect_equal(val1,-2785.064)
                    testthat::expect_equal(val2, 0.039)
                    testthat::expect_equal(val3, 5570.128,tolerance = .001)
                    testthat::expect_equal(val4, 0.9566,tolerance = .001)
                    
                    })

val1<-round(as.numeric(as.character(mod$main$anova$asDF$test[[1]])),digits = 3)
val2<-round(as.numeric(as.character(mod$main$anova$asDF$p[[2]])),digits = 5)
val3<-as.character(mod$main$anova$asDF$name[1])

testthat::test_that("anova table is fine",{
  testthat::expect_equal(val1,95.125)
  testthat::expect_equal(val2, 0)
  testthat::expect_equal(val3, "math")
  
})

val1<-round(as.numeric(as.character(mod$main$fixed$asDF[1,5])),digits = 4)
val2<-round(as.numeric(as.character(mod$main$fixed$asDF[2,7])),digits = 4)
val3<-as.character(mod$main$fixed$asDF[2,1])

testthat::test_that("params table is fine",{
  testthat::expect_equal(val1,0.7104)
  testthat::expect_equal(val2, 9.7532)
  testthat::expect_equal(val3, "math")
  
})

val1<-round(as.numeric(as.character(mod$main$random$asDF[1,3])),digits = 3)

testthat::test_that("vars table is fine",{
  testthat::expect_equal(val1,1.34)

})


mod<-gamlj::gamljGlmMixed(
  formula = pass ~ 1 + math*activity+( 1 | school ),
  data = schoolexam,
  plotHAxis = math,
  correlatedEffects = "nocorr",
  cimethod = "wald",
  scaling=c(math="clusterbasedcentered")
  )
mod


val1<-round(as.numeric(as.character(mod$info$asDF$value[6])),digits = 3)
val2<-round(as.numeric(as.character(mod$info$asDF$value[9])),digits = 3)
val3<-as.numeric(as.character(mod$info$asDF$value[7]))
val4<-as.numeric(as.character(mod$info$asDF$value[14]))

testthat::test_that("info table is fine",{
  testthat::expect_equal(val1,-2785.625)
  testthat::expect_equal(val2, 0.039)
  testthat::expect_equal(val3, 5571.25,tolerance = .001)
  testthat::expect_equal(val4, 0.9564,tolerance = .001)
  
})

schoolexam$school<-factor(schoolexam$school)
mod0<-gamlj::gamljGlmMixed(
  formula = pass ~ 1 + math*activity+( 1 | school ),
  data = schoolexam,
  plotHAxis = math,
  correlatedEffects = "nocorr",
  cimethod = "wald",
  scaling=c(math="clusterbasedcentered")
)

testthat::test_that("info factoring clustes is equivalent",{
  testthat::expect_equal(mod$main$fixed$asDF[3,3],mod0$main$fixed$asDF[3,3])
  testthat::expect_equal(mod$main$fixed$asDF[4,2],mod0$main$fixed$asDF[4,2])
  testthat::expect_equal(mod$main$anova$asDF[2,2],mod0$main$anova$asDF[2,2])
})


testthat::expect_warning(
mod<-gamlj::gamljGlmMixed(
  formula = pass ~ 1 + math+I(math^2)+( 1 | school ),
  data = schoolexam,
  plotHAxis = math,
  correlatedEffects = "nocorr",
  cimethod = "wald")
  )

val1<-as.character(mod$info$asDF$value[15])

testthat::test_that("fail",{
  testthat::expect_equal(val1,"no")
  
})
val1<-round(as.numeric(as.character(mod$main$fixed$asDF[1,5])),digits = 3)
val2<-round(as.numeric(as.character(mod$main$fixed$asDF[2,7])),digits = 3)
val3<-as.character(mod$main$fixed$asDF[3,1])

testthat::test_that("params table is fine",{
  testthat::expect_equal(val1, 0.708)
  testthat::expect_equal(val2, 9.521)
  testthat::expect_equal(val3, "math²")
  
})

mplot<-mod$descPlot$plot$fun()
val1<-"ggplot" %in% class(mplot)

val2<-mplot$labels$y
val3<-mplot$theme$line$linetype
testthat::test_that("plot is there",{
  testthat::expect_equal(val1, TRUE)
  testthat::expect_equal(val2, "pass")
  testthat::expect_equal(val3, 1)
  
})


mod<-gamlj::gamljGlmMixed(
  formula = pass ~ 1 +( 1 | school ),
  data = schoolexam,
  correlatedEffects = "nocorr",
  cimethod = "wald")

val1<-round(mod$main$fixed$asDF[[4]],2)
testthat::test_that("intercept only",{
  testthat::expect_equal(val1, 0.98)

})


data("phdpubs")
phdpubs$q<-phdpubs$art
mod<-gamlj::gamljGlmMixed(
  formula = art ~ 1 + fem*mar +( 1 | program ),
  data = phdpubs,
  modelSelection = "nb",
  cimethod = "wald",
  postHoc = ~fem:mar)


testthat::test_that("negative binomial", {
    testthat::expect_equal(as.character(mod$info$asDF$value[4]),"Negative binomial")
    testthat::expect_equal(as.numeric(as.character(mod$info$asDF$value[10])),3201.73)
    testthat::expect_equal(as.character(mod$postHocs[[1]]$asDF[1,1]),"Men")
    testthat::expect_equal(mod$postHocs[[1]]$asDF[1,6],0.9350,tol=.0001)
    testthat::expect_equal(mod$postHocs[[1]]$asDF[2,9],0.16831,tol=.0001)
    }
)

