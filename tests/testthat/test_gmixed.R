context("gzlmixed")
data("schoolexam")

mod<-gamlj::gamljGlmMixed(
  formula = pass ~ 1 + math+( 1 | school ),
  data = schoolexam,
  plotHAxis = math,
  correlatedEffects = "nocorr",
  cimethod = "wald")


val1<-round(as.numeric(as.character(mod$info$asDF$value[6])),digits = 3)
val2<-round(as.numeric(as.character(mod$info$asDF$value[9])),digits = 3)
val3<-as.character(mod$info$asDF$value[13])

testthat::test_that("info table is fine",{
                    testthat::expect_equal(val1,-2826.509)
                    testthat::expect_equal(val2, 5659.02)
                    testthat::expect_equal(val3, "yes")
                    
                    })

val1<-round(as.numeric(as.character(mod$main$anova$asDF$test)),digits = 3)
val2<-round(as.numeric(as.character(mod$main$anova$asDF$p)),digits = 5)
val3<-as.character(mod$main$anova$asDF$name[1])

testthat::test_that("anova table is fine",{
  testthat::expect_equal(val1,90.855)
  testthat::expect_equal(val2, 0)
  testthat::expect_equal(val3, "math")
  
})

val1<-round(as.numeric(as.character(mod$main$fixed$asDF[1,5])),digits = 3)
val2<-round(as.numeric(as.character(mod$main$fixed$asDF[2,7])),digits = 3)
val3<-as.character(mod$main$fixed$asDF[2,1])

testthat::test_that("params table is fine",{
  testthat::expect_equal(val1,0.716)
  testthat::expect_equal(val2, 9.532)
  testthat::expect_equal(val3, "math")
  
})

val1<-round(as.numeric(as.character(mod$main$random$asDF[1,3])),digits = 3)

testthat::test_that("vars table is fine",{
  testthat::expect_equal(val1,1.312)

})

testthat::expect_warning(
mod<-gamlj::gamljGlmMixed(
  formula = pass ~ 1 + math+I(math^2)+( 1 | school ),
  data = schoolexam,
  plotHAxis = math,
  correlatedEffects = "nocorr",
  cimethod = "wald")
  )

val1<-as.character(mod$info$asDF$value[13])

testthat::test_that("fail",{
  testthat::expect_equal(val1,"no")
  
})
val1<-round(as.numeric(as.character(mod$main$fixed$asDF[1,5])),digits = 3)
val2<-round(as.numeric(as.character(mod$main$fixed$asDF[2,7])),digits = 3)
val3<-as.character(mod$main$fixed$asDF[3,1])

testthat::test_that("params table is fine",{
  testthat::expect_equal(val1, 0.708)
  testthat::expect_equal(val2, 9.516)
  testthat::expect_equal(val3, "math²")
  
})

val1<-"ggplot" %in% class(mod$descPlot$plot)
val2<-mod$descPlot$plot$labels$y
val3<-mod$descPlot$plot$theme$line$linetype
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
