context("contrasts")

data("fivegroups")
fivegroups$Group<-factor(fivegroups$Group)

mod<-gamlj::gamljGLM(formula = Score ~ Group,  data = fivegroups,
                      contrasts = list(list(var="Group",type="deviation")))

testthat::test_that("deviation works",{
  testthat::expect_equal(as.character(mod$main$fixed$asDF[3,2]),  "3 - ( 1, 2, 3, 4 )")        
  testthat::expect_equal(round(mod$main$fixed$asDF[3,3],digits=3),  0.097)        
}
)


mod<-gamlj::gamljGLM(formula = Score ~ Group,  data = fivegroups)

testthat::test_that("simple works",{
  testthat::expect_equal(as.character(mod$main$fixed$asDF[3,2]),  "3 - 1")        
  testthat::expect_equal(round(mod$main$fixed$asDF[3,3],digits=3),  0.244)        
}
)

mod<-gamlj::gamljGLM(formula = Score ~ Group,  data = fivegroups,
                     contrasts = list(list(var="Group",type="dummy")))
mod
test_that("dummy works",{
  expect_equal(as.character(mod$main$fixed$asDF[3,2]),  "3 - 1")        
  expect_equal(round(mod$main$fixed$asDF[3,3],digits=3),  0.244)        
  expect_equal(round(mod$main$fixed$asDF[1,3],digits=3),  0.01)        
}
)

mod<-gamlj::gamljGLM(formula = Score ~ Group,  data = fivegroups,
                     contrasts = list(list(var="Group",type="difference")))
test_that("difference works",{
  expect_equal(as.character(mod$main$fixed$asDF[3,2]),  "3 -  (1, 2)")        
  expect_equal(round(mod$main$fixed$asDF[3,3],digits=3),  0.294)        
  expect_equal(round(mod$main$fixed$asDF[1,3],digits=3),  0.156)        
}
)

mod<-gamlj::gamljGLM(formula = Score ~ Group,  data = fivegroups,
                     contrasts = list(list(var="Group",type="polynomial")))
test_that("polynomial works",{
  expect_equal(as.character(mod$main$fixed$asDF[4,2]),  "cubic")        
  expect_equal(round(mod$main$fixed$asDF[4,3],digits=3),  -0.131)        
  expect_equal(round(mod$main$fixed$asDF[1,3],digits=3),  0.156)        
}
)

mod<-gamlj::gamljGLM(formula = Score ~ Group,  data = fivegroups,
                     contrasts = list(list(var="Group",type="repeated")))
test_that("polynomial works",{
  expect_equal(as.character(mod$main$fixed$asDF[4,2]),  "3 - 4")        
  expect_equal(round(mod$main$fixed$asDF[3,3],digits=3),  -0.344)        
  expect_equal(round(mod$main$fixed$asDF[1,3],digits=3),  0.156)        
}
)

mod<-gamlj::gamljGLM(formula = Score ~ Group,  data = fivegroups,
                     contrasts = list(list(var="Group",type="helmert")))
test_that("helmert works",{
  expect_equal(as.character(mod$main$fixed$asDF[3,2]),  "2 -  (3, 4)")        
  expect_equal(round(mod$main$fixed$asDF[3,3],digits=3),  -0.443)        
  expect_equal(round(mod$main$fixed$asDF[1,3],digits=3),  0.156)        
}
)


cake<-lme4::cake
formula <- angle ~ recipe  + (1|replicate)

mod<-gamlj::gamljMixed(formula = formula, data=cake,
                     contrasts = list(list(var="recipe",type="helmert")))

test_that("helmert works",{
  expect_equal(as.character(mod$main$fixed$asDF[2,2]),  "A -  (B, C)")        
  expect_equal(round(mod$main$fixed$asDF[2,3],digits=3),  1.5)        
  expect_equal(round(mod$main$fixed$asDF[1,3],digits=3),  32.122)        
}
)

mod<-gamlj::gamljMixed(formula = formula, data=cake,
                       contrasts = list(list(var="recipe",type="deviation")))

test_that("deviation works",{
  expect_equal(as.character(mod$main$fixed$asDF[3,2]),  "C - ( A, B, C )")        
  expect_equal(round(mod$main$fixed$asDF[3,3],digits=3),  -0.522)        
  expect_equal(round(mod$main$fixed$asDF[1,3],digits=3),  32.122)        
}
)

means<-tapply(cake$angle,cake$recipe,mean)

mod<-gamlj::gamljMixed(formula = formula, data=cake)

test_that("simple works",{
  expect_equal(as.character(mod$main$fixed$asDF[3,2]),  "C - A")        
  expect_equal(mod$main$fixed$asDF[3,3], as.numeric(means[3]-means[1]), tolerance=.001)        
  expect_equal(mod$main$fixed$asDF[1,3], as.numeric(mean(means)), tolerance=.001)        
}
)


mod<-gamlj::gamljMixed(formula = formula, data=cake,
                       contrasts = list(list(var="recipe",type="dummy")))

test_that("dummy works",{
  expect_equal(as.character(mod$main$fixed$asDF[3,2]),  "C - A")        
  expect_equal(mod$main$fixed$asDF[3,3], as.numeric(means[3]-means[1]), tolerance=.001)        
  expect_equal(mod$main$fixed$asDF[1,3], as.numeric(means[1]), tolerance=.001)        
}
)

mod<-gamlj::gamljMixed(formula = formula, data=cake,
                       contrasts = list(list(var="recipe",type="dummy")))

test_that("dummy works",{
  expect_equal(as.character(mod$main$fixed$asDF[3,2]),  "C - A")        
  expect_equal(mod$main$fixed$asDF[3,3], as.numeric(means[3]-means[1]), tolerance=.001)        
  expect_equal(mod$main$fixed$asDF[1,3], as.numeric(means[1]), tolerance=.001)        
}
)


data("fivegroups")
fivegroups$Group<-factor(fivegroups$Group)

mod<-gamlj::gamljGzlm(formula = Score ~ Group,  data = fivegroups,
                     contrasts = list(list(var="Group",type="deviation")))

test_that("deviation works",{
  expect_equal(as.character(mod$main$fixed$asDF[3,2]),  "3 - ( 1, 2, 3, 4 )")        
  expect_equal(round(mod$main$fixed$asDF[3,3],digits=3),  0.097)        
}
)


mod<-gamlj::gamljGzlm(formula = Score ~ Group,  data = fivegroups)

test_that("simple works",{
  expect_equal(as.character(mod$main$fixed$asDF[3,2]),  "3 - 1")        
  expect_equal(round(mod$main$fixed$asDF[3,3],digits=3),  0.244)        
}
)

mod<-gamlj::gamljGzlm(formula = Score ~ Group,  data = fivegroups,
                     contrasts = list(list(var="Group",type="dummy")))

test_that("dummy works",{
  expect_equal(as.character(mod$main$fixed$asDF[3,2]),  "3 - 1")        
  expect_equal(round(mod$main$fixed$asDF[3,3],digits=3),  0.244)        
  expect_equal(round(mod$main$fixed$asDF[1,3],digits=3),  0.01)        
}
)

mod<-gamlj::gamljGzlm(formula = Score ~ Group,  data = fivegroups,
                     contrasts = list(list(var="Group",type="difference")))
test_that("difference works",{
  expect_equal(as.character(mod$main$fixed$asDF[3,2]),  "3 -  (1, 2)")        
  expect_equal(round(mod$main$fixed$asDF[3,3],digits=3),  0.294)        
  expect_equal(round(mod$main$fixed$asDF[1,3],digits=3),  0.156)        
}
)

mod<-gamlj::gamljGzlm(formula = Score ~ Group,  data = fivegroups,
                     contrasts = list(list(var="Group",type="polynomial")))
test_that("polynomial works",{
  expect_equal(as.character(mod$main$fixed$asDF[4,2]),  "cubic")        
  expect_equal(round(mod$main$fixed$asDF[4,3],digits=3),  -0.131)        
  expect_equal(round(mod$main$fixed$asDF[1,3],digits=3),  0.156)        
}
)

mod<-gamlj::gamljGzlm(formula = Score ~ Group,  data = fivegroups,
                     contrasts = list(list(var="Group",type="repeated")))
test_that("polynomial works",{
  expect_equal(as.character(mod$main$fixed$asDF[4,2]),  "3 - 4")        
  expect_equal(round(mod$main$fixed$asDF[3,3],digits=3),  -0.344)        
  expect_equal(round(mod$main$fixed$asDF[1,3],digits=3),  0.156)        
}
)


mod<-gamlj::gamljGzlm(formula = Score ~ Group,  data = fivegroups,
                     contrasts = list(list(var="Group",type="helmert")))
test_that("helmert works",{
  expect_equal(as.character(mod$main$fixed$asDF[3,2]),  "2 -  (3, 4)")        
  expect_equal(round(mod$main$fixed$asDF[3,3],digits=3),  -0.443)        
  expect_equal(round(mod$main$fixed$asDF[1,3],digits=3),  0.156)        
}
)


data("fivegroups")
fivegroups$Group<-factor(fivegroups$Group)
mod<-gamlj::gamljGzlm(formula = Score ~ Group,  data = fivegroups)
fivegroups$Group<-factor(fivegroups$Group,levels = c(4,3,2,1))
modr<-gamlj::gamljGzlm(formula = Score ~ Group,  data = fivegroups)


test_that("reversing works",{
  expect_equal(as.character(modr$main$fixed$asDF[2,2]),  "3 - 4")        
  expect_equal(mod$main$fixed$asDF[4,3],(-modr$main$fixed$asDF[4,3]))        
  expect_equal(mod$main$fixed$asDF[1,3],modr$main$fixed$asDF[1,3])        
}
)
