testthat::context("glm")
data("hsbdemo")
tol <- 0.001
mod0 <- GAMLj3::gamlj_glm(
  formula = schtyp ~ write + honors + honors:write,
  data = hsbdemo,
  model_type = "logistic"
)


mod1 <- GAMLj3::gamlj_glm(
  data = hsbdemo,
  model_type = "logistic",
  dep = "schtyp",
  factors = "honors",
  covs = "write",
  model_terms = ~ write + honors + honors:write
  
)


mod2 <- GAMLj3::gamlj_glm(
  formula = schtyp ~ write * honors,
  data = hsbdemo,
  model_type = "logistic"
)

testthat::test_that("equivalent model input (1)", {
  testthat::expect_equal(mod0$info$asDF$specs[2], mod1$info$asDF$specs[2])
  testthat::expect_equal(mod0$main$anova$asDF$f[3], mod1$main$anova$asDF$f[3])
  testthat::expect_equal(mod0$main$coefficients$asDF$label[2], mod0$main$coefficients$asDF$label[2])
  testthat::expect_equal(mod0$main$coefficients$asDF$est.ci.lower[1], mod0$main$coefficients$asDF$est.ci.lower[1])
  testthat::expect_equal(mod0$main$coefficients$asDF$est.ci.lower[1], mod2$main$coefficients$asDF$est.ci.lower[1])
  testthat::expect_equal(mod1$main$anova$asDF$f[3], mod2$main$anova$asDF$f[3])
})




testthat::test_that("gzlm logistic coherence", {
  testthat::expect_error(GAMLj3::gamlj_glm(
    formula = ses ~ 1,
    data = hsbdemo,
    model_type = "logistic"
  ))
})

data("hsbdemo")
mod <- GAMLj3::gamlj_glm(
  formula = schtyp ~ write + honors + honors:write,
  data = hsbdemo,
  estimates_ci = TRUE,
  model_type = "logistic"
)

res <- mod$main$coefficients$asDF
testthat::test_that("glm estimates are correct", {
  testthat::expect_equal(as.character(res[3, 1]), "honors1")
  testthat::expect_equal(res$est.ci.lower[2], -0.20408, tolerance = tol)
  testthat::expect_equal(res$est.ci.upper[2], .1703, tolerance = tol)
  testthat::expect_equal(res$p[2], 0.8079, tolerance = tol)
  testthat::expect_equal(mod$main$fit$asDF$value[2], 175.787, tolerance = tol)
})

data <- hsbdemo

mod <- GAMLj3::gamlj_glm(
  formula = schtyp ~ write + honors + honors:write,
  data = hsbdemo,
  model_type = "logistic",
  ci_width = 90,
  simple_x = "write",
  simple_mods = "honors",
  plot_x = "write",
  show_contrastcodes = T
)
# 
r.anova <- mod$main$anova$asDF
r.show <- as.character(mod$main$contrastCodeTables[[1]]$asDF[1, 1])
testthat::test_that("gzlm anova is correct", {
  testthat::expect_equal(as.character(r.anova[3, 1]), "write:honors")
  testthat::expect_equal(round(r.anova[1, 4], 3), 0.809)
})
testthat::test_that("contrasts are correct", {
  testthat::expect_equal(r.show, "-0.5")
})


mod <- GAMLj3::gamlj_glm(
  formula = schtyp ~ write + honors + honors:write,
  data = hsbdemo,
  model_type = "logistic",
  posthoc = "honors",
  plot_x = "write"
)


testthat::test_that("gzlm posthoc", {
  testthat::expect_equal(round(mod$posthoc[[1]]$asDF[[5]], 3), 1.635)
  testthat::expect_equal(as.character(mod$posthoc[[1]]$asDF[[3]]), "not enrolled")
})

res <- mod$main$coefficients$asDF

testthat::test_that("glm contrasts", {
  testthat::expect_equal(round(res[3, 3], 2), 0.19)
  testthat::expect_equal(round(res[1, 3], 2), 1.36)
})

testthat::test_that("gzlm plot", {
  testthat::expect_is(mod$mainPlots[[1]],"Image")
 })

testthat::test_that("gzlm CI width", {
  testthat::expect_equal(mod$main$coefficients$asDF$expb.ci.lower[4], .6072, tol)
})


mod <- GAMLj3::gamlj_glm(
  formula = schtyp ~ 1,
  data = hsbdemo,
  model_type = "logistic"
)

testthat::test_that(
  "intercept only works",
  testthat::expect_equal(round(mod$main$coefficients$asDF[1, 3], digits = 3), 1.658)
)



data <- hsbdemo
names(data)[3] <- c("Gender (test ?)")
mod <- GAMLj3::gamlj_glm(
  formula = schtyp ~ math + `Gender (test ?)` + math:`Gender (test ?)`,
  data = data,
  model_type = "logistic",
  emmeans = ~`Gender (test ?)`
)

testthat::test_that("glm weird names", {
  testthat::expect_equal(as.character(mod$main$coefficients$asDF[3, 1]), "Gender (test ?)1")
})


res <- mod$emmeans[[1]]$asDF
testthat::test_that("glm EMM", {
  testthat::expect_equal(round(res[1, 2], 2), 0.84)
})

mod <- GAMLj3::gamlj_glm(
  formula = prog ~ math + ses * female,
  data = hsbdemo,
  model_type = "multinomial",
  posthoc = ~ ses:female
)

mod$posthoc[[1]]$asDF
testthat::test_that("Multinomial posthoc works", {
  testthat::expect_equal(mod$posthoc[[1]]$asDF$response[[1]], "academic")
  testthat::expect_equal(mod$posthoc[[1]]$asDF$estimate[8], .2101, tol)
  testthat::expect_equal(mod$posthoc[[1]]$asDF$response[[42]], "vocation")
  testthat::expect_equal(mod$posthoc[[1]]$asDF[42, 1], "vocation")
  testthat::expect_equal(mod$posthoc[[1]]$asDF[42, 2], "low")
  testthat::expect_equal(mod$posthoc[[1]]$asDF[42, 3], "female")
})


mod2 <- GAMLj3::gamlj_glm(
  formula = prog ~ ses * female + math,
  data = hsbdemo,
  model_type = "multinomial"
)



res <- mod$main$anova$asDF
res2 <- mod2$main$anova$asDF


testthat::test_that("glm order does not count", {
  testthat::expect_equal(res[1, 2], res2[3, 2])
  testthat::expect_equal(as.character(res[1, 1]), as.character(res2[3, 1]))
})

## testing options for all models

data("poissonacts")
data <- poissonacts
data$age <- factor(data$age)


mod <- GAMLj3::gamlj_glm(
  formula = acts ~ agg_test * age,
  data = data,
  model_type = "poisson",
  simple_x = agg_test,
  simple_mods = age,
  posthoc = ~age,
  emmeans = ~agg_test,
  es=c("expb","marginals")
)

testthat::test_that("Poisson works", {
  testthat::expect_equal(mod$main$coefficients$asDF$expb[1], .1064, tol)
  testthat::expect_equal(mod$main$anova$asDF$test[1], 68.38, tol)
  testthat::expect_equal(mod$main$coefficients$asDF$expb.ci.lower[6], .8577, tol)
  testthat::expect_equal(mod$main$r2$asDF$r2, .898, tol)
  testthat::expect_equal(mod$main$fit$asDF$value[4], 9.82, tol)
  testthat::expect_equal(mod$emmeans[[1]]$asDF$est.ci.upper[2], .338, tol)
  testthat::expect_equal(mod$simpleEffects$anova$asDF$test[2], 14.048, tol)
  testthat::expect_equal(mod$simpleEffects$coefficients$asDF$se[1], .0428, tol)
  testthat::expect_equal(mod$simpleEffects$coefficients$asDF$contrast[1], "agg_test")
  testthat::expect_equal(mod$posthoc[[1]]$asDF$estimate[2], .630, tol)
  testthat::expect_equal(mod$main$marginals$asDF[2,3],.2769,tol)
  testthat::expect_equal(mod$main$marginals$asDF[3,5],.0666,tol)
})

data$q <- data$acts + 1

mod <- GAMLj3::gamlj_glm(
  formula = q ~ agg_test * age,
  data = data,
  model_type = "custom",
  custom_family = "Gamma",
  custom_link = "inverse",
  simple_x = agg_test,
  simple_mods = age,
  posthoc = ~age,
  emmeans = ~agg_test,
  es=c("expb","marginals")
  
)
testthat::test_that("Custom model works", {
  testthat::expect_equal(mod$main$coefficients$asDF$expb[1], 2.151, tol)
  testthat::expect_equal(mod$main$anova$asDF$test[1], 168.42, tol)
  testthat::expect_equal(mod$main$coefficients$asDF$expb.ci.lower[[1]], 1.994, tol)
  testthat::expect_equal(mod$main$r2$asDF$r2, .870, tol)
  testthat::expect_equal(mod$main$fit$asDF$value[4], 2.538, tol)
  testthat::expect_equal(mod$emmeans[[1]]$asDF$est.ci.upper[2], 1.448, tol)
  testthat::expect_equal(mod$simpleEffects$anova$asDF$test[2], 75.937, tol)
  testthat::expect_equal(mod$simpleEffects$coefficients$asDF$se[1], .001, tol)
  testthat::expect_equal(mod$simpleEffects$coefficients$asDF$contrast[1], "agg_test")
  testthat::expect_equal(mod$posthoc[[1]]$asDF$estimate[2], .010, tol)
  testthat::expect_equal(mod$posthoc[[1]]$asDF$age_lev1[2], "1")
  testthat::expect_equal(mod$main$marginals$asDF[2,3],.255,tol)
  testthat::expect_equal(mod$main$marginals$asDF[3,5],.073,tol)
})


### negative binomial
data$q <- as.integer(data$q)
testthat::expect_warning({
  mod <- GAMLj3::gamlj_glm(
    formula = q ~ agg_test * age,
    data = data,
    model_type = "nb",
    simple_x = agg_test,
    simple_mods = age,
    posthoc = ~age,
    emmeans = ~agg_test,
    estimates_ci = TRUE,
    es=c("expb","marginals")
  )
})

testthat::test_that("negative binomial model works", {
  testthat::expect_equal(mod$main$coefficients$asDF$expb[1], 1.425, tol)
  testthat::expect_equal(mod$main$anova$asDF$test[1], 22.6, tol)
  testthat::expect_equal(mod$main$coefficients$asDF$est.ci.lower[1], -.0845, tol)
  testthat::expect_equal(mod$main$r2$asDF$r2, .777, tol)
  testthat::expect_equal(mod$main$fit$asDF$value[4], 8.89, tol)
  testthat::expect_equal(mod$emmeans[[1]]$asDF$est.ci.upper[2], 2.08, tol)
  testthat::expect_equal(mod$simpleEffects$anova$asDF$test[2], 5.91, tol)
  testthat::expect_equal(mod$simpleEffects$coefficients$asDF$se[1], .010, tol)
  testthat::expect_equal(mod$simpleEffects$coefficients$asDF$contrast[1], "agg_test")
  testthat::expect_equal(mod$posthoc[[1]]$asDF$estimate[2], 1.056, tol)
  testthat::expect_equal(mod$posthoc[[1]]$asDF$age_lev1[2], "1")
  testthat::expect_equal(mod$main$marginals$asDF[2,3],.254,tol)
  testthat::expect_equal(mod$main$marginals$asDF[3,5],.0409,tol)
  
})


### quasi poisson
data$q <- as.integer(data$q)
mod <- GAMLj3::gamlj_glm(
  formula = q ~ agg_test * age,
  data = data,
  model_type = "poiover",
  simple_x = agg_test,
  simple_mods = age,
  posthoc = ~age,
  emmeans = ~agg_test,
  estimates_ci = TRUE,
  es=c("expb","marginals")
)

testthat::test_that("quasi poisson binomial works", {
  testthat::expect_equal(mod$main$coefficients$asDF$expb[1], 1.425, tol)
  testthat::expect_equal(mod$main$anova$asDF$test[1], 105.641, tol)
  testthat::expect_equal(mod$main$coefficients$asDF$est.ci.lower[1], .167, tol)
  testthat::expect_equal(mod$main$r2$asDF$r2, .777, tol)
  testthat::expect_equal(mod$main$fit$asDF$value[4], 8.893, tol)
  testthat::expect_equal(mod$emmeans[[1]]$asDF$est.ci.upper[2], 1.698, tol)
  testthat::expect_equal(mod$simpleEffects$anova$asDF$test[2], 27.64, tol)
  testthat::expect_equal(mod$simpleEffects$coefficients$asDF$se[1], .00475, tol)
  testthat::expect_equal(mod$simpleEffects$coefficients$asDF$contrast[1], "agg_test")
  testthat::expect_equal(mod$posthoc[[1]]$asDF$estimate[2], 1.056, tol)
  testthat::expect_equal(mod$main$marginals$asDF[2,3],.254,tol)
  testthat::expect_equal(mod$main$marginals$asDF[3,5],.0591,tol)
  
})


## ordinal

## testing options for all models

data("manymodels")
data <- manymodels
data$cat3 <- factor(data$cat3)
data$yord <- factor(data$yord)

mod <- GAMLj3::gamlj_glm(
  formula = yord ~ x * cat3,
  data = data,
  model_type = "ordinal",
  emmeans = ~cat3,
  simple_x= x,
  simple_mods = cat3,
  posthoc=~cat3
)

testthat::test_that("Ordinal works", {
  testthat::expect_equal(mod$main$coefficients$asDF$expb[1], .0118, tol)
  testthat::expect_equal(mod$main$anova$asDF$test[1], 51.056, tol)  
  testthat::expect_equal(mod$main$coefficients$asDF$expb.ci.lower[6], .851, tol)
  testthat::expect_equal(mod$main$r2$asDF$r2, .199, tol)
  testthat::expect_equal(mod$main$fit$asDF$value[4], 899.646, tol)
  testthat::expect_equal(mod$emmeans[[1]]$asDF$est.ci.upper[2], 3.310, tol)
  testthat::expect_equal(mod$simpleEffects$anova$asDF$test[2], 15.407, tol)
  testthat::expect_equal(mod$simpleEffects$coefficients$asDF$se[1], .363, tol)
  testthat::expect_equal(mod$simpleEffects$coefficients$asDF$contrast[1], "x")
  testthat::expect_equal(mod$posthoc[[1]]$asDF$estimate[2], 3.318, tol)
})




### model comparison 

mod0 <- GAMLj3::gamlj_glm(
  formula = schtyp ~ write + honors + honors:write,
  data = hsbdemo,
  model_type = "logistic"
)
testthat::test_that("no comparison", {
  testthat::expect_equal(nrow(mod0$main$r2$asDF),1)
})

mod <- GAMLj3::gamlj_glm(
  formula = schtyp ~ write + honors + honors:write,
  data = hsbdemo,
  model_type = "logistic",
  nested_terms = ~write
)

testthat::test_that("logistic comparison", {
  testthat::expect_equal(mod$main$r2$asDF[3,2],.0265,tol)
  testthat::expect_equal(mod$main$r2$asDF[1,6],.0444,tol)
  
})


mod <- GAMLj3::gamlj_glm(
  formula = prog ~ write + honors + honors:write,
  data = hsbdemo,
  model_type = "multinomial",
  nested_terms = ~write
)

testthat::test_that("multinomila comparison", {
  testthat::expect_equal(mod$main$r2$asDF[3,2],.0061,tol)
})

