testthat::context("glmixed")
tol <- .001
data("clustermanymodels")
data <- clustermanymodels

data$cluster <- factor(data$cluster)
data$cat3 <- factor(data$cat3)
data$yord <- factor(data$yord)
data$ybin <- factor(data$ybin)
data$ycat <- factor(data$ycat)

testthat::expect_warning(
    mod0 <- GAMLj3::gamlj_gmixed(
        formula = ybin ~ x * cat3 + (1 + x | cluster),
        data = data,
        model_type = "logistic"
    )
)

testthat::expect_warning(
    mod1 <- GAMLj3::gamlj_gmixed(
        data = data,
        model_type = "logistic",
        dep = "ybin",
        factors = "cat3",
        covs = "x",
        model_terms = ~ x * cat3,
        re = list(list(list("Intercept", "cluster"), list("x", "cluster")))
    )
)
mod1
testthat::expect_warning(
    mod2 <- GAMLj3::gamlj_gmixed(
        data = data,
        model_type = "logistic",
        dep = "ybin",
        factors = "cat3",
        covs = "x",
        model_terms = ~ x * cat3,
        re = ~ (1 + x | cluster)
    )
)
testthat::test_that("equivalent model input", {
    testthat::expect_equal(mod0$info$asDF$specs[2], mod1$info$asDF$specs[2])
    testthat::expect_equal(mod0$main$anova$asDF$f[3], mod1$main$anova$asDF$f[3])
    testthat::expect_equal(mod0$main$coefficients$asDF$label[2], mod0$main$coefficients$asDF$label[2])
    testthat::expect_equal(mod0$main$coefficients$asDF$expb.ci.lower[1], mod0$main$coefficients$asDF$expb.ci.lower[1])
    testthat::expect_equal(mod0$main$coefficients$asDF$expb.ci.lower[1], mod2$main$coefficients$asDF$expb.ci.lower[1])
    testthat::expect_equal(mod1$main$anova$asDF$f[3], mod2$main$anova$asDF$f[3])
})


testthat::expect_warning(
    model <- GAMLj3::gamlj_gmixed(
        formula = ybin ~ x * cat3 * z + (1 + x | cluster),
        data = data,
        model_type = "logistic"
    )
)
testthat::test_that("info is ok", {
    testthat::expect_equal(as.numeric(model$info$asDF$value[6]), 1800)
    testthat::expect_equal(model$info$asDF$value[7], "yes")
})

testthat::test_that("R2 is ok", {
    testthat::expect_equal(model$main$r2$asDF[1, 2], .059, tol)
    testthat::expect_equal(model$main$r2$asDF[2, 4], 42.11, tol)
})

testthat::test_that("fit is ok", {
    testthat::expect_equal(model$main$fit$asDF[1, 2], -1200.583, tol)
    testthat::expect_equal(model$main$fit$asDF[4, 2], 2401.166, tol)
})


mod <- GAMLj3::gamlj_gmixed(
    formula = ypoi ~ x * cat3 + (1 + x | cluster),
    data = data,
    model_type = "poisson",
    simple_x = x,
    simple_mods = cat3,
    posthoc = ~cat3,
    emmeans = ~x,
    es = c("expb", "marginals")
)
testthat::test_that("Poisson works", {
    testthat::expect_equal(mod$main$coefficients$asDF$expb[1], 1.5941, tol)
    testthat::expect_equal(mod$main$anova$asDF$test[1], 9.8702, tol)
    testthat::expect_equal(mod$main$coefficients$asDF$expb.ci.lower[2], 1.01, tol)
    testthat::expect_equal(mod$main$r2$asDF$r2[1], .266, tol)
    testthat::expect_equal(mod$main$fit$asDF$value[4], 8440, tol)
    testthat::expect_equal(mod$emmeans[[1]]$asDF$est.ci.upper[2], 1.79, tol)
    testthat::expect_equal(mod$simpleEffects$anova$asDF$test[2], 1.138, tol)
    testthat::expect_equal(mod$simpleEffects$coefficients$asDF$se[1], .0105, tol)
    testthat::expect_equal(mod$simpleEffects$coefficients$asDF$contrast[1], "x")
    testthat::expect_equal(mod$posthoc[[1]]$asDF$estimate[1], .883, tol)
    testthat::expect_equal(mod$main$marginals$asDF[1, 3], .162, tol)
    testthat::expect_equal(mod$main$marginals$asDF[1, 5], .00919, tol)
})


mod <- GAMLj3::gamlj_gmixed(
    formula = ycat ~ x * cat3 + (1 + x | cluster),
    data = data,
    model_type = "multinomial",
    simple_x = x,
    simple_mods = cat3,
    posthoc = ~cat3,
    emmeans = ~x,
    es = c("expb", "marginals")
)

testthat::test_that("multinomial works", {
    testthat::expect_equal(mod$main$coefficients$asDF$expb[1], 2.6, tol)
    #  testthat::expect_equal(mod$main$anova$asDF$test[1], 9.8702, tol)
    testthat::expect_equal(mod$main$coefficients$asDF$expb.ci.lower[2], 1.008, tol)
    testthat::expect_equal(mod$main$r2$asDF$r2[1], 0.098, tol)
    testthat::expect_equal(mod$main$fit$asDF$value[3], 3566.971, tol)
    testthat::expect_equal(mod$emmeans[[1]]$asDF$est.ci.upper[2], .251, tol)
    testthat::expect_equal(mod$simpleEffects$coefficients$asDF$se[1], .0225, tol)
    testthat::expect_equal(mod$simpleEffects$coefficients$asDF$contrast[1], "x")
    testthat::expect_equal(mod$posthoc[[1]]$asDF$estimate[1], .0161, tol)
    testthat::expect_equal(mod$main$marginals$asDF[1, 4], -0.0194, tol)
    testthat::expect_equal(mod$main$marginals$asDF[1, 5], .02383, tol)
})


# mod0<-mclogit::mblogit( ycat ~ x + cat3, random= ~1+x|cluster,data=data)
# mod1<-mclogit::mblogit( ycat ~ x * cat3, random= ~1+x|cluster,data=data)
# anova(mod0,mod1)
