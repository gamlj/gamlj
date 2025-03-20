library(GAMLj3)
testthat::context("model comparisons")
tol <- .001
data("clustermanymodels")
data <- clustermanymodels
data$cluster <- factor(data$cluster)
data$cat2 <- factor(data$cat2)
data$cat3 <- factor(data$cat3)
data$yord <- factor(data$yord)
data$ybin <- factor(data$ybin)


mod <- GAMLj3::gamlj_lm(
    formula = ycont ~ x * cat2,
    nested_terms = ~x,
    omnibus = "LRT",
    data = data
)
testthat::test_that("test glm anova comparison option", {
    testthat::expect_equal(mod$main$r2$asDF[3, 5], 264.805, tolerance = tol)
    testthat::expect_equal(mod$main$r2$asDF[3, 4], 2, tolerance = tol)
})

mod <- GAMLj3::gamlj_lm(
    formula = ycont ~ x * cat2,
    nested_terms = ~ x * cat2,
    omnibus = "LRT",
    data = data
)

testthat::test_that("test glm anova comparison option", {
    testthat::expect_equal(mod$main$r2$asDF[3, 5], 0, tolerance = tol)
    testthat::expect_equal(mod$main$r2$asDF[3, 4], 0, tolerance = tol)
})

testthat::expect_warning(
    mod <- GAMLj3::gamlj_mixed(
        formula = ycont ~ x * cat2 + (1 + x | cluster),
        nested_terms = ~x,
        omnibus = "LRT",
        data = data
    )
)

testthat::test_that("test mixed comparison: fixed", {
    testthat::expect_equal(mod$main$r2$asDF[5, 5], 279.86, tolerance = tol)
    testthat::expect_equal(mod$main$r2$asDF[5, 4], 5, tolerance = tol)
})

testthat::expect_warning(
    mod <- GAMLj3::gamlj_mixed(
        formula = ycont ~ x * cat2 + (1 + x | cluster),
        nested_terms = ~ x * cat2,
        nested_re = ~ 1 + (1 | cluster),
        omnibus = "LRT",
        data = data
    )
)
testthat::test_that("test mixed comparison: random", {
    testthat::expect_equal(mod$main$r2$asDF[5, 5], .458, tolerance = tol)
    testthat::expect_equal(mod$main$r2$asDF[5, 4], 2, tolerance = tol)
})

testthat::expect_warning(
    mod <- GAMLj3::gamlj_mixed(
        formula = ycont ~ x * cat2 + (1 + x | cluster),
        nested_terms = ~ x * cat2,
        nested_re = ~ 1 + (1 + x | cluster),
        omnibus = "LRT",
        data = data
    )
)
testthat::test_that("test mixed comparison: 0 df", {
    testthat::expect_equal(mod$main$r2$asDF[5, 5], 0, tolerance = tol)
    testthat::expect_equal(mod$main$r2$asDF[5, 4], 0, tolerance = tol)
})

testthat::expect_warning(
    mod <- GAMLj3::gamlj_gmixed(
        formula = ybin ~ x * cat2 + (1 + x | cluster),
        nested_terms = ~ x * cat2,
        nested_re = ~ 1 + (1 | cluster),
        data = data
    )
)

testthat::test_that("test mixed comparison: random", {
    testthat::expect_equal(mod$main$r2$asDF[5, 5], .142, tolerance = tol)
    testthat::expect_equal(mod$main$r2$asDF[5, 4], 2, tolerance = tol)
})


formula <- ybin~x * cat2 + (1 + x | cluster)
suppressWarnings(rmod <- lme4::glmer(formula, data = data, family = binomial()))


testthat::expect_warning(
    mod <- GAMLj3::gamlj_gmixed(
        formula = ybin ~ x * cat2 + (1 + x | cluster),
        nested_terms = ~ x * cat2,
        nested_re = ~ 1 + (1 + x | cluster),
        data = data
    )
)

testthat::test_that("test mixed comparison: 0 df", {
    testthat::expect_equal(mod$main$r2$asDF[5, 5], 0, tolerance = tol)
    testthat::expect_equal(mod$main$r2$asDF[5, 4], 0, tolerance = tol)
})


mod <- GAMLj3::gamlj_gmixed(
    formula = ybin ~ x * cat2 + (1 | cluster),
    model_type = "probit",
    nested_terms = ~x,
    nested_re = ~ 1 + (1 | cluster),
    data = data
)

testthat::test_that("test mixed comparison: probit", {
    testthat::expect_equal(mod$main$r2$asDF[5, 5], 154.56, tolerance = tol)
    testthat::expect_equal(mod$main$r2$asDF[5, 4], 2, tolerance = tol)
})

mod <- GAMLj3::gamlj_gmixed(
    formula = yord ~ x * cat2 + (1 | cluster),
    model_type = "ordinal",
    nested_terms = ~x,
    nested_re = ~ 1 + (1 | cluster),
    data = data
)

testthat::test_that("test mixed comparison: ordinal", {
    testthat::expect_equal(mod$main$r2$asDF[5, 5], 220.117, tolerance = tol)
    testthat::expect_equal(mod$main$r2$asDF[5, 4], 2, tolerance = tol)
})


data$ycat <- factor(data$ycat)

suppressWarnings({
    mod <- GAMLj3::gamlj_gmixed(
        formula = ycat ~ x * cat2 + (1 | cluster),
        model_type = "multinomial",
        nested_terms = ~x,
        nested_re = ~ 1 + (1 | cluster),
        data = data
    )
})

testthat::test_that("test mixed comparison: ordinal", {
    testthat::expect_equal(mod$main$r2$asDF[2, 5], 393.26, tolerance = tol)
    testthat::expect_true(is.na(mod$main$r2$asDF[3, 5]))
})

mod <- GAMLj3::gamlj_gmixed(
    formula = ypoi ~ x * cat2 + (1 | cluster),
    model_type = "poisson",
    nested_terms = ~x,
    nested_re = ~ 1 + (1 | cluster),
    data = data
)

testthat::test_that("test mixed comparison: poisson", {
    testthat::expect_equal(mod$main$r2$asDF[5, 5], 630.53, tolerance = tol)
    testthat::expect_equal(mod$main$r2$asDF[5, 4], 2)
})


mod <- GAMLj3::gamlj_gmixed(
    formula = ypoi ~ x * cat2 + (1 | cluster),
    model_type = "nb",
    nested_terms = ~x,
    nested_re = ~ 1 + (1 | cluster),
    data = data
)

testthat::test_that("test mixed comparison: negative binomila", {
    testthat::expect_equal(mod$main$r2$asDF[5, 5], 183.766, tolerance = tol)
    testthat::expect_equal(mod$main$r2$asDF[5, 4], 2)
})


##### generalized

mod <- GAMLj3::gamlj_glm(
    formula = ybin ~ x * cat2,
    model_type = "logistic",
    nested_terms = ~x,
    data = data
)


testthat::test_that("test gzlm comparison: logistic", {
    testthat::expect_equal(mod$main$r2$asDF[3, 5], 155.044, tolerance = tol)
    testthat::expect_equal(mod$main$r2$asDF[3, 4], 2)
})
mod <- GAMLj3::gamlj_glm(
    formula = ybin ~ x * cat2,
    model_type = "probit",
    nested_terms = ~x,
    data = data
)

testthat::test_that("test gzlm comparison: probit", {
    testthat::expect_equal(mod$main$r2$asDF[3, 5], 155.493, tolerance = tol)
    testthat::expect_equal(mod$main$r2$asDF[3, 4], 2)
})

mod <- GAMLj3::gamlj_glm(
    formula = yord ~ x * cat2,
    model_type = "ordinal",
    nested_terms = ~x,
    data = data
)

testthat::test_that("test gzlm comparison: ordinal", {
    testthat::expect_equal(mod$main$r2$asDF[3, 4], 207.433, tolerance = tol)
    testthat::expect_equal(mod$main$r2$asDF[3, 3], 2)
})

mod <- GAMLj3::gamlj_glm(
    formula = yord ~ x * cat2,
    model_type = "multinomial",
    nested_terms = ~x,
    data = data
)
testthat::test_that("test gzlm comparison: multinomial", {
    testthat::expect_equal(mod$main$r2$asDF[3, 4], 217.523, tolerance = tol)
    testthat::expect_equal(mod$main$r2$asDF[3, 3], 8)
})

data$yperc <- data$ycont / (max(data$ycont) + .1)

mod <- GAMLj3::gamlj_glm(
    formula = yperc ~ x * cat2,
    model_type = "beta",
    nested_terms = ~x,
    data = data
)

testthat::test_that("test gzlm comparison: multinomial", {
    testthat::expect_equal(mod$main$r2$asDF[3, 4], 259.670, tolerance = tol)
    testthat::expect_equal(mod$main$r2$asDF[3, 3], 2)
})


mod <- GAMLj3::gamlj_glm(
    formula = ypoi ~ x * cat2,
    model_type = "nb",
    nested_terms = ~x,
    data = data
)
testthat::test_that("test gzlm comparison: negative binomial", {
    testthat::expect_equal(mod$main$r2$asDF[3, 5], 168.62, tolerance = tol)
    testthat::expect_equal(mod$main$r2$asDF[3, 4], 2)
})
