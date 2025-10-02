testthat::context("R interface")
tol <- 0.001
data("qsport")

obj <- GAMLj3::gamlj_lm(
    formula = performance ~ hours + type,
    posthoc = ~type,
    data = qsport
)

preds <- predict(obj)
reds <- residuals(obj)

testthat::test_that("test glm", {
    testthat::expect_equal(round(mean(preds), 2), 37.88)
    testthat::expect_equal(length(preds), 100)
    testthat::expect_equal(mean(reds), 0, tol)
})

sums <- summary(obj)


testthat::test_that("summary glm", {
    testthat::expect_equal(sums[[1]]$info[2], "Model")
    testthat::expect_equal(sums[[2]]$ar2, .408, tol)
    testthat::expect_equal(sums[[3]]$df[4], 97)
})



newopt <- list(var = "hours", type = "standardized")


qsport$z <- as.numeric(scale(qsport$performance))
zobj <- GAMLj3::gamlj_lm(
    formula = z ~ hours,
    data = qsport,
    covs_scale = newopt
)

cc <- zobj$main$coefficients$asDF
testthat::test_that("standardizing", {
    testthat::expect_equal(cc$estimate[2], .1942, tol)
})

upd <- update(obj, covs_scale = newopt, es = c("beta", "etap", "omega"))
res1 <- upd$main$coefficients$asDF
res2 <- upd$main$anova$asDF

testthat::test_that("updating", {
    testthat::expect_equal(res1$estimate[2], 1.4073, tol)
    testthat::expect_equal(res2$etaSqP[2], 0.4147, tol)
})


obj <- GAMLj3::gamlj_lm(
    formula = performance ~ hours + type,
    posthoc = ~type,
    data = qsport
)


data("hsbdemo")

mod <- GAMLj3::gamlj_glm(
    formula = prog ~ write + ses * female,
    data = hsbdemo,
    estimates_ci = TRUE,
    plot_x = write,
    plot_z = ses,
    plot_by = female,
    model_type = "multinomial"
)

mod2 <- update(mod, vcov = T)

mplots <- plot(mod)

testthat::test_that("plot ok", {
    testthat::expect_true(is.list(mplots))
    testthat::expect_true(ggplot2::is_ggplot(mplots[[1]]))
})

data("subjects_by_stimuli")

subjects_by_stimuli$subj <- factor(subjects_by_stimuli$subj)
mod <- GAMLj3::gamlj_mixed(
    formula = y ~ cond + (1 | subj),
    data = subjects_by_stimuli
)

p0 <- predict(mod)
p1 <- predict(mod, random.only = T)


testthat::test_that("Mixed dots work", {
    testthat::expect_equal(mean(p0), 19.6043, tol = tol)
    testthat::expect_equal(mean(p1), 0, tol = tol)
})


data("clustermanymodels")

clustermanymodels$ybin <- factor(clustermanymodels$ybin)
clustermanymodels$cluster <- factor(clustermanymodels$cluster)

mod1 <- GAMLj3::gamlj_mixed(
    formula = ycont ~ 1 + x + (1 | cluster),
    data = clustermanymodels,
    plot_x = x,
    ci_method = "wald"
)


mplot <- plot(mod1)

testthat::test_that(
    "plot ok",
    testthat::expect_true(ggplot2::is_ggplot(mplot))
)
data("wicksell")
wicksell$time <- factor(wicksell$time)
wicksell$group <- factor(wicksell$group)
wicksell$subj <- factor(wicksell$subj)

gobj <- GAMLj3::gamlj_mixed(
    formula = dv ~ 1 + group + time:group + time + (1 | subj),
    data = wicksell
)

r1 <- GAMLj3::posthoc(gobj)
r2 <- GAMLj3::posthoc(gobj, formula = ~ group + group:time, adjust = c("bonf", "holm"))
tab <- r2[[2]]$asDF
testthat::test_that("posthoc function", {
    testthat::expect_false(r1)
    testthat::expect_equal(tab[6, 8], 6.8638, tolerance = tol)
    testthat::expect_true(tab[3, 4] == 1)
})

gobj <- GAMLj3::gamlj_mixed(
    formula = dv ~ 1 + group + time:group + time + (1 | subj),
    data = wicksell
)


r2 <- simple_effects(gobj, formula = ~ group:time)
tab <- r2[[2]]$asDF

testthat::test_that("simple effect function", {
    testthat::expect_equal(tab[4, 8], -1.4187, tolerance = tol)
})




data("subjects_by_stimuli")
subjects_by_stimuli$subj <- factor(subjects_by_stimuli$subj)
subjects_by_stimuli$cond <- factor(subjects_by_stimuli$cond)
contrasts(subjects_by_stimuli$cond) <- contr.sum(2) / 2

mod1 <- GAMLj3::gamlj_mixed(
    formula = y ~ 1 + cond + (1 + cond | subj),
    data = subjects_by_stimuli,
    rand_hist = T
)

res <- GAMLj3::assumptions(mod1)

testthat::test_that("assumptions plots are there", {
    testthat::expect_equal(length(res), 2)
    testthat::expect_equal(res[[1]]$name, "randHist1")
    testthat::expect_true(ggplot2::is_ggplot(res[[1]]$plot))
})



mod <- GAMLj3::gamlj_mixed(
    formula = y ~ 1 + cond + (1 | subj),
    data = subjects_by_stimuli
)

preds <- predict(mod)
n1 <- dim(GAMLj3::get_data(mod))[1]
n2 <- length(preds)
testthat::test_that("mixed predict", {
    testthat::expect_equal(mean(preds), 19.6, tol)
    testthat::expect_equal(n1, n2)
})


rmod0 <- mod$model

rmod1 <- lme4::lmer(
    formula = y ~ 1 + cond + (1 | subj),
    data = GAMLj3::get_data(mod),
    REML = TRUE
)

testthat::test_that("mixed get model", {
    testthat::expect_equal(rmod0@theta, rmod1@theta, tolerance = 0.001)
    testthat::expect_equal(rmod1@theta, mod$model@theta, tolerance = 0.001)
})


mod <- GAMLj3::gamlj_mixed(
    formula = y ~ 1 + cond + (1 | subj),
    data = subjects_by_stimuli,
    contrasts = c(cond = "deviation")
)

res <- mod$main$coefficients$asDF[2, 3]

testthat::test_that("contrast option works", {
    testthat::expect_equal(res, .484954, tolerance = 0.001)
})




data("hsbdemo")
mod0 <- stats::glm(schtyp ~ write + honors + honors:write, data = hsbdemo, family = binomial())
preds0 <- predict(mod0, type = "response")

mod1 <- GAMLj3::gamlj_glm(
    formula = schtyp ~ write + honors + honors:write,
    data = hsbdemo,
    expb_ci = T,
    model_type = "logistic"
)

preds <- predict(mod1)

testthat::test_that("gzlm predict ", {
    testthat::expect_equal(mean(preds), mean(preds0), tol)
})

rmod <- mod1$model


testthat::test_that("glm get model ", {
    testthat::expect_equal(sigma(rmod), sigma(mod0), tolerance = 0.01)
    testthat::expect_equal(sigma(mod0), sigma(mod1$model), tolerance = 0.01)
})



se <- GAMLj3::simple_effects(mod1, simple_x = "write", simple_mods = "honors")

res <- se$anova$asDF
testthat::test_that("simple effects ", {
    testthat::expect_equal(round(res[2, 2], 2), 6.64)
    testthat::expect_equal(round(res[2, 3], 2), 1)
})


mod <- GAMLj3::gamlj_glm(
    formula = schtyp ~ write + honors + honors:write,
    data = hsbdemo,
    estimates_ci = TRUE,
    model_type = "logistic",
    covs_scale = c(write = "standardized"),
    ci_method = "quantile"
)

res <- mod$main$coefficients$asDF[2, 3]
testthat::test_that("test scaling works ", {
    testthat::expect_equal(res, -0.214873, tol = .001)
})


data <- GAMLj3::get_data(mod)

testthat::test_that("get_data works ", {
    testthat::expect_equal(dim(data)[1], 200)
    testthat::expect_equal(names(data)[1], "schtyp")
})


####### custom contrasts


data("clustermanymodels")
data <- clustermanymodels
data$cat2 <- factor(data$cat2)
data$cat3 <- factor(data$cat3)
data$ybin <- factor(data$ybin)
data$ycat <- factor(data$ycat)


mod <- GAMLj3::gamlj_lm(
    data = data,
    formula = ycont ~ cat3 * cat2 + x,
    contrasts = c(cat3 = "custom"),
    contrast_custom_values = c(cat3 = "-1,2,2")
)


cont <- GAMLj3::test_contrasts(mod, contrasts = list(cat2 = c(-1, 1)))

testthat::test_that("test_contrasts works ", {
    testthat::expect_equal(cont$cat2$asDF$label, "{ -1*-1, 1*1 }")
    testthat::expect_equal(cont$cat2$asDF$estimate, 1.58198, tol)
})



