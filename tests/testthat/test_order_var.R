testthat::test_that("order_var works with nlme residual structures", {
    testthat::skip_if_not_installed("nlme")

    set.seed(42)
    data <- data.frame(
        id = factor(rep(seq_len(30), each = 6)),
        time = factor(rep(seq_len(6), 30)),
        x = rnorm(180)
    )
    data$y <- 1 + .4 * data$x + .15 * as.numeric(data$time) +
        rep(rnorm(30, sd = .5), each = 6) + rnorm(180, sd = .7)

    to_b64 <- function(x) GAMLj3:::.tob64.default(x)
    data64 <- data
    names(data64) <- to_b64(names(data64))

    formulas <- list(
        formula64 = function() paste(
            to_b64("y"), "~ 1 +", to_b64("x"), "+ (1 |", to_b64("id"), ")"
        ),
        fixed_formula64 = function() stats::as.formula(
            paste(to_b64("y"), "~ 1 +", to_b64("x"))
        ),
        listify_random_formulas64 = function() list(
            stats::as.formula(paste("~ 1 |", to_b64("id")))
        )
    )
    datamatic <- list(
        variables = setNames(
            list(list(levels_labels = NULL)),
            to_b64("y")
        )
    )

    fit_lme <- function(res_struct, order_var = NULL) {
        options <- GAMLj3:::gamljmixedOptions$new(
            .caller = "lmer",
            .interface = "jamovi",
            model_type = "lmer",
            dep = "y",
            order_var = order_var,
            cluster = "id",
            reml = TRUE,
            res_struct = res_struct
        )
        info <- GAMLj3:::Infomatic$new(options, datamatic, formulas)
        model <- do.call(
            GAMLj3:::estimate_lme,
            c(info$calloptions, list(data = data64))
        )
        list(info = info, model = model)
    }

    for (res_struct in c("ar1", "un", "cs", "arma")) {
        fitted <- fit_lme(res_struct, order_var = "time")
        testthat::expect_s3_class(fitted$model, "lme")
        testthat::expect_match(fitted$info$calloptions$form, "as.numeric")
        testthat::expect_match(
            deparse(stats::formula(fitted$model$modelStruct$corStruct)),
            "as.numeric"
        )
    }

    without_order <- fit_lme("ar1")
    testthat::expect_s3_class(without_order$model, "lme")
    testthat::expect_false(grepl("as.numeric", without_order$info$calloptions$form, fixed = TRUE))
    testthat::expect_match(
        deparse(stats::formula(without_order$model$modelStruct$corStruct)),
        "~1"
    )
})
