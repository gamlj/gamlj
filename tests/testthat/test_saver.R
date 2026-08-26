test_that("saved residuals retain model-frame row names", {
    data <- data.frame(
        y = c(1, NA, 3, 4),
        x = 1:4
    )
    model <- stats::lm(y ~ x, data = data)

    output_mock <- R6::R6Class(
        "SaverOutputMock",
        public = list(
            value = NULL,
            isNotFilled = function() TRUE,
            setValues = function(value) self$value <- value
        )
    )

    residuals_output <- output_mock$new()
    options <- list(
        names = c("predicted", "residuals", "export"),
        predicted = FALSE,
        residuals = TRUE,
        export = FALSE
    )
    analysis <- list(
        options = options,
        results = list(residuals = residuals_output)
    )

    saver <- GAMLj3:::Saver$new(
        analysis,
        list(model = model),
        NULL
    )
    saver$run()

    expect_identical(
        rownames(residuals_output$value),
        rownames(insight::get_data(model, source = "frame"))
    )
})
