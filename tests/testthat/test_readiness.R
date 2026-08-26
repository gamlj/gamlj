test_that("check_package handles multiple negated conditions", {
    options <- list(
        names = c("first", "second"),
        first = "not-x",
        second = "not-y"
    )

    expect_true(
        GAMLj3:::check_package(
            options,
            "base",
            c(first = "!x", second = "!y"),
            "the readiness test"
        )
    )
})
