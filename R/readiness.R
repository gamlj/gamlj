readiness <- function(options) {
    if (options$.interface == "R") {
        if (!check_package(options, "nlme", c(model_type = "lmer", res_struct = "!id"), "structured error covariances")) {
            return(list(ready = FALSE, reason = FALSE, report = FALSE))
        }

        if (!check_package(options, "boot", c(ci_method = "!wald"), "for bootstrap confidence intervals")) {
            return(list(ready = FALSE, reason = FALSE, report = FALSE))
        }

        if (!check_package(options, "marginaleffects", c(es = "marginals"), "marginals effects")) {
            return(list(ready = FALSE, reason = FALSE, report = FALSE))
        }

        if (!check_package(options, "nnet", c(model_type = "multinomial", .caller = "glm"), "multinomial models")) {
            return(list(ready = FALSE, reason = FALSE, report = FALSE))
        }

        if (!check_package(options, "mclogit", c(model_type = "multinomial", .caller = "glmer"), "multinomial models")) {
            return(list(ready = FALSE, reason = FALSE, report = FALSE))
        }

        if (!check_package(options, "betareg", c(model_type = "beta", .caller = "glm"), "beta models")) {
            return(list(ready = FALSE, reason = FALSE, report = FALSE))
        }

        if (!check_package(options, "ordinal", c(model_type = "ordinal"), "ordinal models")) {
            return(list(ready = FALSE, reason = FALSE, report = FALSE))
        }

        if (!check_package(options, "MASS", c(model_type = "nb", .caller = "glm"), "negative binomial models")) {
            return(list(ready = FALSE, reason = FALSE, report = FALSE))
        }

        if (!check_package(options, "lme4", c(model_type = "nb", .caller = "glmer"), "negative binomial models")) {
            return(list(ready = FALSE, reason = FALSE, report = FALSE))
        }

        if (!check_package(options, "sandwich", c(se_method = "robust"), "robust standard errors")) {
            return(list(ready = FALSE, reason = FALSE, report = FALSE))
        }
    }

    result <- list(reason = NULL, ready = TRUE, report = FALSE)

    if (isTRUE(options$donotrun)) {
        result$ready <- FALSE
        result$report <- TRUE
        result$reason <- "Do not run option activated"
        return(result)
    }

    if (!is.something(options$dep)) {
        result$ready <- FALSE
        result$report <- TRUE
        result$reason <- "Please select the dependent variable"
        return(result)
    }


    if (is.joption(options, "input_method")) {
        if (options$input_method != "standard" && !is.something(options$dep2)) {
            result$ready <- FALSE
            result$report <- TRUE
            result$reason <- "Please define all dependent variable fields"
            return(result)
        }
    }

    if (is.joption(options, "cluster")) {
        if (!is.something(options$cluster)) {
            result$ready <- FALSE
            result$report <- TRUE
            result$reason <- "Please select a cluster variable"
            return(result)
        }
    }

    if (is.joption(options, "re")) {
        if (any(sapply(options$re, function(x) length(x) == 0))) {
            result$ready <- FALSE
            result$report <- TRUE
            result$reason <- "Please define the random coefficients"
            return(result)
        }
    }

    if (is.joption(options, "contrast_custom_values")) {
        ## is custom contrasts are defined but no codes are input we stop
        types <- unlist(lapply(options$contrasts, function(x) x$type))
        test <- any(types == "custom")
        if (!test) {
            return(result)
        }
        test <- any(sapply(options$contrast_custom_values, function(x) (stringr::str_length(x$codes) == 0)))
        if (test) {
            result$ready <- FALSE
            result$report <- TRUE
            result$reason <- "Please define all custom contrasts"
            return(result)
        }
    }



    return(result)
}


check_package <- function(options, pkg, conditions, aim) {
    neg <- grep("!", conditions)
    names <- names(conditions)
    conditions <- stringr::str_remove(conditions, "!")
    names(conditions) <- names
    str1 <- paste0("is.joption(options,", paste0("'", names(conditions), "'"), ")", collapse = " && ")
    str2 <- paste("(", paste(paste0("'", conditions, "' "), paste0("options$", names(conditions)), sep = " %in% "), ")")
    if (length(neg) > 0) str2[[neg]] <- paste0("!", str2[[neg]])
    str2 <- paste(str2, collapse = " && ")
    str <- str2lang(paste(str1, str2, sep = " && "))
    test <- eval(str)

    if (test) {
        if (!requireNamespace(pkg, quietly = T)) {
            msg <- paste0("Package ", pkg, " is required for ", aim, ". Please install it and re-run the model.")
            cat(paste0("\033[0;31m", msg, "\033[0m", "\n"))
            return(FALSE)
        }
    }
    return(TRUE)
}
