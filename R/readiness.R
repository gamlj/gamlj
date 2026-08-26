readiness <- function(options) {
    package_checks <- list(
        list(pkg = "nlme", conditions = c(model_type = "lmer", res_struct = "!id"), aim = "structured error covariances"),
        list(pkg = "boot", conditions = c(ci_method = "!wald"), aim = "bootstrap confidence intervals"),
        list(pkg = "marginaleffects", conditions = c(es = "marginals"), aim = "marginal effects"),
        list(pkg = "nnet", conditions = c(model_type = "multinomial", .caller = "glm"), aim = "multinomial models"),
        list(pkg = "mclogit", conditions = c(model_type = "multinomial", .caller = "glmer"), aim = "multinomial models"),
        list(pkg = "betareg", conditions = c(model_type = "beta", .caller = "glm"), aim = "beta models"),
        list(pkg = "ordinal", conditions = c(model_type = "ordinal"), aim = "ordinal models"),
        list(pkg = "MASS", conditions = c(model_type = "nb", .caller = "glm"), aim = "negative binomial models"),
        list(pkg = "lme4", conditions = c(model_type = "nb", .caller = "glmer"), aim = "negative binomial models"),
        list(pkg = "sandwich", conditions = c(se_method = "robust"), aim = "robust standard errors")
    )

    for (check in package_checks) {
        available <- check_package(options, check$pkg, check$conditions, check$aim)
        if (!available) {
            return(list(
                ready = FALSE,
                reason = attr(available, "message"),
                report = TRUE
            ))
        }
    }

    result <- list(reason = NULL, ready = TRUE, report = FALSE)

 
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
    neg <- grep("!", conditions, fixed = TRUE)
    names <- names(conditions)
    conditions <- sub("^!", "", conditions)
    names(conditions) <- names
    str1 <- paste0("is.joption(options,", paste0("'", names(conditions), "'"), ")", collapse = " && ")
    str2 <- paste("(", paste(paste0("'", conditions, "' "), paste0("options$", names(conditions)), sep = " %in% "), ")")
    if (length(neg) > 0) str2[neg] <- paste0("!", str2[neg])
    str2 <- paste(str2, collapse = " && ")
    str <- str2lang(paste(str1, str2, sep = " && "))
    test <- eval(str)

    if (test) {
        if (!requireNamespace(pkg, quietly = T)) {
            msg <- paste0("Package ", pkg, " is required for ", aim, ". Please install it and re-run the model.")
            return(structure(FALSE, message = msg))
        }
    }
    return(TRUE)
}
