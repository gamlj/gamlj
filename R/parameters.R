############# produces parameters in a somehow standard format ##########

gparameters <- function(x, ...) UseMethod(".parameters")

.parameters.default <- function(model, obj) {
    jinfo("GPARAMETERS: default .parameters for class", class(model))

    .bootstrap <- obj$options$ci_method %in% c("quantile", "bcai")
    .iterations <- obj$options$boot_r
    .ci_method <- obj$options$ci_method
    .ci_width <- obj$ciwidth

    .se_method <- obj$options$se_method

    if (obj$option("se_method", "standard")) {
        .se_method <- NULL
    }

    if (obj$option("se_method", "robust")) {
        .se_method <- obj$options$robust_method
        warning(WARNS[["stde.robust_test"]])
    }
    if (is.something(obj$boot_model)) .model <- obj$boot_model else .model <- model

  
    .coefficients <- as.data.frame(parameters::parameters(
        model,
        vcov = .se_method,
        ci = NULL,
    ), stringAsFactors = FALSE)


    names(.coefficients) <- c("source", "estimate", "se", "test", "df", "p")

    if (obj$option("estimates_ci")) {
        cidata <- as.data.frame(parameters::ci(.model,
            ci = .ci_width,
            ci_method = .ci_method,
            vcov=.se_method
        ))

        .coefficients$est.ci.lower <- cidata$CI_low
        .coefficients$est.ci.upper <- cidata$CI_high
    }

    if (obj$option("es", "beta")) {
        if (obj$formulaobj$hasTerms) {
            ## if no CI are required, we do not bootstrap again
            if (!obj$option("betas_ci")) {
                ..bootstrap <- FALSE
                .ci_method <- "wald"
            } else {
                ..bootstrap <- .bootstrap
            }

            ### up to parameters 0.16.0, if bootstrap is required standardize does not work
            ### so we standardize before parameters() and feed the model to it

            opts_list <- list(
                model = mf.standardize(model),
                bootstrap = ..bootstrap,
                ci_method = .ci_method,
                ci = .ci_width,
                iterations = .iterations
            )

            if (..bootstrap) {
                jinfo("ESTIMATE: we need to reboostrap for betas CI")

                ### check if we can go in paraller ###
                test <- try_hard(find.package("parallel"))
                if (isFALSE(test$error)) {
                    opts_list[["n_cpus"]] <- parallel::detectCores()
                    opts_list[["parallel"]] <- "multicore"
                }
            }
            estim <- do.call(parameters::parameters, opts_list)

            .coefficients$beta <- estim$Coefficient
            .coefficients$beta.ci.lower <- estim$CI_low
            .coefficients$beta.ci.upper <- estim$CI_high
        } else {
            .coefficients$beta <- 0
            .coefficients$beta.ci.lower <- 0
            .coefficients$beta.ci.upper <- 0
        }
    }
    .coefficients
}


.parameters.glm <- function(model, obj) {
  
    .bootstrap  <- obj$options$ci_method %in% c("quantile", "bcai")
    .iterations <- obj$options$boot_r
    .ci_method  <- obj$options$ci_method
    .ci_width   <- obj$ciwidth
    .se_method  <- NULL
  
    norobust<-c("ordinal")
   
    if (obj$option("se_method", "robust")) {
      if (obj$options$model_type %in% norobust)
           warning("Robust estimation not available for parameters of ",obj$options$model_type)
       else {
         .se_method <- obj$options$robust_method
          warning(WARNS[["stde.robust_test"]])
       }
    }  

    if (is.something(obj$boot_model)) .model <- obj$boot_model else .model <- model

    .coefficients <- as.data.frame(parameters::parameters(
        model,
        vcov = .se_method,
        ci = NULL,
        effects = "fixed"
    ), stringAsFactors = FALSE)


    .transnames <- list(
        source = "Parameter",
        estimate = "Coefficient",
        se = "SE",
        test = c("z", "t"),
        df = "df_error"
    )
    names(.coefficients) <- transnames(names(.coefficients), .transnames)

    ## Robust covariance matrices can be used by ci.glm() only for Wald-like
    ## intervals.  Keep bootstrap intervals independent of the SE option and
    ## leave the non-glm methods handled by this function unchanged.
    .ci_vcov <- NULL
    if (!.bootstrap && inherits(model, "glm")) {
        .ci_vcov <- .se_method
        if (!is.null(.ci_vcov) && identical(.ci_method, "profile")) {
            warning("Profile confidence intervals are not available with robust standard errors. Wald confidence intervals based on the robust covariance matrix were used.")
            .ci_method <- "wald"
        }
    }

    .ci_args <- list(
        x = .model,
        method = .ci_method,
        ci = .ci_width
    )
    if (!is.null(.ci_vcov)) .ci_args$vcov <- .ci_vcov
    cidata <- do.call(parameters::ci, .ci_args)
    cidata <- cidata[seq_len(nrow(.coefficients)), , drop = FALSE]
    .coefficients$est.ci.lower <- cidata$CI_low
    .coefficients$est.ci.upper <- cidata$CI_high

    .coefficients$expb <- exp(.coefficients$estimate)


    if (obj$option("expb_ci") & obj$option("es", "expb")) {
        if (inherits(.model, "bootstrap_model")) {
            .classes <- class(.model)
            .names <- names(.model)
            .attributes <- attributes(.model)
            x <- as.data.frame(exp(as.matrix(.model)))
            names(x) <- .names
            attributes(x) <- .attributes
            class(x) <- .classes
            cidata <- parameters::ci(x, method = .ci_method, ci = .ci_width)
            cidata <- cidata[seq_len(nrow(.coefficients)), , drop = FALSE]
            .coefficients$expb.ci.lower <- cidata$CI_low
            .coefficients$expb.ci.upper <- cidata$CI_high
        } else {
            .coefficients$expb.ci.lower <- exp(.coefficients$est.ci.lower)
            .coefficients$expb.ci.upper <- exp(.coefficients$est.ci.upper)
        }
    }

    ## Keep the existing CI path for models that reuse this method but are
    ## not glm objects (for example multinom, betareg, and clm).
    if (obj$option("estimates_ci") && !inherits(model, "glm")) {
        cidata <- as.data.frame(parameters::ci(.model,
            ci = .ci_width,
            ci_method = .ci_method,
            vcov = .se_method
        ))

        .coefficients$est.ci.lower <- cidata$CI_low
        .coefficients$est.ci.upper <- cidata$CI_high
    }

    .coefficients
}

.parameters.multinom <- function(model, obj) {
    .coefficients <- .parameters.glm(model, obj)
    names(.coefficients) <- tolower(names(.coefficients))
    ### fix the names
    dep<-all.vars(formula(model))[[1]]
    levs<-tob64(obj$datamatic$variables[[dep]]$levels)
    dummies<-gsub(LEVEL_SYMBOL,"",unique(.coefficients$response))
    ref<-setdiff(levs,dummies)
    dummies<-paste(dummies,ref,sep=" - ")
    .coefficients$response<-dummies
    .coefficients
}

.parameters.betareg <- function(model, obj) {

    .coefficients <- .parameters.glm(model, obj)
    .coefficients
}



.parameters.mmblogit <- function(model, obj) {
    ss <- mclogit::getSummary.mblogit(model)
    .names <- dimnames(ss$coef)[[3]]
    alist <- list()
    for (i in seq_along(.names)) {
        one <- as.data.frame(ss$coef[, , i])
        one$response <- as.character(.names[i])
        ladd(alist) <- one
    }
    .coefficients <- as.data.frame(do.call("rbind", alist))
    .coefficients$source <- gsub("\\).", ")", rownames(.coefficients))
    .transnames <- list(
        estimate = "est",
        test = c("stat"),
        est.ci.lower = "lwr", est.ci.upper = "upr"
    )
    names(.coefficients) <- transnames(names(.coefficients), .transnames)
    if (obj$option("es", "expb")) {
        .coefficients$expb <- exp(.coefficients$estimate)
        .coefficients$expb.ci.lower <- exp(.coefficients$est.ci.lower)
        .coefficients$expb.ci.upper <- exp(.coefficients$est.ci.upper)
    }
    ## clean names
    for (var in obj$datamatic$variables) {
        for (name in var$paramsnames64) {
            test <- grep(name, .coefficients$source)
            if (length(test) > 0) .coefficients$source[test] <- name
        }
    }
    ## fix response labels
    .coefficients$response <- sub("/", " - ", .coefficients$response, fixed = TRUE)
    .coefficients
}

.parameters.clm <- function(model, obj) {
    params <- .parameters.glm(model, obj)
    params$label <- params$source
    check <- grep(LEVEL_SYMBOL, params$source, fixed = TRUE)
    params$source[check] <- "(Threshold)"
    params$label <- sub("|", " - ", params$label, fixed = TRUE)
    params
}

.parameters.clmm <- function(model, obj) {
    params <- .parameters.clm(model, obj)
    params <- params[params$Effects == "fixed", ]
    params
}


.parameters.lmerModLmerTest <- function(model, obj) {
  
     jinfo("RUNNER: estimating parameters for lmerMod")
    .bootstrap <- obj$options$ci_method %in% c("quantile", "bcai")
    .iterations <- obj$options$boot_r
    .ci_method <- obj$options$ci_method
    .ci_width <- obj$ciwidth
    .df_method <- switch(obj$options$df_method,
        Satterthwaite = "satterthwaite",
        "Kenward-Roger" = "kenward"
    )

    ss<-summary(model,ddf=obj$options$df_method)
    
    .coefficients<-as.data.frame(ss$coefficients)
    .coefficients=cbind(source=rownames(.coefficients),.coefficients)
  
    ## parameters::parameters fails with non-identifiable models
    
#    .coefficients <- as.data.frame(parameters::parameters(
#        model,
#        ci = NULL,
#        effects = "fixed",
#        ci_method = .df_method
#    ), stringAsFactors = FALSE)
    
   

    names(.coefficients) <- c("source", "estimate", "se", "df", "test", "p")

    if (obj$option("estimates_ci")) {
        if (is.something(obj$boot_model)) .model <- obj$boot_model else .model <- model
        
        cidata <- as.data.frame(parameters::ci(.model,
            ci = .ci_width,
            ci_method = .ci_method
        ))

        .coefficients$est.ci.lower <- cidata$CI_low
        .coefficients$est.ci.upper <- cidata$CI_high
    }
    return(.coefficients)
}

.parameters.glmerMod <- function(model, obj) {
    jinfo("GPARAMETERS: glmerMod .parameters for class", class(model))

    .bootstrap <- obj$options$ci_method %in% c("quantile", "bcai")
    .iterations <- obj$options$boot_r
    .ci_method <- obj$options$ci_method
    .ci_width <- obj$ciwidth
    .coefficients <- as.data.frame(parameters::parameters(
        model,
        ci = NULL,
        effects = "fixed",
    ), stringAsFactors = FALSE)

    names(.coefficients) <- c("source", "estimate", "se", "test", "df", "p")


    if (is.something(obj$boot_model)) .model <- obj$boot_model else .model <- model

    if (obj$option("expb_ci") | obj$option("estimates_ci")) {
        cidata <- as.data.frame(parameters::ci(.model,ci=.ci_width))
        .coefficients$expb <- exp(.coefficients$estimate)
        .coefficients$expb.ci.lower <- exp(cidata$CI_low)
        .coefficients$expb.ci.upper <- exp(cidata$CI_high)
        .coefficients$est.ci.lower <- cidata$CI_low
        .coefficients$est.ci.upper <- cidata$CI_high
    }


    return(.coefficients)
}

.parameters.lme <- function(model, obj) {
    jinfo("GPARAMETERS: lme .parameters for class", class(model))

    .bootstrap <- obj$options$ci_method %in% c("quantile", "bcai")
    .iterations <- obj$options$boot_r
    .ci_method <- obj$options$ci_method
    .ci_width <- obj$ciwidth
    .coefficients <- as.data.frame(parameters::parameters(
        model,
        ci = NULL,
        effects = "fixed",
    ), stringAsFactors = FALSE)

    names(.coefficients) <- c("source", "estimate", "se", "test", "df", "p")


    if (obj$option("estimates_ci")) {
        if (is.something(obj$boot_model)) .model <- obj$boot_model else .model <- model


        cidata <- as.data.frame(parameters::ci(.model,
            ci = .ci_width,
            ci_method = .ci_method
        ))

        .coefficients$est.ci.lower <- cidata$CI_low
        .coefficients$est.ci.upper <- cidata$CI_high
    }
    return(.coefficients)
}
