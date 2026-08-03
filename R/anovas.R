############# produces anova/deviance table in a somehow standard format ##########
ganova <- function(x, ...) UseMethod(".anova")

.anova.default <- function(model, obj) {
    stop("GANOVA: no suitable model found")
}


.anova.glm <- function(model, obj, test = "LR") {
 
   
    if (!obj$formulaobj$hasTerms) {
        obj$warning <- list(topic = "main_anova", message = "Omnibus tests cannot be computed")
        return(NULL)
    }
  
    opts<-list(model, test = test, type = 3, singular.ok = T)
    
    norobust<-c("multinomial") 
    if (obj$option("se_method", "robust")) {
     if (obj$options$model_type %in% norobust)
       warning("Robust estimation not available for omnibus tests of ",obj$options$model_type)
      else {
         opts$vcov<-sandwich::vcovHC(model,type=obj$options$robust_method)
         opts$test<-"Wald"
         warning(WARNS[["stde.robust_test"]])
      }
    }
   
    anoobj <- try_hard(do.call(car::Anova,opts))

    ### LR is less lenient than Wald
    if (!isFALSE(anoobj$error)) {
        opts$test<-"Wald"
        anoobj <- try_hard(do.call(car::Anova,opts))
        obj$warning <- list(topic = "main_anova", message = "Wald test was used because LRT failed")
    }
    obj$error <- list(topic = "main_anova", message = anoobj$error)
    obj$warning <- list(topic = "main_anova", message = anoobj$warning)


    if (!isFALSE(anoobj$error)) {
        return(NULL)
    }
    
    .anova <- as.data.frame(anoobj$obj, stringsAsFactors = F)
    .transnames <- list("test" = c("Chisq", "LR Chisq"), df = c("Df", "df1"), p = c("Pr(>Chisq)"))
    names(.anova) <- transnames(names(.anova), .transnames)

    .anova <- .anova[rownames(.anova) != "(Intercept)", ]

    
    #### effect size
    class(.anova) <- c("main_anova_glm", class(.anova))
    .anova <- add_effect_size(.anova, model, obj)
    
    return(.anova)
}

.anova.multinom <- function(model, obj) {
    .anova.glm(model, obj)
}

.anova.clm <- function(model, obj) {
    if (!obj$formulaobj$hasTerms) {
        obj$warning <- list(topic = "main_anova", message = "Omnibus tests cannot be computed")
        return(NULL)
    }
  
    if (obj$options$se_method=="robust") warning(WARNS["norobustanova"])

    anoobj <- try_hard(stats::anova(model, type = 3))
    obj$error <- list(topic = "main_anova", message = anoobj$error)
    obj$warning <- list(topic = "main_anova", message = anoobj$warning)

    if (!isFALSE(anoobj$error)) {
        return(NULL)
    }

    .anova <- as.data.frame(anoobj$obj, stringsAsFactors = F)
    .transnames <- list("test" = c("Chisq", "LR Chisq"), df = c("Df", "df1"), p = c("Pr(>Chisq)"))
    names(.anova) <- transnames(names(.anova), .transnames)
    class(.anova) <- c("main_anova_clm", class(.anova))
    .anova<-add_effect_size(.anova,model,obj)
    .anova
}

.anova.betareg <- function(model, obj) {
  
    if (!obj$formulaobj$hasTerms) {
        obj$warning <- list(topic = "main_anova", message = "Omnibus tests cannot be computed")
        return(NULL)
    }

  opts<-list(mod=model,test = "Chisq", type = 3, singular.ok = T)
  
  if (obj$option("se_method", "robust")) {
      warning(WARNS[["norobustanova"]])
    }
  
  
    anoobj <- try_hard(do.call(car::Anova,opts))
    obj$error <- list(topic = "main_anova", message = anoobj$error)
    if (!isFALSE(anoobj$error)) {
        return(NULL)
    }

    .anova <- as.data.frame(anoobj$obj, stringsAsFactors = F)
    .transnames <- list("test" = c("Chisq", "LR Chisq"), df = c("Df", "df1"), p = c("Pr(>Chisq)"))
    names(.anova) <- transnames(names(.anova), .transnames)

    .anova <- .anova[rownames(.anova) != "(Intercept)", ]
     class(.anova) <- c("main_anova_beta", class(.anova))
    .anova<-add_effect_size(.anova,model,obj)
    
    #### effect size

    .anova
}

.anova.lm <- function(model, obj) {
    opts <- list(mod = model, test = "F", type = 3, singular.ok = TRUE)
    .anova <- do.call(car::Anova, opts)
    .anova <- .anova[!(rownames(.anova) %in% c("(Intercept)")), , drop = FALSE]
    .anova <- as.data.frame(.anova)
    names(.anova) <- c("ss", "df", "f", "p")
    residual_rows <- rownames(.anova) == "Residuals"
    if (any(residual_rows)) {
        ssres <- .anova$ss[residual_rows][[1]]
        dfres <- .anova$df[residual_rows][[1]]
    } else {
        dfres <- model$df.residual
        ssres <- stats::sigma(model)^2 * dfres
    }

    fstatistic <- summary(model)$fstatistic
    if (is.null(fstatistic)) {
        modss <- 0
        mdf <- 0
        f <- NA_real_
        p <- NA_real_
    } else {
        f <- fstatistic[[1]]
        mdf <- fstatistic[[2]]
        p <- stats::pf(f, mdf, fstatistic[[3]], lower.tail = FALSE)
        modss <- f * ssres * mdf / fstatistic[[3]]
    }

    model_row <- data.frame(
        ss = modss,
        df = mdf,
        f = f,
        p = p,
        row.names = "Model"
    )
    total_row <- data.frame(
        ss = modss + ssres,
        df = mdf + dfres,
        f = NA_real_,
        p = NA_real_,
        row.names = "Total"
    )

    .anova <- rbind(model_row, .anova, total_row)
    class(.anova) <- c("main_anova_lm", class(.anova))
    add_effect_size(.anova, model, obj)
}

.anova.glmerMod <- function(model, obj) {
    jinfo("GANOVA: glmerMod for class", class(model))

    ano <- .car.anova(model)
    names(ano) <- c("test", "df", "p")
    if (nrow(ano) == 0) ano <- NULL
    ano
}


.anova.lmerModLmerTest <- function(model, obj) {
    if (!obj$formulaobj$hasTerms) {
        return()
    }

    df <- obj$options$df_method
    results <- try_hard(stats::anova(model, type = "3", ddf = df))
    if (!isFALSE(results$warning)) {
        lapply(results$warning, function(x) obj$warning <- list(topic = "main_anova", message = x))
    }
    if (!isFALSE(results$error)) {
        obj$error <- list(topic = "main_anova", message = results$error)
    }


    .anova <- results$obj
    if (dim(.anova)[1] == 0) {
        obj$warning <- list(topic = "main_anova", message = "F-Tests cannot be computed without fixed effects")
        return(.anova)
    }
    if (dim(.anova)[2] == 4) {
        .anova <- .car.anova(model, df)
        obj$warning <- list(topic = "main_anova", message = "Degrees of freedom computed with method Kenward-Roger")
    }

    .transnames <- list("f" = c("F", "F value"), df1 = c("Df", "NumDF"), df2 = c("Df.res", "DenDF"), p = ("Pr(>F)"))
    names(.anova) <- transnames(names(.anova), .transnames)

    return(.anova)
}

.anova.lme <- function(model, obj) {
    jinfo("GANOVA: lme for class", class(model))
    ano <- stats::anova(model, type = "marginal")
    names(ano) <- c("df1", "df2", "f", "p")
    if (nrow(ano) == 0) ano <- NULL
    ano[-1, ]
}



.anova.clmm <- function(model, obj) {
    jinfo("anova for clmm")
    ## at the moment ordinal::anova.clmm does not work and drop1 tests
    ## only the higher order term. So we go all the way with a custom
    ## drop. We also have to be careful when there is only one predictors,
    ## because drop1 will not work . This results is Type II testing

    if (!obj$formulaobj$hasTerms) {
        return()
    }

    results <- emmeans::joint_tests(model)

    .names <- list(
        df   = c("df1"),
        test = c("Chisq"),
        p    = c("p.value")
    )
    names(results) <- transnames(names(results), .names)


    results$source <- fromb64(results$source)
    results
}

.anova.mmblogit <- function(model, obj) {
    return()
}


.car.anova <- function(model, df) {
    jinfo("GANOVA: car::Anova is used")

    if (model@devcomp$dims["REML"] == 0) {
        test <- "Chisq"
    } else {
        test <- "F"
    }
    .anova <- car::Anova(model, type = 3, test.statistic = test)
    if (attr(stats::terms(model), "intercept") == 1) {
        .anova <- .anova[-1, ]
    }

    attr(.anova, "method") <- "Kenward-Roger"
    attr(.anova, "statistic") <- test
    .anova
}

## test for random variances

anovas.ranova <- function(x, ...) UseMethod(".ranova")

.ranova.default <- function(model, obj) {
    warning("Random coefficients LRT not available for model:", obj$infomatic$model[1])
    list(list(test = "Not available"))
}

.ranova.lmerMod <- function(model, obj) {
    data <- model@frame
    tab <- as.data.frame(lmerTest::ranova(model))
    tab <- tab[-1, ]
    .names <- list(LRT = "Chisq", df = "Df", p = "Pr(>Chisq)")
    names(tab) <- transnames(names(tab), .names)
    tab$test <- fromb64(rownames(tab))
    tab
}


.ranova.lme <- function(model, obj) {
    jinfo("ranova for lme")
    models <- obj$formulaobj$reduced_random()
    fixed <- obj$formulaobj$fixed_formula64()
    .names <- list(LRT = c("Chisq", "L.Ratio"), npar = "df", p = "p-value")

    tab <- lapply(names(models), function(x) {
        .formula <- fixed
        if (is.something(models[[x]])) {
            .formula <- paste(fixed, models[[x]], sep = " + ")
        }
        model0 <- mf.update(model, formula = .formula)
        .anova <- stats::anova(model, model0)[2, ]
        names(.anova) <- transnames(names(.anova), .names)
        .anova$test <- x
        .anova
    })
    tab
}


.ranova.glmerMod <- function(model, obj) {
    jinfo("ranova for glmerMod")
    models <- obj$formulaobj$reduced_random()
    fixed <- obj$formulaobj$fixed_formula64()
    .names <- list(LRT = "Chisq", df = "Df", p = "Pr(>Chisq)")

    tab <- lapply(names(models), function(x) {
        .formula <- fixed
        if (is.something(models[[x]])) {
            .formula <- paste(fixed, models[[x]], sep = " + ")
        }
        model0 <- mf.update(model, formula = .formula)
        .anova <- stats::anova(model, model0)[2, ]
        names(.anova) <- transnames(names(.anova), .names)
        .anova$test <- x
        .anova
    })
    tab
}

.ranova.clmm <- function(model, obj) {
    jinfo("ranova for clmm")

    models <- obj$formulaobj$reduced_random()
    fixed <- obj$formulaobj$fixed_formula64()
    .names <- list(LRT = "Chisq", df = "Df", p = "Pr(>Chisq)")
    tab <- lapply(names(models), function(x) {
        .formula <- fixed
        if (is.something(models[[x]])) {
            .formula <- paste(fixed, models[[x]], sep = " + ")
        }
        model0 <- mf.update(model, formula = .formula)
        aic <- (-2 * model0$logLik + 2 * model$edf)
        .anova <- as.data.frame(performance::test_likelihoodratio(model0, model))[2, ]
        names(.anova) <- c("name", "model", "npar", "df", "LRT", "p")
        .anova$AIC <- aic
        .anova$test <- x
        .anova
    })
    tab
}
