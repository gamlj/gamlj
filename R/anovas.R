############# produces anova/deviance table in a somehow standard format ##########
ganova <- function(x, ...) UseMethod(".anova")

.anova.default <- function(model, obj) {
    stop("GANOVA: no suitable model found")
}


.anova.glm <- function(model, obj, test = "LR") {
 
    norobust<-c("multinomial")
    if (!obj$formulaobj$hasTerms) {
        obj$warning <- list(topic = "main_anova", message = "Omnibus tests cannot be computed")
        return(NULL)
    }
  
    opts<-list(model, test = test, type = 3, singular.ok = T)
    
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

    d0 <- null.deviance(model)
    .anova$etaSq <- .anova$test / d0
    .anova
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
    d0 <- null.deviance(model)
    .anova$etaSq <- .anova$test / d0
    .anova <- .anova[rownames(.anova) != "(Intercept)", ]
    .anova
}

.anova.betareg <- function(model, obj) {
    if (!obj$formulaobj$hasTerms) {
        obj$warning <- list(topic = "main_anova", message = "Omnibus tests cannot be computed")
        return(NULL)
    }

    anoobj <- try_hard(car::Anova(model, test = "Chisq", type = 3, singular.ok = T))
    obj$error <- list(topic = "main_anova", message = anoobj$error)
    if (!isFALSE(anoobj$error)) {
        return(NULL)
    }

    .anova <- as.data.frame(anoobj$obj, stringsAsFactors = F)
    .transnames <- list("test" = c("Chisq", "LR Chisq"), df = c("Df", "df1"), p = c("Pr(>Chisq)"))
    names(.anova) <- transnames(names(.anova), .transnames)

    .anova <- .anova[rownames(.anova) != "(Intercept)", ]

    #### effect size

    .anova
}

.anova.lm <- function(model, obj) {
    opts <- list(mod = model, test = "F", type = 3, singular.ok = T)
    .anova <- do.call(car::Anova, opts)
    .anova <- .anova[!(rownames(.anova) %in% c("(Intercept)")), ]
    anovatab <- .anova
    colnames(anovatab) <- c("ss", "df", "f", "p")
    effss <- anovatab[!(rownames(anovatab) %in% c("Residuals")), ]
    reds <- list(ss = anovatab$ss[rownames(anovatab) == "Residuals"], df = anovatab$df[rownames(anovatab) == "Residuals"])
    ## returns if model has no terms
    if (!obj$formulaobj$hasTerms) {
        tots <- list(ss = reds$ss, df = reds$df)
        return(list(reds, tots))
    }
    sumr <- summary(model)

    ### whole model ###
    f <- sumr$fstatistic[[1]]
    edf <- sumr$fstatistic[[3]]
    mdf <- sumr$fstatistic[[2]]
    p <- stats::pf(f, mdf, edf, lower.tail = F)
    modeta <- effectsize::F_to_eta2(f, mdf, edf)
    modomega <- effectsize::F_to_omega2(f, mdf, edf)
    modepsilon <- effectsize::F_to_epsilon2(f, mdf, edf)
    modss <- f * reds$ss * mdf / edf
    mods <- list(
        ss = modss,
        df = mdf,
        f = f,
        p = p,
        etaSq = modeta[[1]],
        etaSqP = modeta[[1]],
        omegaSq = modomega[[1]],
        omegaSqP = modomega[[1]],
        epsilonSq = modepsilon[[1]],
        epsilonSqP = modepsilon[[1]]
    )

    tots <- list(ss = mods$ss + reds$ss, df = mdf + edf)

    #####
    # Here we need a correct to the computation of the effect sizes. To compute the non-partial indexes
    ## In unbalanced designs, the sum does not necessarily correspond to the model SS (plus residuals)
    ## so the estimation is biased. Eta-squared does not correspond to semi-partial r^2 any more
    ## and many properties of the non-partial indices are broken.
    ## Thus, we fixed it by adding a bogus effect whose SS is exactly the discrepancy betweem
    ## the table SS and the model+error SS. In this way, the estimation uses the correct total SS
    #####
    diff <- mods$ss - sum(effss$ss)
    add <- data.frame(diff, 1, 1, 0)
    names(add) <- names(.anova)
    .canova <- rbind(.anova, add)
    last <- dim(effss)[1] + 1
    etap <- effectsize::eta_squared(.anova, partial = T, verbose = F)
    eta <- effectsize::eta_squared(.canova, partial = F, verbose = F)
    omegap <- effectsize::omega_squared(.anova, partial = T, verbose = F)
    omega <- effectsize::omega_squared(.canova, partial = F, verbose = F)
    epsilonp <- effectsize::epsilon_squared(.anova, partial = T, verbose = F)
    epsilon <- effectsize::epsilon_squared(.canova, partial = F, verbose = F)

    effss$etaSq <- eta[-last, 2]
    effss$etaSqP <- etap[, 2]

    effss$omegaSq <- omega[-last, 2]
    effss$omegaSqP <- omegap[, 2]
    effss$epsilonSq <- epsilon[-last, 2]
    effss$epsilonSqP <- epsilonp[, 2]

    if (obj$option("se_method", "robust")) {
        opts[["white.adjust"]] <- TRUE
        .anova <- do.call(car::Anova, opts)
        .anova <- .anova[!(rownames(.anova) %in% c("(Intercept)", "Residuals")), ]

        effss$f <- .anova$F
        effss$p <- .anova$`Pr(>F)`
        warning(WARNS[["stde.robust_test"]])
    }

    opts <- list(mod = model, test = "F", type = 3, singular.ok = T)
    .anova <- do.call(car::Anova, opts)

    reslist <- listify(effss)
    ladd(reslist) <- reds
    ladd(reslist) <- tots
    padd(reslist) <- mods

    reslist
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
    .anova <- car::Anova(model, type = 3, test = test)
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
