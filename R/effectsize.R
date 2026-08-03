### marginal effects ###

es.marginals <- function(x, ...) UseMethod(".margins")

.margins.default <- function(obj) {
    model <- obj$model
    jinfo("EFFECTSIZE: margins default for model of class", class(model))
    ciWidth <- obj$ciwidth
    results <- try_hard(marginaleffects::avg_slopes(model))
    params <- results$obj
    if (!isFALSE(results$error) && inherits(model, "glm")) {
        formula <- stats::as.formula(fromb64(paste(deparse(stats::formula(model)), collapse = "")))
        data <- insight::get_data(model, source = "frame")
        names(data) <- fromb64(names(data))
        for (name in names(data)) {
            if (is.factor(data[[name]])) {
                levels(data[[name]]) <- fromb64(levels(data[[name]]))
            }
        }

        decoded_model <- stats::glm(formula = formula, data = data, family = model$family)
        results <- try_hard(marginaleffects::avg_slopes(decoded_model))
        params <- results$obj
    }
    if (!isFALSE(results$error)) {
        obj$error <- list(topic = "main_marginals", message = "Marginal effects cannot be computed for this model")
        return()
    }

    if (obj$option("ci_method", c("quantile", "bcai"))) {
        method <- "boot"
        switch(obj$options$ci_method,
            quantile = type <- "perc",
            bcai     = type <- "bca"
        )
        params <- as.data.frame(marginaleffects::inferences(params, method = "boot", R = obj$options$boot_r, conf_type = type))
    }
    params <- as.data.frame(params)
    names(params) <- transnames(
        names(params),
        list(
            "source" = "term",
            "response" = "group",
            "se" = "std.error",
            "estimate" = "AME",
            "test" = "statistic",
            "est.ci.lower" = "conf.low",
            "est.ci.upper" = "conf.high",
            "p" = "p.value"
        )
    )
    params$contrast <- fromb64(params$contrast)
    params$source <- fromb64(params$source)
    if (utils::hasName(params, "response")) {
        params$response <- fromb64(params$response)
    }
    return(params)
}


#### Relative risk ######


es.relativerisk <- function(obj) {
    model <- obj$model
    data <- insight::get_data(model, source = "frame")
    ciWidth <- obj$ciwidth
    depobj <- obj$datamatic$variables[[tob64(obj$options$dep)]]
    levs <- levels(data[[depobj$name64]])


    data[, depobj$name64] <- as.numeric(data[[depobj$name64]] == levs[2])

    ## in previous versions the geepack::gee() poisson model was used
    ## now we use glm poisson(log) because with robust standard errors
    ## results are practically the same and it's faster (and we do not
    ## need to load geepack)

    results <- stats::update(model, data = data, family = stats::poisson())
    params <- as.data.frame(parameters::parameters(results,
        vcov = sandwich::vcovHC,
        ci_method = "wald",
        exponentiate = TRUE,
        effects = "fixed"
    ))

    if (!obj$option("ci_method", "wald")) {
        warning("Wald method for confidence intervals has been used")
    }

    names(params) <- c("source", "estimate", "se", "nothing", "est.ci.lower", "est.ci.upper", "test", "df", "p")

    return(params)
}

es.custom_variances <- function(model, obj) {
    names64 <- unlist(lapply(obj$datamatic$variables, function(x) if (x$method == "custom") x$paramsnames64[[1]] else NULL))

    if (length(names64) == 0) {
        return()
    }

    .anova <- car::Anova(model, type = "III")
    atable <- as.data.frame(.anova[c(-1, -dim(.anova)[1]), ])
    names(atable) <- c("SS", "df", "test", "p")
    dfres <- model$df.residual
    sumr <- summary(model)
    N <- dfres + sumr$fstatistic[[2]] + 1
    ssres <- stats::sigma(model)^2 * dfres
    ssmod <- sumr$fstatistic[[1]] * sumr$fstatistic[[2]] * ssres / dfres

    ## CONTRASTS
    eff <- stats::coef(model)[names64]
    SS <- eff^2 * N - 1
    df <- rep(1, length(SS))

    es <- SS / (ssmod + ssres)
    etaSq <- ci_effectsize_lm(es, df, dfres, obj, "eta")

    es <- SS / (SS + ssres)
    etaSqP <- ci_effectsize_lm(es, df, dfres, obj, "etap")

    es <- (SS - (ssres * df / dfres)) / (ssmod + (ssres * (dfres + 1) / dfres))
    omegaSq <- ci_effectsize_lm(es, df, dfres, obj, "omega")

    es <- (SS - (ssres * df / dfres)) / (SS + (ssres * (N - df) / dfres))
    omegaSqP <- ci_effectsize_lm(es, df, dfres, obj, "omegap")

    es <- (SS - (ssres * df / dfres)) / (ssmod + ssres)
    epsilonSq <- ci_effectsize_lm(es, df, dfres, obj, "epsilon")

    es <- (SS - (ssres * df / dfres)) / (SS + ssres)
    epsilonSqP <- ci_effectsize_lm(es, df, dfres, obj, "epsilonp")

    alist <- list()
    for (i in seq_along(etaSq$es)) {
        ladd(alist) <- list(estimate = etaSq[i, 1], est.ci.lower = etaSq[i, 2], est.ci.upper = etaSq[i, 3])
        ladd(alist) <- list(estimate = etaSqP[i, 1], est.ci.lower = etaSqP[i, 2], est.ci.upper = etaSqP[i, 3])
        ladd(alist) <- list(estimate = omegaSq[i, 1], est.ci.lower = omegaSq[i, 2], est.ci.upper = omegaSq[i, 3])
        ladd(alist) <- list(estimate = omegaSqP[i, 1], est.ci.lower = omegaSqP[i, 2], est.ci.upper = omegaSqP[i, 3])
        ladd(alist) <- list(estimate = epsilonSq[i, 1], est.ci.lower = epsilonSq[i, 2], est.ci.upper = epsilonSq[i, 3])
        ladd(alist) <- list(estimate = epsilonSqP[i, 1], est.ci.lower = epsilonSqP[i, 2], est.ci.upper = epsilonSqP[i, 3])
    }

    return(alist)
}



### ES estimates ###

initialize_effectsize <- function(obj) {
    specs <- switch(obj$options$.caller,
        lm = list(
            list(key = "eta", name = letter_eta2),
            list(key = "etap", name = letter_peta2),
            list(key = "omega", name = letter_omega2),
            list(key = "omegap", name = letter_pomega2),
            list(key = "epsilon", name = letter_epsilon2),
            list(key = "epsilonp", name = letter_pepsilon2)
        ),
        glm = list(
            list(key = "eta", name = letter_eta2),
            list(key = "etap", name = letter_peta2),
            list(key = "epsilon", name = letter_epsilon2),
            list(key = "epsilonp", name = letter_pepsilon2)
        ),
        list()
    )

    specs[vapply(specs, function(spec) spec$key %in% obj$options$es, logical(1))]
}

### these add effect size indices to the anova table depending on the model

add_effect_size <- function(x, ...) UseMethod(".add_es")

.add_es.default <- function(atable, model, obj) {
    return(atable)
}


.add_es.main_anova_lm <- function(atable, model, obj) {
    specs <- initialize_effectsize(obj)
    if (length(specs) == 0 || nrow(atable) == 0) {
        return(atable)
    }

    row_names <- rownames(atable)
    term_rows <- !(row_names %in% c("(Intercept)", "Model", "Residuals", "Total"))
    if (!any(term_rows)) {
        return(atable)
    }

    residual_rows <- row_names == "Residuals"
    model_rows <- row_names == "Model"
    if (any(residual_rows)) {
        ssres <- atable$ss[residual_rows][[1]]
        dfres <- atable$df[residual_rows][[1]]
    } else {
        dfres <- model$df.residual
        ssres <- stats::sigma(model)^2 * dfres
    }

    sumr <- summary(model)
    if (is.null(sumr$fstatistic)) {
        return(atable)
    }

    if (any(model_rows)) {
        modss <- atable$ss[model_rows][[1]]
        mdf <- atable$df[model_rows][[1]]
        f <- atable$f[model_rows][[1]]
        if (length(f) == 0 || is.na(f)) {
            f <- sumr$fstatistic[[1]]
        }
    } else {
        f <- sumr$fstatistic[[1]]
        mdf <- sumr$fstatistic[[2]]
        modss <- f * ssres * mdf / dfres
    }

    N <- dfres + mdf + 1
    SS <- atable$ss[term_rows]
    df <- atable$df[term_rows]
    model_eta <- modss / (modss + ssres)
    model_omega <- pmax(0,
        (modss - ssres * mdf / dfres) /
        (modss + ssres * (dfres + 1) / dfres))
    model_epsilon <- pmax(0,
        (modss - ssres * mdf / dfres) /
        (modss + ssres))

    for (spec in specs) {
        switch(spec$key,
            eta = {
                atable$etaSq <- NA_real_
                atable$etaSq[term_rows] <- SS / (modss + ssres)
                if (any(model_rows)) atable$etaSq[model_rows] <- model_eta
            },
            etap = {
                atable$etaSqP <- NA_real_
                atable$etaSqP[term_rows] <- SS / (SS + ssres)
                if (any(model_rows)) atable$etaSqP[model_rows] <- model_eta
            },
            omega = {
                atable$omegaSq <- NA_real_
                atable$omegaSq[term_rows] <- pmax(0,
                    (SS - ssres * df / dfres) /
                    (modss + ssres * (dfres + 1) / dfres))
                if (any(model_rows)) atable$omegaSq[model_rows] <- model_omega
            },
            omegap = {
                atable$omegaSqP <- NA_real_
                atable$omegaSqP[term_rows] <- pmax(0,
                    (SS - ssres * df / dfres) /
                    (SS + ssres * (N - df) / dfres))
                if (any(model_rows)) atable$omegaSqP[model_rows] <- model_omega
            },
            epsilon = {
                atable$epsilonSq <- NA_real_
                atable$epsilonSq[term_rows] <- pmax(0,
                    (SS - ssres * df / dfres) /
                    (modss + ssres))
                if (any(model_rows)) atable$epsilonSq[model_rows] <- model_epsilon
            },
            epsilonp = {
                atable$epsilonSqP <- NA_real_
                atable$epsilonSqP[term_rows] <- pmax(0,
                    (SS - ssres * df / dfres) /
                    (SS + ssres))
                if (any(model_rows)) atable$epsilonSqP[model_rows] <- model_epsilon
            }
        )
    }

    atable
}



.add_es.main_anova_glm <- function(atable, model, obj) {
  
  specs <- initialize_effectsize(obj)
  if (length(specs) == 0 || nrow(atable) == 0) {
    return(atable)
  }
  
  d0 <- null.deviance(model)
  test <- atable$test
  df <- atable$df
  
  for (spec in specs) {
    switch(spec$key,
           eta = {
             atable$etaSq <- test / d0
           },
           epsilon = {
             atable$epsilonSq <- pmax(0, (test - df) / d0)
           }
    )
  }
  
  atable
}


######## these take care of main_effectsize table #########


fill_effectsize <- function(x, ...) UseMethod(".fill_es")

.fill_es.default <- function(atable, model, obj) {
  return(atable)
}


.fill_es.main_anova_lm <- function(atable, model, obj) {
    atable <- as.data.frame(atable)
    names(atable) <- c("SS", "df", "test", "p")
    atable<-atable[!(rownames(atable) %in% c("Residuals","Total","Model")),]
    effects <- rownames(atable)
    
    df <- atable$df
    dfres <- model$df.residual
    sumr <- summary(model)
    N <- dfres + sumr$fstatistic[[2]] + 1
    ssres <- stats::sigma(model)^2 * dfres
    ssmod <- sumr$fstatistic[[1]] * sumr$fstatistic[[2]] * ssres / dfres
    SS <- df * atable$test * ssres / dfres

    es <- SS / (ssmod + ssres)
    etaSq <- ci_effectsize_lm(es, df, dfres, obj, "eta")

    es <- SS / (SS + ssres)
    etaSqP <- ci_effectsize_lm(es, df, dfres, obj, "etap")

    es <- (SS - (ssres * df / dfres)) / (ssmod + (ssres * (dfres + 1) / dfres))
    omegaSq <- ci_effectsize_lm(es, df, dfres, obj, "omega")

    es <- (SS - (ssres * df / dfres)) / (SS + (ssres * (N - df) / dfres))
    omegaSqP <- ci_effectsize_lm(es, df, dfres, obj, "omegap")

    es <- (SS - (ssres * df / dfres)) / (ssmod + ssres)
    epsilonSq <- ci_effectsize_lm(es, df, dfres, obj, "epsilon")

    es <- (SS - (ssres * df / dfres)) / (SS + ssres)
    epsilonSqP <- ci_effectsize_lm(es, df, dfres, obj, "epsilonp")

    estimates <- list(
        eta = etaSq,
        etap = etaSqP,
        omega = omegaSq,
        omegap = omegaSqP,
        epsilon = epsilonSq,
        epsilonp = epsilonSqP
    )

    alist <- list()
    specs <- initialize_effectsize(obj)
    for (i in seq_along(effects)) {
        for (spec in specs) {
            estimate <- estimates[[spec$key]][i, ]
            ladd(alist) <- list(
                effect = fromb64(effects[i]),
                name = spec$name,
                estimate = estimate[[1]],
                est.ci.lower = estimate[[2]],
                est.ci.upper = estimate[[3]]
            )
        }
    }

    alist
}

.fill_es.main_anova_glm <- function(atable, model, obj) {
  atable <- as.data.frame(atable)
  effects <- rownames(atable)
  specs <- initialize_effectsize(obj)
  
  if (length(specs) == 0 || length(effects) == 0) {
    return(list())
  }
  
  test <- atable$test
  df <- atable$df
  d0 <- null.deviance(model)
  ciwidth <- if (is.null(obj$ciwidth)) obj$options$ci_width / 100 else obj$ciwidth
  
  estimates <- list()
  for (spec in specs) {
    es <- switch(spec$key,
                 eta = test / d0,
                 epsilon = pmax(0, (test - df) / d0),
                 NULL
    )
    
    if (is.null(es)) {
      next
    }
    
    estimates[[spec$key]] <- do.call(rbind, lapply(seq_along(es), function(i) {
      estimate <- ci_effectsize_glm(es[i], df[i], d0, conf.level = ciwidth)
      if (is.null(estimate)) {
        estimate <- data.frame(
          es = es[i],
          es.ci.lower = NA_real_,
          es.ci.upper = NA_real_
        )
      }
      estimate
    }))
  }
  
  alist <- list()
  for (i in seq_along(effects)) {
    for (spec in specs) {
      estimate <- estimates[[spec$key]]
      if (is.null(estimate)) {
        next
      }
      ladd(alist) <- list(
        effect = fromb64(effects[i]),
        name = spec$name,
        estimate = estimate$es[i],
        est.ci.lower = estimate$es.ci.lower[i],
        est.ci.upper = estimate$es.ci.upper[i]
      )
    }
  }
  
  alist
}

### 




### simple effects, maybe useless

.add_es.simple_params_lm <- function(atable, model, variable) {
    xstd <- 1
    if (!is.factor(model$model[, variable])) xstd <- stats::sd(model$model[, variable])
    y <- names(attr(model$terms, "dataClass"))[1]
    ystd <- stats::sd(model$model[, y])
    atable$beta <- atable$estimate * (xstd / ystd)
    atable
}

.add_es.simple_params_glm <- function(atable, model, variable = NULL) {
    atable$expb <- exp(atable$estimate)
    atable$expb.ci.lower <- exp(atable$est.ci.lower)
    atable$expb.ci.upper <- exp(atable$est.ci.upper)
    atable
}

.add_es.simple_params_glmer <- function(atable, model, variable = NULL) {
    .add_es.simple_params_glm(atable, model, variable)
}



.add_es.simple_anova_lm <- function(atable, model) {
    dfres <- model$df.residual
    sumr <- summary(model)
    N <- dfres + sumr$fstatistic[[2]] + 1
    ssres <- stats::sigma(model)^2 * dfres
    ssmod <- sumr$fstatistic[[1]] * sumr$fstatistic[[2]] * ssres / dfres
    df <- atable$df1
    SS <- df * atable$test * ssres / dfres
    atable$etaSq <- SS / (ssmod + ssres)
    atable$etaSqP <- SS / (SS + ssres)
    atable$omegaSq <- (SS - (ssres * df / dfres)) / (ssmod + (ssres * (dfres + 1) / dfres))
    atable$omegaSqP <- (SS - (ssres * df / dfres)) / (SS + (ssres * (N - df) / dfres))
    atable$epsilonSq <- (SS - (ssres * df / dfres)) / (ssmod + ssres)
    atable$epsilonSqP <- (SS - (ssres * df / dfres)) / (SS + ssres)
    as.data.frame(atable)
}





.add_es.simple_anova_glm <- function(atable, model) {
    d0 <- null.deviance(model)
    atable$etaSq <- atable$test / d0
    atable$epsilonSq <- pmax(0, (atable$test - atable$df1) / d0)
    as.data.frame(atable)
}


### confidence intervals for effect size indices

ci_effectsize_lm <- function(es, df, dfres, obj, what = "any") {
    if (is.null(obj$boot_variances)) {
        fs <- .v_to_F(es, df, dfres)
        cilist <- lapply(seq_along(fs), function(i) {
            res <- .get_ncp_F(fs[i], df[i], dfres, conf.level = obj$ciwidth)
            res[is.na(res)] <- 0
            c(es[i], .F_to_v(res, df = df[i], dfres))
        })
        res <- as.data.frame(do.call(rbind, cilist))
        names(res) <- c("es", "es.ci.lower", "es.ci.upper")
        res
    } else {
        terms <- seq_along(es)
        N <- df + dfres + 1
        get_boot_ci(what, terms, obj$boot_variances, type = obj$options$ci_method, width = obj$ciwidth, df = df, dfres = dfres, N = N)
    }
}
.F_to_v <- function(f, df, dfres) {
    (f * df) / (f * df + dfres)
}

.v_to_F <- function(e, df, dfres) pmax(0, (e / df) / ((1 - e) / dfres))

### this is taken from effectsize package. We copied here because effectsize does not expose the function
.get_ncp_F <- function(f, df, df_error, conf.level = 0.90) {
    if (!is.finite(f) || !is.finite(df) || !is.finite(df_error)) {
        return(c(NA, NA))
    }
    alpha <- 1 - conf.level
    probs <- c(alpha / 2, 1 - alpha / 2)
    lambda <- f * df
    ncp <- suppressWarnings(stats::optim(par = 1.1 * rep(
        lambda,
        2
    ), fn = function(x) {
        p <- stats::pf(q = f, df, df_error, ncp = x)
        abs(max(p) - probs[2]) + abs(min(p) - probs[1])
    }, control = list(abstol = 1e-09)))
    f_ncp <- sort(ncp$par)
    if (f <= stats::qf(probs[1], df, df_error)) {
        f_ncp[2] <- 0
    }
    if (f <= stats::qf(probs[2], df, df_error)) {
        f_ncp[1] <- 0
    }
    return(f_ncp)
}


ci_effectsize_glm <- function(eta2, u, D0 , conf.level = 0.95) {
  
  if (!requireNamespace("MBESS", quietly = TRUE)) {
    warning("Package 'MBESS' is required for GzLM C.I. Install it with install.packages('MBESS').")
    return()
  }
  Qx<-eta2*D0
  # Noncentrality-parameter CI via inversion of the noncentral chi-square CDF.
  # MBESS::conf.limits.nc.chisq() clips lambda.L at 0 internally when Qx < u,
  # matching the boundary rule described in the manuscript.
  th_obj <- try_hard(MBESS::conf.limits.nc.chisq(
    Chi.Square = Qx,
    df         = u,
    conf.level = conf.level
  ))
  nc_ci<-th_obj$obj
  
  lambda_L <- nc_ci$Lower.Limit
  lambda_U <- nc_ci$Upper.Limit
  
  # NA handling: conf.limits.nc.chisq() can return NULL/NA at the boundary;
  # treat that as lambda_L = 0.
  if (is.null(lambda_L) || is.na(lambda_L)) lambda_L <- 0
  if (is.null(lambda_U) || is.na(lambda_U)) lambda_U <- 0
  results<-data.frame(es=eta2,es.ci.lower=lambda_L / D0,es.ci.upper = lambda_U / D0)
  results
}

### bootstrap ####

es.var_boot_fun <- function(data, indices, model = NULL) {
    .data <- data[indices, ]
    .model <- stats::update(model, data = .data)
    .anova <- car::Anova(.model, type = "III", singular.ok = T)
    atable <- as.data.frame(.anova[!(rownames(.anova) %in% c("(Intercept)", "Residuals")), ])
    names(atable) <- c("ss", "df", "test", "p")
    dfres <- model$df.residual
    sumr <- summary(model)
    ssres <- stats::sigma(model)^2 * dfres
    ssmod <- sumr$fstatistic[[1]] * sumr$fstatistic[[2]] * ssres / dfres
    ss <- atable$ss
    unlist(c(ss, ssmod, ssres))
}

## computes bootstrap conf int for variances effect size indices
get_boot_ci <- function(effsize, terms, bootresults, type, width, df, dfres, N) {
    type <- switch(type,
        quantile = "perc",
        bcai = "bca"
    )
    fun <- switch(effsize,
        eta = function(ss, ssmod, ssres, df, dfres, N) ss / (ssmod + ssres),
        etap = function(ss, ssmod, ssres, df, dfres, N) ss / (ss + ssres),
        omega = function(ss, ssmod, ssres, df, dfres, N) (ss - (ssres * df / dfres)) / (ssmod + (ssres * (dfres + 1) / dfres)),
        omegap = function(ss, ssmod, ssres, df, dfres, N) (ss - (ssres * df / dfres)) / (ss + (ssres * (N - df) / dfres)),
        epsilon = function(ss, ssmod, ssres, df, dfres, N) (ss - (ssres * df / dfres)) / (ssmod + ssres),
        epsilonp = function(ss, ssmod, ssres, df, dfres, N) (ss - (ssres * df / dfres)) / (ss + ssres),
    )

    sterms <- seq_along(terms)
    l <- length(bootresults$t0)
    ss <- bootresults$t0[sterms]
    ssmod <- bootresults$t0[l - 1]
    ssres <- bootresults$t0[l]
    N <- dim(bootresults$data)[1]
    es <- fun(ss, ssmod, ssres, df, dfres, N)
    bootresults$t0[sterms] <- es

    for (i in 1:nrow(bootresults$t)) {
        ss <- bootresults$t[i, sterms]
        ssmod <- bootresults$t[i, l - 1]
        ssres <- bootresults$t[i, l]
        es <- fun(ss, ssmod, ssres, df, dfres, N)
        bootresults$t[i, sterms] <- es
    }
    alist <- lapply(sterms, function(i) {
        r <- boot::boot.ci(bootresults, type = type, conf = width, index = i)
        c(r$t0, r[[length(r)]][c(4:5)])
    })
    res <- as.data.frame(do.call(rbind, alist))
    names(res) <- c("es", "est.ci.lower", "est.ci.upper")
    res$type <- effsize
    res$effect <- terms
    res
}


#### helper 

## Genuine Type-III likelihood-ratio test for `clm` (ordinal::clm) objects. Neither
## car::Anova() (Anova.clm just relabels the object and delegates to Anova.default, which
## is a Wald test based on vcov()) nor ordinal::clm's own single-model anova() (explicitly
## labeled "Wald chi-square tests" in its own output heading) provide a refit-based
## per-term LR test for this model class -- unlike glm/multinom, whose car::Anova() methods
## do refit reduced models internally. This refits the model once per term (dropping that
## term, holding all others -- i.e. Type III), and compares deviances directly, producing a
## table shaped like car::Anova(type=3, test="Chisq")'s output (Df, Chisq, Pr(>Chisq)) so it
## can be dropped into eta2.default()/eta2_partial.default() via their `anova_table` override.
.clm_anova_lr <- function(object) {
  
  full_terms <- attr(stats::terms(object), "term.labels")
  if (length(full_terms) == 0)
    stop("model has no terms to test")
  
  full_ll <- as.numeric(stats::logLik(object))
  full_df <- length(stats::coef(object))
  
  rows <- lapply(full_terms, function(term) {
    # data=object$model (rather than relying on update()'s default re-evaluation of the
    # original call in parent.frame()) keeps this self-contained: the original `data`
    # argument may reference a variable that isn't in scope wherever this helper is called
    # from, but the fitted object's own stored model frame always is.
    reduced <- stats::update(object, stats::as.formula(paste("~ . -", term)), data = object$model)
    df <- full_df - length(stats::coef(reduced))
    chisq <- 2 * (full_ll - as.numeric(stats::logLik(reduced)))
    c(Df = df, Chisq = chisq)
  })
  
  tab <- as.data.frame(do.call(rbind, rows))
  rownames(tab) <- full_terms
  tab[["Pr(>Chisq)"]] <- stats::pchisq(tab$Chisq, tab$Df, lower.tail = FALSE)
  tab
}
