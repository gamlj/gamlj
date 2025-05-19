#' Update a GAMLj model
#'
#' Re-estimates a GAMLj model applying new options to the original model
#'
#' @param  object of class `gamlj*Results`
#' @param  ... any parameter to be passed to \code{\link[GAMLj3]{gamlj_lm}},  \code{\link[GAMLj3]{gamlj_mixed}},  \code{\link[GAMLj3]{gamlj_glm}}, or  \code{\link{gamlj_gmixed}}
#' @return an object of class gamlj*Results as the input object
#' @author Marcello Gallucci
#' @rdname update
#' @export


update.gamljlmResults <- function(object, ...) {
    fun <- gamlj_lm
    .gamlj_update(fun, object, ...)
}

#' @rdname update
#' @export

update.gamljglmResults <- function(object, ...) {
    fun <- gamlj_glm
    .gamlj_update(fun, object, ...)
}

#' @rdname update
#' @export

update.gamljmixedResults <- function(object, ...) {
    fun <- gamlj_mixed
    .gamlj_update(fun, object, ...)
}

#' @rdname update
#' @export

update.gamljgmixedResults <- function(object, ...) {
    fun <- gamlj_gmixed
    .gamlj_update(fun, object, ...)
}

.gamlj_update <- function(fun, object, ...) {
    params <- list(...)
    if (is.null(names(params))) {
        params <- params[[1]]
    }

    forms <- formals(fun)
    alist <- list()
    for (f in names(forms)) {
        if (f %in% names(object$options)) {
            alist[[f]] <- object$options[[f]]
        }
    }
    for (p in names(params)) {
        alist[[p]] <- params[[p]]
    }
    data <- object$options$.getData()
    alist[["data"]] <- data
    do.call(fun, alist)
}

#'  GAMLj plot
#'
#' This function re-estimates a GAMLj model adding a new plot. If no option is passed, extracts the
#' plots present in the `gamlj*Results` object. If one plot is present, it is returned as a ggplot2 object,
#'  if more than one is present, a list of plots is returned. FALSE is returned if no plot is present or defined.

#' @param x a gamlj results object of the class `gamlj`
#' @param formula a right hand side formula specifying the effect to plot, of the form `~x`, `~x*z` or `~x*z*w`.
#' It has prevalence on other options defining a plot.
#' @param ... all options accepted by a gamlj model function.
#'            Relevant for new plots are \code{plot_x}, \code{plot_z} and \code{plot_by}
#' @return an object of class ggplot or a list of ggplot objects
#' @author Marcello Gallucci
#' @examples
#' data(qsport)
#' gmod <- GAMLj3::gamlj_lm(
#'     formula = performance ~ hours,
#'     data = qsport
#' )
#'
#' plot(gmod, plot_x = "hours")
#' plot(gmod, formula = ~hours)
#' @rdname plot
#' @export

plot.gamlj <- function(x, formula = NULL, ...) {
    if (is.something(formula)) {
        .calls <- jmvcore::marshalFormula(formula, from = "rhs", type = "terms", data = NULL)
        plots <- lapply(.calls, function(.call) {
            haxis <- .call[1]
            if (!is.na(.call[2])) {
                sepLines <- .call[2]
            } else {
                sepLines <- NULL
            }
            if (!is.na(.call[3])) {
                sepPlots <- .call[3]
            } else {
                sepPlots <- NULL
            }
            args <- list(plot_x = haxis, plot_z = sepLines, plot_by = sepPlots, ...)
            object <- stats::update(x, args)
            .extract_plots(object, "mainPlots")
        })
    } else {
        if (is.something(list(...))) {
            x <- stats::update(x, ...)
        }

        plots <- .extract_plots(x, "mainPlots")
    }
    if ("list" %in% class(plots) && length(plots) == 1) {
        plots <- plots[[1]]
    }
    plots
}


#'  GAMLj Johnson-Nayman plots
#'
#' This function re-estimates a GAMLj model adding a Johnson-Nayman plot, if not available already. If no option is passed, extracts the
#' plots present in the `gamlj*Results` object. If JN plot is present, it is returned as a ggplot2 object,
#'  if more than one is present, a list of plots is returned. FALSE is returned if no plot is present or defined.

#' @param x a gamlj results object of the class `gamlj`
#' @param formula a right hand side formula specifying the effect to plot, of the form `~x*z` or `~x*z*w`.
#' It has prevalence on other options defining a plot.
#' @param ... all options accepted by a gamlj model function.
#'            Relevant for new plots are \code{plot_x}, \code{plot_z} and \code{plot_by}
#' @return an object of class ggplot or a list of ggplot objects
#' @author Marcello Gallucci
#' @examples
#' data(qsport)
#' gmod <- GAMLj3::gamlj_lm(
#'     formula = performance ~ hours,
#'     data = qsport
#' )
#'
#' plot(gmod, plot_x = "hours")
#' plot(gmod, formula = ~hours)
#' @rdname jnplot
#' @export

jnplot <- function(x, formula = NULL, ...) {
    if (is.something(formula)) {
        .calls <- jmvcore::marshalFormula(formula, from = "rhs", type = "terms", data = NULL)
        plots <- lapply(.calls, function(.call) {
            haxis <- .call[1]
            if (!is.na(.call[2])) {
                sepLines <- .call[2]
            } else {
                sepLines <- NULL
            }
            if (!is.na(.call[3])) {
                sepPlots <- .call[3]
            } else {
                sepPlots <- NULL
            }
            args <- list(plot_x = haxis, plot_z = sepLines, plot_by = sepPlots, plot_jn = TRUE, ...)
            object <- stats::update(x, args)
            .extract_plots(object, "jnPlots")
        })
    } else {
        if (is.something(list(...))) {
            x <- stats::update(x, ...)
        }

        plots <- .extract_plots(x, "jnPlots")
    }
    if ("list" %in% class(plots) && length(plots) == 1) {
        plots <- plots[[1]]
    }
    plots
}


.extract_plots <- function(object, what) {
    alist <- list()

    if (!utils::hasName(object, what)) {
        warning("The requested plots are not available. You can ask them with the passing the gamlj options to the funcion")
        return(FALSE)
    }
    if (length(object[[what]]) == 0) {
        warning("The requested plots are not available. You can ask them with the passing the gamlj options to the funcion")
        return(FALSE)
    }

    object<-object[[what]]
    if (what=="mainPlots") object<-object[[1]]
    
    for (i in 1:length(object)) {
        title <- (object[[i]]$title)
        gplot <- object[[i]]$plot$fun() + ggplot2::ggtitle(title)
        alist[[i]] <- gplot
    }
    return(alist)
}

#' Assumptions checking plots
#'
#' This function returns a list of plots as a ggplot objects produced by the assumptions checking options, such
#' as `qq_plot`, `resid_plot` and `norm_plot` for continuous dependent variable models,
#' `cluster_boxplot`, `cluster_respred` and `rand_hist` for mixed models.
#' @param object a gamlj results object of the class `gamlj*Results``
#' @return a list of lists with titles and ggplot objects
#' @author Marcello Gallucci
#' @export

assumptions <- function(object) {
    if (!("assumptions" %in% names(object))) {
        stop("No assumptions checking plot is contained in the GAMLj model")
    }

    pnames <- names(object$assumptions)
    if (!is.something(pnames)) {
        stop("No assumptions checking plots is contained in the GAMLj model")
    }
    res <- list()
    for (pname in pnames) {
        obj <- object$assumptions[[pname]]
        if ("Image" %in% class(obj)) {
            plot <- obj$plot$fun()
            if (is.something(plot)) {
                title <- obj$title
                name <- obj$name
                plot <- obj$plot$fun()
                res[[length(res) + 1]] <- list(name = name, title = title, plot = plot)
            }
        }
        if ("Array" %in% class(obj)) {
            groupname <- obj$name
            j <- 0
            for (key in obj$itemKeys) {
                j <- j + 1
                one <- obj$get(key = key)
                title <- one$title
                name <- paste0(groupname, j)
                plot <- one$plot$fun()
                res[[length(res) + 1]] <- list(name = name, title = title, plot = plot)
            }
        }
    }

    res
}


#' Coefficients covariances for a GAMLj model
#'
#' Returns the parameters variances and covariances
#'
#' @param  object of class `gamlj*Results`
#' @param  ... any parameter to be passed to \code{\link[GAMLj3]{gamlj_lm}},  \code{\link[GAMLj3]{gamlj_mixed}},  \code{\link[GAMLj3]{gamlj_glm}}, or  \code{\link[GAMLj3]{gamlj_gmixed}}
#' @return an table of class gamlj*Results
#' @author Marcello Gallucci
#' @export

vcov.gamlj <- function(object, ...) {
    if (utils::hasName(object, "main") && utils::hasName(object$main, "vcov")) {
        return(object$main$vcov)
    }
    mod <- stats::update(object, vcov = T, ...)
    mod$main$vcov
}


#' Extract data
#'
#' This function returns a dataset with the variables in the GAMLj model
#' transformed according to GAMLj options. It is usefull to run additional
#' models with other R packages with the same setup used by GAMLj
#'
#' @name   get_data
#' @param object a gamlj results object of the class `gamlj*Results`
#' @param ... additional arguments passed to the GAMLj estimation function
#' @aliases get_data.gamlj
#' @return a dataset
#' @author Marcello Gallucci
#' @examples
#' data("qsport")
#' gmod <- GAMLj3::gamlj_lm(
#'     formula = performance ~ hours,
#'     data = qsport,
#'     covs_scale = c(hours = "standardized")
#' )
#'
#' gdata <- get_data(gmod)
#' lm(performance ~ hours, data = gdata)
#' @export

get_data <- function(object, ...) UseMethod("get_data")

#' @export
get_data.gamlj <- function(object, ...) {
    if (isS4(object$model)) {
        .data <- object$model@frame
    } else {
        .data <- object$model$model
    }

    .names <- names(.data)
    good <- grep("I(", .names, fixed = T, invert = T)
    .data <- .data[, good]
    .names <- fromb64(.names[good])
    names(.data) <- .names
    .factors <- .names[sapply(.data, is.factor)]
    for (f in .factors) {
        levels(.data[[f]]) <- fromb64(levels(.data[[f]]))
        colnames(stats::contrasts(.data[[f]])) <- gsub(FACTOR_SYMBOL, "", colnames(stats::contrasts(.data[[f]])))
    }
    .data
}

#' Predicted values from GAMLj models
#'
#' Returns predicted values from the estimated model
#'
#' @name predict
#' @rdname predict
#' @aliases predict.gamlj_lm_Results
#' @param object a gamlj results object of the class `gamlj*Results`
#' @param re.form (formula, NULL, or NA) specify which random effects to condition on when predicting. If NULL, include all random effects; if NA or ~0, include no random effects. Used only for the mixed models.
#' @param type the type of prediction required. The default is on the scale of the response variables ('response'); Thus for  binomial models the default is to compute the predicted probabilities.  'link' gives the scale of the linear predictors;
#'             is on the scale of the linear predictors;  The 'terms' option returns a matrix giving the fitted values of each term in the model formula on the linear predictor scale.
#'             Cf. \code{\link[stats:predict]{stats::predict()}}, \code{\link[stats:predict.lm]{stats::predict.lm()}}
#' @param ...  additional arguments for specific predict methods other than the ones specified here.
#' @return a R object of the class of the estimated model
#' @author Marcello Gallucci
#' @examples
#' data("qsport")
#' gmod <- GAMLj3::gamlj_lm(
#'     formula = performance ~ hours,
#'     data = qsport
#' )
#' preds <- predict(gmod)
#'
#' @export

predict.gamljlmResults <- function(object, ...) {
    stats::predict(object$model, ...)
}


#'  preds<-predict(obj)
#' @rdname predict
#' @export

predict.gamljglmResults <- function(object, type = "response", ...) {
    stats::predict(object$model, type = type, ...)
}

#' @rdname predict
#' @export


predict.gamljmixedResults <- function(object, re.form = NULL, type = "response", ...) {
    stats::predict(object$model, re.form = re.form, type = type, ...)
}

#' @rdname predict
#' @export

predict.gamljgmixedResults <- function(object, re.form = NULL, type = "response", ...) {
    stats::predict(object$model, re.form = re.form, type = type, ...)
}


#' Residuals values from GAMLj models
#'
#' Returns residuals values from the estimated model
#'
#' @name residuals
#' @rdname residuals
#' @aliases residuals.gamlj_lm_Results
#' @param object a gamlj results object of the class `gamlj*Results`
#' @param type the type of residuals for generalized models. The alternatives are: 'deviance' (default), 'pearson', 'working', 'response', and 'partial'. Can be abbreviated.
#'             Cf. \code{\link[stats:residuals]{stats::residuals()}}, \code{\link[stats:residuals.lm]{stats::residuals.lm()}}, \code{\link[stats:residuals.glm]{stats::residuals.glm()}}
#' @param ...  additional arguments for specific residuals methods.
#' @return a R object of the class of the estimated model
#' @author Marcello Gallucci
#' @examples
#' data("qsport")
#' gmod <- GAMLj3::gamlj_lm(
#'     formula = performance ~ hours,
#'     data = qsport
#' )
#' preds <- residuals(gmod)
#'
#' @export

residuals.gamljlmResults <- function(object, ...) {
    stats::residuals(object$model, ...)
}

#' @rdname residuals
#' @export

residuals.gamljglmResults <- function(object, type = "deviance", ...) {
    stats::residuals(object$model, type = type, ...)
}

#' @rdname residuals
#' @export


residuals.gamljmixedResults <- function(object, ...) {
    stats::residuals(object$model, ...)
}

#' @rdname residuals
#' @export

residuals.gamljgmixedResults <- function(object, type = "deviance", ...) {
    stats::residuals(object$model, type = type, ...)
}


#'  anova tests in GAMLj results
#'
#' This is a convenience function to extract the ANOVA table (omnibus tests) from a GAMLj model. If no option is passed, extracts the
#' ANOVA tests table already in the model results (if any). If two GAMLj models are provided, a model comparison is produces. Any option
#' accepted by gamlj model can be passed.
#' are returned.

#' @param object a gamlj results object of the class `gamlj`
#' @param object2 a gamlj results object of the class `gamlj` representing the nested model. Overiddes \code{nested_terms}.
#'                 It can be passed also as a a right-hand side formula specifying terms of the nested model.
#' @param ... all options accepted by a gamlj model function.
#' @return a list of tables of class `ResultsElement`
#' @author Marcello Gallucci
#' @examples
#' data(fivegroups)
#' fivegroups$Group <- factor(fivegroups$Group)
#' gmod <- GAMLj3::gamlj_lm(
#'     formula = Score ~ Group,
#'     data = fivegroups
#' )
#'
#' anova(gmod)
#' @rdname anova
#' @export


anova.gamlj <- function(object, object2, ...) {
    args <- list(...)
    if (!missing(object2)) {
        if (inherits(object2, "gamlj")) {
            args$nested_terms <- fromb64(stats::formula(object2$model))
        }
        object <- stats::update(object, args)
        res <- object$main$r2
        if (utils::hasName(object$main, "fit")) {
            res <- c(object$main$r2, object$main$fit)
        }
    } else {
        res <- object$main$r2
        if (utils::hasName(object$main, "fit")) {
            res <- c(object$main$r2, object$main$fit)
        }
        res <- c(res, object$main$anova)
    }
    res
}


#'  Post-hoc test on GAMLj results
#'
#' This is a convenience function to re-estimates a GAMLj model adding posthoc tests. If no option is passed, extracts the
#' post-hoc tests tables already in the model results (if any). If new post-hoc are defined, the post-hoc tests tables
#' are returned.

#' @param object a gamlj results object of the class `gamlj`
#' @param formula a right hand side formula specifying the factors or factors combinations to test, of the form `~x+z`, `~x:z` or `~x*z`.
#' It has prevalence on other options defining a post-hoc test via character options.
#' @param ... all options accepted by a gamlj model function. Relevant for new tests are
#'   `post_hoc` (a list of list of terms), `adjust`, a list of correction to apply:
#'    one or more of \code{none}, \code{bonf},  \code{holm}, \code{scheffe} or \code{sidak}.
#' @return a list of tables of class `ResultsElement`
#' @author Marcello Gallucci
#' @examples
#' data(fivegroups)
#' fivegroups$Group <- factor(fivegroups$Group)
#' gmod <- GAMLj3::gamlj_lm(
#'     formula = Score ~ Group,
#'     data = fivegroups
#' )
#'
#' posthoc(gmod, formula = ~Group)
#' @rdname posthoc
#' @export

posthoc <- function(object, ...) UseMethod("posthoc")

#' @rdname posthoc
#' @export

posthoc.gamlj <- function(object, formula = NULL, ...) {
    if (is.something(formula)) {
        object <- stats::update(object, posthoc = formula, ...)
    } else if (is.something(list(...))) {
        object <- stats::update(object, ...)
    }
    if (length(object$posthoc) == 0) {
        return(FALSE)
    }

    object$posthoc
}




#'  Simple Effects on GAMLj results
#'
#' This is a convenience function to re-estimates a GAMLj model adding simple effect analysis. If no option is passed, extracts the
#' simple effects tables already in the model results (if any). If new tests are defined, the simple effects tests tables
#' are returned.

#' @param object a gamlj results object of the class `gamlj`
#' @param formula a right hand side formula specifying the variables to test, of the form `~x:z`, `~x:z:w` or `~x*z`.
#' The formula is not expanded, so the first variable is the simple effect variable, the second is the moderator,
#' the third an optional additional moderator, an so on.
#' It has prevalence on other options defining a simple effects test via character options.
#' @param ... all options accepted by a gamlj model function. Relevant for new tests are
#'   `simple_x` (the simple effect variable), \code{`simple_mods`}, the moderator(s). Both are overriden by the formula option.
#' @return a list of tables of class `ResultsElement`
#' @author Marcello Gallucci
#' @examples
#' data(wicksell)
#' wicksell$time <- factor(wicksell$time)
#' wicksell$group <- factor(wicksell$group)
#' wicksell$subj <- factor(wicksell$subj)

#' gmod<-GAMLj3::gamlj_mixed(
#'    formula = dv ~ 1 +group+ time:group+ time+( 1 | subj ),
#'    data = wicksell)
#'
#' simple_effects(gmod,formula =~time:group)
#' @rdname simple_effects
#' @export

simple_effects <- function(object, ...) UseMethod("simple_effects")


#' @rdname simple_effects
#' @export

simple_effects.gamlj <- function(object, formula = NULL, ...) {
    args <- list(...)
    if (is.something(formula)) {
        v <- all.vars(formula)
        if (length(v) < 2) stop("Specify a simple effect formula with at least two variables in the model.")
        args$simple_x <- v[[1]]
        args$simple_mods <- v[-1]
    }
    if (is.something(args)) {
        object <- stats::update(object, args)
    }

    if (dim(object$simpleEffects$anova$asDF)[1] != 0) {
        return(object$simpleEffects)
    } else {
        stop("Simple effects not available")
    }
}


#'  S3 methods for class jamovi ResultsElement
#'
#' These functions extract all visible tables from a ResultsElement or related classes produced by GAMLj3
#' and print them in R style.

#' @param object a gamlj results object of the class `gamlj`
#' @param formula not used
#' @param ... additional arguments passed to the GAMLj3 estimation function
#' @return a list of table as data.frame
#' @author Marcello Gallucci
#' @examples
#' data(fivegroups)
#' fivegroups$Group <- factor(fivegroups$Group)
#' gmod <- GAMLj3::gamlj_lm(
#'     formula = Score ~ Group,
#'     data = fivegroups
#' )
#'
#' summary(gmod)
#' @rdname s3methods
#'
#' @export

summary.ResultsElement <- function(object, ...) {
    .get_table <- function(obj) {
        if ("Table" %in% class(obj) && obj$visible) {
            if (nrow(obj$asDF) > 0) {
                atab <- obj$asDF
                attr(atab, "name") <- obj$name
                attr(atab, "title") <- obj$title
                class(atab) <- c("jmvrtable", "data.frame")
                tables[[length(tables) + 1]] <<- atab
            }
            return()
        }
        if ("Html" %in% class(obj) && obj$visible) {
            tables[[length(tables) + 1]] <<- obj$content
        }

        if (obj$.has("items")) {
            for (item in obj$items) {
                .get_table(item)
            }
        }
    }

    tables <- list()
    .get_table(object)
    class(tables) <- c("jmvrobj", "list")
    tables
}


#'  S3 methods for class galmj_list
#'
#' These functions extract all visible tables from a list of tables produced by GAMLj3
#' and print them in R style.

#' @param object a gamlj results object of the class `gamlj`
#' @param ... additional arguments passed to the GAMLj3 estimation function
#' @return a list of tables as data.frame
#' @author Marcello Gallucci
#' @examples
#' data(fivegroups)
#' fivegroups$Group <- factor(fivegroups$Group)
#' gmod <- GAMLj3::gamlj_lm(
#'     formula = Score ~ Group,
#'     data = fivegroups
#' )
#'
#' summary(gmod)
#' @rdname s3methods
#'
#' @export

summary.gamlj_list <- function(object, ...) {
    lapply(object, function(x) summary.ResultsElement(x))
}


#' Print a jamovi Table in R style
#'
#' @param x a gamlj results object of the class `gamlj`
#' @param ... options passed to print.data.frame()

#' @rdname s3methods
#'
#' @export

print.jmvrtable <- function(x, ...) {
    cat(attr(x, "title"), "\n\n")
    for (var in names(x)) {
        if (is.character(x[[var]])) {
            x[is.na(x[[var]]), var] <- ""
        }
    }
    print.data.frame(x)
}

#' Print a jamovi summary of jamovi ResultElement in R style
#'
#' @param x a gamlj results object of the class `gamlj`
#' @param ... options passed to print()
#' @rdname s3methods
#' @export

print.jmvrobj <- function(x, ...) {
    for (t in x) {
        print(t)
        cat("\n\n")
    }
}

#' Print a jamovi summary of jamovi ResultElement in R style
#'
#' @param x a gamlj results object of the class `gamlj`
#' @param ... options passed to print()
#' @rdname s3methods
#' @export

print.gamlj_list <- function(x, ...) {
    .names <- names(x)
    for (i in seq_along(x)) {
        name <- .names[i]
        cat("Table for ", name, "\n")
        print(x[[i]])
        cat("\n\n")
    }
}

#' Extract coefficients from a GAMLj result object
#'
#' @param object a gamlj results object of the class `gamlj`
#' @param ... not used
#' @rdname s3methods
#'
#' @export

coef.gamlj <- function(object, ...) {
    return(object$main$coefficients)
}

#' Extract model R2 (r-squared) from a GAMLj results object
#'
#' @param x a gamlj results object of the class `gamlj`
#' @param ... not used
#' @rdname s3methods
#' @export
#'
fit <- function(x, ...) UseMethod("fit")

#' @rdname s3methods
#' @export

fit.gamlj <- function(x, ...) {
    res <- x$main$r2
    if (utils::hasName(x$main, "fit")) {
        res <- c(x$main$r2, x$main$fit)
    }

    return(res)
}


#'  Estimated marginal means in GAMLj results
#'
#' This is a convenience function to extract the estimated marginal means table from a GAMLj model. If no option is passed, extracts the
#' emmeans table already in the model results (if any). If a formula is passed, the estimated marginal means for the defined
#' factors are displayed.

#' @param object a gamlj results object of the class `gamlj`
#' @param formula (optional) rhs formula defining the factor(s) for which levels expected means are estimated
#' @param ... all options accepted by a gamlj model function.
#' @return an object of class
#' @author Marcello Gallucci
#' @examples
#' data(fivegroups)
#' fivegroups$Group <- factor(fivegroups$Group)
#' gmod <- GAMLj3::gamlj_lm(
#'     formula = Score ~ Group,
#'     data = fivegroups
#' )
#'
#' em_means(gmod, formula = ~Group)
#' @export
#'
em_means <- function(object, formula, ...) UseMethod("em_means")


#' @rdname s3methods
#' @export

em_means.gamlj <- function(object, formula = NULL, ...) {
    if (is.something(formula)) {
        object <- stats::update(object, emmeans = formula, ...)
    } else if (is.something(list(...))) {
        object <- stats::update(object, ...)
    }
    if (length(object$emmeans) == 0) {
        return(FALSE)
    }
    object$emmeans
}


#'  Custom contrasts
#'
#' This is a convenience function to test and extract estimates and information about
#' custom contrast.

#' @param object a gamlj results object of the class `gamlj`
#' @param contrasts a named list with contrasts weights to test of the form \code{list(factor1=c(a1,a2,b3), factor2=c(b1,b2,b3))}. Multiple
#'                  contrasts for the same factor can be tested. In this case, each contrast is tested independently.
#' @param ... all options accepted by a gamlj model function.
#' @return an object of class
#' @author Marcello Gallucci
#' @examples
#' data(fivegroups)
#' fivegroups$Group <- factor(fivegroups$Group)
#' gmod <- GAMLj3::gamlj_lm(
#'     formula = Score ~ Group,
#'     data = fivegroups
#' )
#'
#' em_means(gmod, formula = ~Group)
#' @export
#'


test_contrasts <- function(object, contrasts, ...) {
    results <- list()
    for (i in seq_along(contrasts)) {
        name <- names(contrasts)[i]
        w <- contrasts[[i]]

        values <- list(w)
        names(values) <- name
        settings <- list("custom")
        names(settings) <- name
        model <- stats::update(object, contrasts = settings, contrast_custom_values = values)
        ladd(results) <- model$main$contrasts
    }
    names(results) <- names(contrasts)
    class(results) <- c("gamlj_list", class(results))
    results
}



#' Alternative spelling of GAMLj main functions
#'
#' An alternative spelling of the command \code{\link{gamlj_lm}}

#' @param ... the same arguments that can be passed to the corresponding function
#' @return a GAMLj results object equivalent to the results of (\code{\link{gamlj_lm}})
#' @author Marcello Gallucci
#' @rdname alternatives
#'
#' @export

gamljlm <- function(...) GAMLj3::gamlj_lm(...)

#' Alternative spelling of GAMLj main functions
#'
#' An alternative spelling of the command \code{\link{gamlj_glm}}

#' @param ... the same arguments that can be passed to the corresponding function
#' @return a GAMLj results object equivalent to the results of (\code{\link{gamlj_glm}})
#' @author Marcello Gallucci
#' @rdname alternatives
#'
#' @export

gamljglm <- function(...) GAMLj3::gamlj_glm(...)

#' Alternative spelling of GAMLj main functions
#'
#' An alternative spelling of the command \code{\link{gamlj_mixed}}

#' @param ... the same arguments that can be passed to the corresponding function
#' @return a GAMLj results object equivalent to the results of (\code{\link{gamlj_mixed}})
#' @author Marcello Gallucci
#' @rdname alternatives
#'
#' @export

gamljmixed <- function(...) GAMLj3::gamlj_mixed(...)

#' Alternative spelling of GAMLj main functions
#'
#' An alternative spelling of the command \code{\link{gamlj_gmixed}}

#' @param ... the same arguments that can be passed to the corresponding function
#' @return a GAMLj results object equivalent to the results of (\code{\link{gamlj_gmixed}})
#' @author Marcello Gallucci
#' @rdname alternatives
#'
#' @export

gamljgmixed <- function(...) GAMLj3::gamlj_gmixed(...)
