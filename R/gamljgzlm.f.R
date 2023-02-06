#' Generalized Linear Models
#'
#' Generalized Linear Model with options to estimate logistic, probit, 
#' ordinal, multinomial and custum link function models. Provides options
#' to estimate posthoc, simple effects and slopes, plots of main effects and
#' interaction. Model comparison is also an option.
#' 
#' @examples
#'    data <- emmeans::neuralgia
#'    gamlj::gamljGzlm(formula = Pain ~ Duration,
#'                     data = data,
#'                     model_type = "logistic"
#'                     )
#'      
#' @param formula (optional) the formula of the model compatible with \link[stats]{glm}. If not passed,
#'                 model terms should be defined as a list in  \code{model_terms} option.
#' @param data the data as a data.frame
#' @param dep a string naming the dependent variable from \code{data}; the
#'            variable must be numeric. Not needed if \code{formula} is used.
#' @param model_type define the type of model (link function and distribution combination) required.
#'                   It can be \code{linear}, \code{poisson}, \code{poiover}, \code{nb},
#'                   \code{logistic}, \code{probit}, \code{custom}, \code{ordinal}, \code{multinomial}
#' @param custom_family Distribution family for the custom model, accepts
#'                        \code{gaussian}, \code{binomial}, \code{gamma} and \code{inverse_gaussian}.
#' @param custom_link Distribution family for the \code{model_type="custom"}, accepts
#'   \code{identity}, \code{log} and \code{inverse}. Use \code{onemu2}  for 1/mu^2).
#' @param factors a vector of strings naming the fixed factors from
#'   \code{data}. Not needed if \code{formula} is used.
#' @param covs a vector of strings naming the covariates from \code{data}. Not
#'   needed if \code{formula} is used.
#' @param offset a vector of strings naming the offset variables.
#' @param model_terms a list of character vectors describing fixed effects
#'   terms. Not needed if \code{formula} is used.
#' @param fixed_intercept \code{TRUE} (default) or \code{FALSE}, estimates
#'   fixed intercept. Not needed if \code{formula} is used.
#' @param nested_intercept \code{TRUE} (default) or \code{FALSE}, estimates
#'   fixed intercept. Not needed if \code{formula} is used.
#' @param nested_terms a list of character vectors describing effects terms
#'   for nested model. It can be passed as right-hand formula.
#' @param comparison Not present in R
#' @param omnibus Whether the omnibus test for the model should be \code{wald} or \code{LRT}.
#' @param estimates_ci \code{TRUE} (default) or \code{FALSE} , coefficients CI
#'   in tables
#' @param ci_method .
#' @param boot_r a number bootstrap repetitions.
#' @param ci_width a number between 50 and 99.9 (default: 95) specifying the
#'   confidence interval width for the plots.
#' @param contrasts a named vector of the form \code{c(var1="type",
#'   var2="type2")} specifying the type of contrast to use, one of
#'   \code{deviation}, \code{simple}, \code{dummy}, \code{difference},
#'   \code{helmert}, \code{repeated} or \code{'polynomial'}. If NULL,
#'   \code{simple} is used. Can also be passed as a list of list of the form
#'   list(list(var="var1",type="type1")).
#' @param show_contrastnames \code{TRUE} or \code{FALSE} (default), shows raw
#'   names of the contrasts variables in tables
#' @param show_contrastcodes \code{TRUE} or \code{FALSE} (default), shows
#'   contrast coefficients tables
#' @param vcov \code{TRUE} or \code{FALSE} (default), shows coefficients
#'   covariances
#' @param plot_x a string naming the variable placed on the horizontal axis of
#'   the plot
#' @param plot_z a string naming the variable represented as separate lines in
#'   the plot
#' @param plot_by a list of variables defining the levels at which a separate
#'   plot should be produced.
#' @param plot_raw \code{TRUE} or \code{FALSE} (default), plot raw data along
#'   the predicted values
#' @param plot_yscale \code{TRUE} or \code{FALSE} (default), set the Y-axis
#'   range equal to the range of the observed values.
#' @param plot_xoriginal \code{TRUE} or \code{FALSE} (default), use original
#'   scale for covariates.
#' @param plot_black \code{TRUE} or \code{FALSE} (default), use different
#'   linetypes per levels.
#' @param plot_around \code{'none'} (default), \code{'ci'}, or \code{'se'}.
#'   Use no error bars, use confidence intervals, or use standard errors on the
#'   plots, respectively.
#' @param emmeans a rhs formula with the terms specifying the marginal means
#'   to estimate (of the form \code{'~x+x:z'})
#' @param posthoc a rhs formula with the terms specifying the table to apply
#'   the comparisons (of the form \code{'~x+x:z'}). The formula is not expanded,
#'   so \code{x*z} becomes \code{x+z} and not \code{x+z+x:z}. It can be
#'   passed also as a list of the form \code{'list("x","z",c("x","z")'}
#' @param simple_x The variable for which the simple effects (slopes)
#'   are computed
#' @param simple_mods the variable that provides the levels at which the
#'   simple effects are computed
#' @param simple_interactions should simple Interactions be computed
#' @param covs_conditioning \code{'mean_sd'} (default), \code{'custom'} , or
#'   \code{'percent'}. Use to condition the covariates (if any)
#' @param ccm_value how many st.deviations around the means used to condition
#'   simple effects and plots. Used if \code{simpleScale}=\code{'mean_sd'}
#' @param ccp_value offsett (number of percentiles) around the median used to
#'   condition simple effects and plots. Used if
#'   \code{simpleScale}=\code{'percent'}
#' @param covs_scale_labels how the levels of a continuous moderator should
#'   appear in tables and plots: \code{labels}, \code{values} and
#'   \code{values_labels}, \code{ovalues}, \code{ovalues_labels}. The latter two refer
#'   to the variable original levels, before scaling.
#' @param adjust one or more of \code{'none'},  \code{'bonf'},\code{'tukey'}
#'   \code{'holm'}; provide no,  Bonferroni, Tukey and Holm Post Hoc corrections
#'   respectively.
#' @param covs_scale a named vector of the form \code{c(var1='type',
#'   var2='type2')} specifying the transformation to apply to covariates, one of
#'   \code{'centered'} to the mean, \code{'standardized'},\code{'log'} or
#'   \code{'none'}. \code{'none'} leaves the variable as it is.
#' @param expb_ci \code{TRUE} (default) or \code{FALSE} , exp(B) CI in table
#' @param es Effect size indices. \code{expb} (default) exponentiates the
#'   coefficients. For dichotomous dependent variables relative risk indices
#'   (RR) can be obtained. \code{marginals} computes the marginal effects.
#' @param propodds_test Test parallel lines assumptions in cumulative link
#'   model (ordinal regression)
#' @param plot_scale Chi-squared computation method. \code{'lrt'} (default)
#'   gives LogLikelihood ration test,  \code{'wald'} gives the Wald Chi-squared.
#' @return A results object containing:
#' \tabular{llllll}{
#'   \code{results$model} \tab \tab \tab \tab \tab The underlying estimated model \cr
#'   \code{results$info} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$main$r2} \tab \tab \tab \tab \tab a table of R \cr
#'   \code{results$main$fit} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$main$anova} \tab \tab \tab \tab \tab a table of ANOVA results \cr
#'   \code{results$main$coefficients} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$main$vcov} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$main$contrastCodeTables} \tab \tab \tab \tab \tab an array of contrast coefficients tables \cr
#'   \code{results$main$marginals} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$main$relativerisk} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$main$paralleltest} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$posthoc} \tab \tab \tab \tab \tab an array of post-hoc tables \cr
#'   \code{results$simpleEffects$anova} \tab \tab \tab \tab \tab a table of ANOVA for simple effects \cr
#'   \code{results$simpleEffects$coefficients} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$simpleInteractions} \tab \tab \tab \tab \tab an array of simple interactions tables \cr
#'   \code{results$emmeans} \tab \tab \tab \tab \tab an array of predicted means tables \cr
#'   \code{results$mainPlots} \tab \tab \tab \tab \tab an array of results plots \cr
#'   \code{results$plotnotes} \tab \tab \tab \tab \tab a html \cr
#'   \code{results$predicted} \tab \tab \tab \tab \tab an output \cr
#'   \code{results$residuals} \tab \tab \tab \tab \tab an output \cr
#' }
#'
#' Tables can be converted to data frames with \code{asDF} or \code{\link{as.data.frame}}. For example:
#'
#' \code{results$info$asDF}
#'
#' \code{as.data.frame(results$info)}
#'
#' @export
#' 
gamljGzlm <- function(
    formula = NULL,
    data,
    model_type = "linear",
    dep = NULL,
    factors = NULL,
    covs = NULL,
    offset = NULL,
    model_terms = NULL,
    fixed_intercept = TRUE,
    nested_intercept = NULL,
    custom_family = "gaussian",
    custom_link = "identity",
    nested_terms = NULL,
    comparison = FALSE,
    omnibus = "LRT",
    estimates_ci = FALSE,
    donotrun = FALSE,
    ci_method = "wald",
    boot_r = 1000,
    ci_width = 95,
    contrasts = NULL,
    show_contrastnames = TRUE,
    show_contrastcodes = FALSE,
    vcov = FALSE,
    plot_x = NULL,
    plot_z = NULL,
    plot_by = NULL,
    plot_raw = FALSE,
    plot_yscale = FALSE,
    plot_xoriginal = FALSE,
    plot_black = FALSE,
    plot_around = "ci",
    emmeans = NULL,
    posthoc = NULL,
    simple_x = NULL,
    simple_mods = NULL,
    simple_interactions = FALSE,
    covs_conditioning = "mean_sd",
    ccm_value = 1,
    ccp_value = 25,
    covs_scale_labels = "labels",
    adjust = list(
      "bonf"
    ),
    covs_scale = NULL,
    expb_ci = TRUE,
    es = list("expb"),
    propodds_test = FALSE,
    plot_scale = "response") {
  if (!requireNamespace("jmvcore", quietly = TRUE)) {
    stop("gamljGzlm requires jmvcore to be installed (restart may be required)")
  }

  if (!missing(dep)) dep <- jmvcore::resolveQuo(jmvcore::enquo(dep))
  if (!missing(factors)) factors <- jmvcore::resolveQuo(jmvcore::enquo(factors))
  if (!missing(covs)) covs <- jmvcore::resolveQuo(jmvcore::enquo(covs))
  if (!missing(offset)) offset <- jmvcore::resolveQuo(jmvcore::enquo(offset))
  if (!missing(plot_x)) plot_x <- jmvcore::resolveQuo(jmvcore::enquo(plot_x))
  if (!missing(plot_z)) plot_z <- jmvcore::resolveQuo(jmvcore::enquo(plot_z))
  if (!missing(plot_by)) plot_by <- jmvcore::resolveQuo(jmvcore::enquo(plot_by))
  if (!missing(simple_x)) simple_x <- jmvcore::resolveQuo(jmvcore::enquo(simple_x))
  if (!missing(simple_mods)) simple_mods <- jmvcore::resolveQuo(jmvcore::enquo(simple_mods))
  if (missing(data)) {
    data <- jmvcore::marshalData(
      parent.frame(),
      `if`(!missing(dep), dep, NULL),
      `if`(!missing(factors), factors, NULL),
      `if`(!missing(covs), covs, NULL),
      `if`(!missing(offset), offset, NULL),
      `if`(!missing(plot_x), plot_x, NULL),
      `if`(!missing(plot_z), plot_z, NULL),
      `if`(!missing(plot_by), plot_by, NULL),
      `if`(!missing(simple_x), simple_x, NULL),
      `if`(!missing(simple_mods), simple_mods, NULL)
    )
  }

  for (v in factors) if (v %in% names(data)) data[[v]] <- as.factor(data[[v]])
  ##### custom code
  .caller <- "glm"
  .interface <- "R"
  donotrun <- FALSE
  ### model terms
  if (inherits(model_terms, "formula")) {
    f <- rFormula$new(model_terms, data)
    model_terms <- f$terms
    if (missing(fixed_intercept)) {
      fixed_intercept <- f$intercept
    }
    if (missing(factors)) {
      factors <- f$factors
    }
    if (missing(covs)) {
      factors <- f$covs
    }
  }
    
  if (!is.null(formula)) {
    f <- rFormula$new(formula, data)
    dep <- f$dep
    factors <- f$factors
    covs <- f$covs
    fixed_intercept <- f$intercept
    model_terms <- f$terms
  }
  # if no formula or no terms is passed, covs and factors are the terms
  if (is.null(model_terms)) model_terms <- as.list(c(factors, covs))

  # nested terms
  comparison <- FALSE

  if (inherits(nested_terms, "formula")) {
    f <- rFormula$new(nested_terms)
    nested_intercept <- f$intercept
    nested_terms <- f$terms
  }

  if (is.something(nested_terms) | !is.null(nested_intercept)) {
    comparison <- TRUE
  }

  ### other from formula to list
  if (inherits(emmeans, "formula")) emmeans <- jmvcore::decomposeFormula(emmeans)
  if (inherits(posthoc, "formula")) posthoc <- jmvcore::decomposeFormula(posthoc)

  ### fix some options when passed by R ####
  if (is.something(names(covs_scale))) {
    scaling <- lapply(names(covs_scale), function(a) list(var = a, type = covs_scale[[a]]))
  }

  if (is.something(names(contrasts))) {
    contrasts <- lapply(names(contrasts), function(a) list(var = a, type = contrasts[[a]]))
  }

  ## end of custom code

  options <- gamljGzlmOptions$new(
    .caller = .caller,
    .interface = .interface,
    dep = dep,
    factors = factors,
    covs = covs,
    offset = offset,
    model_terms = model_terms,
    fixed_intercept = fixed_intercept,
    nested_intercept = nested_intercept,
    nested_terms = nested_terms,
    comparison = comparison,
    omnibus = omnibus,
    estimates_ci = estimates_ci,
    donotrun = donotrun,
    ci_method = ci_method,
    boot_r = boot_r,
    ci_width = ci_width,
    contrasts = contrasts,
    show_contrastnames = show_contrastnames,
    show_contrastcodes = show_contrastcodes,
    vcov = vcov,
    plot_x = plot_x,
    plot_z = plot_z,
    plot_by = plot_by,
    plot_raw = plot_raw,
    plot_yscale = plot_yscale,
    plot_xoriginal = plot_xoriginal,
    plot_black = plot_black,
    plot_around = plot_around,
    emmeans = emmeans,
    posthoc = posthoc,
    simple_x = simple_x,
    simple_mods = simple_mods,
    simple_interactions = simple_interactions,
    covs_conditioning = covs_conditioning,
    ccm_value = ccm_value,
    ccp_value = ccp_value,
    covs_scale_labels = covs_scale_labels,
    adjust = adjust,
    covs_scale = covs_scale,
    expb_ci = expb_ci,
    es = es,
    model_type = model_type,
    custom_family = custom_family,
    custom_link = custom_link,
    propodds_test = propodds_test,
    plot_scale = plot_scale
  )

  analysis <- gamljGzlmClass$new(
    options = options,
    data = data
  )

  analysis$run()

  analysis$results
}
