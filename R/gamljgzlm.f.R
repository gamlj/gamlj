#' Generalized Linear Models
#'
#' Generalized Linear Model
#'
#' @examples
#' data<-emmeans::neuralgia
#'  gamlj::gamljGzlm(
#'            formula = Pain ~ Duration,
#'            data = data,
#'             model_type = "logistic")
#'
#' @param data the data as a data frame
#' @param dep a string naming the dependent variable from \code{data}; the
#'   variable must be numeric. Not needed if \code{formula} is used.
#' @param factors a vector of strings naming the fixed factors from
#'   \code{data}. Not needed if \code{formula} is used.
#' @param covs a vector of strings naming the covariates from \code{data}. Not
#'   needed if \code{formula} is used.
#' @param model_terms a list of character vectors describing fixed effects
#'   terms. Not needed if \code{formula} is used.
#' @param fixed_intercept \code{TRUE} (default) or \code{FALSE}, estimates
#'   fixed intercept. Not needed if \code{formula} is used.
#' @param nested_intercept \code{TRUE} (default) or \code{FALSE}, estimates
#'   fixed intercept. Not needed if \code{formula} is used.
#' @param nested_terms a list of character vectors describing effects terms
#'   for nestet. It can be passed as right-hand formula.
#' @param omnibus Pseudo R-squared omnibus tests are based on  
#'                loglikelihood ratio test \code{'LRT'}  (default) or Wald's Chi-square \code{'wald'}. 
#' @param estimates_ci \code{TRUE} (default) or \code{FALSE} , coefficients CI
#'   in tables
#' @param ci_method .
#' @param boot_r a number bootstrap repetitions.
#' @param ci_width a number between 50 and 99.9 (default: 95) specifying the
#'   confidence interval width for the plots.
#' @param contrasts a named vector of the form \code{c(var1="type",
#'   var2="type2")} specifying the type of contrast to use, one of
#'   \code{'deviation'}, \code{'simple'}, \code{'dummy'}, \code{'difference'},
#'   \code{'helmert'}, \code{'repeated'} or \code{'polynomial'}. If NULL,
#'   \code{simple} is used. Can also be passed as a list of list of the form
#'   list(list(var="var1",type="type1")).
#' @param show_contrastnames \code{TRUE} or \code{FALSE} (default), shows raw
#'   names of the contrasts variables in tables
#' @param show_contrastcodes \code{TRUE} or \code{FALSE} (default), shows
#'   contrast coefficients tables
#' @param vcov \code{TRUE} or \code{FALSE} (default), shows coefficients
#'   covariances
#' @param plotHAxis a string naming the variable placed on the horizontal axis
#'   of the plot
#' @param plotSepLines a string naming the variable represented as separate
#'   lines in the plot
#' @param plotSepPlots a list of string naming the variables defining the
#'   levels for multiple plots
#' @param plotRaw \code{TRUE} or \code{FALSE} (default), plot raw data along
#'   the predicted values
#' @param plotDvScale \code{TRUE} or \code{FALSE} (default), set the Y-axis
#'   range equal to the range of the observed values.
#' @param plotOriginalScale \code{TRUE} or \code{FALSE} (default), use
#'   original scale for covariates.
#' @param plotLinesTypes \code{TRUE} or \code{FALSE} (default), use different
#'   linetypes per levels.
#' @param plotError \code{'none'} (default), \code{'ci'}, or \code{'se'}. Use
#'   no error bars, use confidence intervals, or use standard errors on the
#'   plots, respectively.
#' @param emmeans a rhs formula with the terms specifying the marginal means
#'   to estimate (of the form \code{'~x+x:z'})
#' @param posthoc a rhs formula with the terms specifying the table to apply
#'   the comparisons (of the form \code{'~x+x:z'}). The formula is not expanded,
#'   so '\code{x*z}' becomes '\code{x+z' and not '}x+z+x:z\code{'. It can be
#'   passed also as a list of the form '}list("x","z",c("x","z")`'
#' @param simple_effects The variable for which the simple effects (slopes)
#'   are computed
#' @param simple_moderators the variable that provides the levels at which the
#'   simple effects are computed
#' @param simple_effects The variable for which the simple effects (slopes)
#'   are computed
#' @param simple_moderators the variable that provides the levels at which the
#'   simple effects are computed
#' @param simple_interactions should simple Interactions be computed
#' @param covs_scale a named vector of the form \code{c(var1='type',
#'   var2='type2')} specifying the transformation to apply to covariates, one of
#'   \code{'centered'} to the mean, \code{'standardized'} or  \code{'none'}.
#'   \code{'none'} leaves the variable as it is.
#' @param covs_conditioning \code{'mean_sd'} (default), or
#'   \code{'percent'}. How to condition covariates in simple effects and plots.
#'   \code{'mean_sd'} for mean +/- \code{ccp_value} * sd. Default is 1.
#'   \code{'percent'} for median +/- \code{ccm_value} for percentiles. Default is 25. 
#' @param ccm_value how many std around the means to condition
#'   simple effects and plots. Used if \code{covs_conditioning}=\code{'mean_sd'}
#' @param ccp_value offsett (number of percentiles) around the median used to
#'   condition simple effects and plots. Used if
#'   \code{simpleScale}=\code{'percent'}

#' @param covs_scale_labels how the levels of a continuous moderator should
#'   appear in tables and plots: \code{'labels'}, \code{'values'} and
#'   \code{'values_labels'}, \code{'ovalues'}, \code{'ovalues_labels'}. The latter two refer
#'   to the variable orginal levels, before scaling.
#' @param adjust one or more of \code{'none'},  \code{'bonf'},\code{'tukey'}
#'   \code{'holm'}, \code{'scheffe'}; provide no,  Bonferroni, Tukey, Holm, Scheffe Post Hoc corrections
#'   respectively.
#' @param covs_scale a named vector of the form \code{c(var1='type',
#'   var2='type2')} specifying the transformation to apply to covariates, one of
#'   \code{'centered'} to the mean, \code{'standardized'},\code{'log'} or
#'   \code{'none'}. \code{'none'} leaves the variable as it is.
#' @param expb_ci \code{TRUE} (default) or \code{FALSE} , exp(B) CI in table
#' @param es .
#' @param model_type Select the generalized linear model:
#'   \code{linear},\code{poisson},\code{logistic},\code{multinomial}
#' @param custom_family Distribution family for the custom model, accepts
#'   gaussian, binomial, gamma and inverse_gaussian .
#' @param custom_link Distribution family for the custom model, accepts
#'   identity, log and inverse, onemu2 (for 1/mu^2).
#' @param propodds_test Test parallel lines assumptions in cumulative link
#'   model (ordinal regression)
#' @param formula (optional) the formula to use, see the examples
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
gamljGzlm <- function(
  formula,
  data,
  dep = NULL,
  factors = NULL,
  covs = NULL,
  model_terms = NULL,
  nested_terms = NULL,
  fixed_intercept = TRUE,
  nested_intercept = TRUE,
  omnibus = "LRT",
  estimates_ci = TRUE,
  expb_ci = TRUE,
  es = list("expb"),
  model_type = "linear",
  custom_family = "gaussian",
  custom_link = "identity",
  propodds_test = FALSE,
  ci_width = 95,
  ci_method = "wald",
  boot_r = 1000,
  contrasts = NULL,
  show_contrastnames = TRUE,
  show_contrastcodes = FALSE,
  covs_scale = NULL, 
  covs_conditioning = "mean_sd",
  ccm_value = 1,
  ccp_value = 25,
  covs_scale_labels = "labels",
  vcov = FALSE,
  emmeans = NULL,
  posthoc = NULL,
  adjust = list("bonf"),
  simple_effects = NULL,
  simple_moderators = NULL,
  simple_interactions = FALSE,
  plotHAxis = NULL,
  plotSepLines = NULL,
  plotSepPlots = NULL,
  plotRaw = FALSE,
  plotDvScale = FALSE,
  plotOriginalScale = FALSE,
  plotLinesTypes = FALSE,
  plotError = "none"
  
  ) {
  
  if ( ! requireNamespace("jmvcore", quietly=TRUE))
    stop("gamljGzlm requires jmvcore to be installed (restart may be required)")
  
  
  ### options not needed or fixed in R
  .caller       = "glm"
  .interface    = "R"
  donotrun      = FALSE
  comparison    = FALSE

  if (is.something(nested_terms))
    comparison = TRUE
  
  
  
  if ( ! missing(formula)) {
    if (missing(dep))
      dep <- jmvcore::marshalFormula(
        formula=formula,
        data=`if`( ! missing(data), data, NULL),
        from="lhs",
        subset="1",
        required=TRUE)
    if (missing(factors))
      factors <- jmvcore::marshalFormula(
        formula=formula,
        data=`if`( ! missing(data), data, NULL),
        from="rhs",
        type="vars",
        permitted="factor")
    if (missing(covs))
      covs <- jmvcore::marshalFormula(
        formula=formula,
        data=`if`( ! missing(data), data, NULL),
        from="rhs",
        type="vars",
        permitted="numeric")
    if (missing(model_terms))
      model_terms <- jmvcore::marshalFormula(
        formula=formula,
        data=`if`( ! missing(data), data, NULL),
        from="rhs",
        type="terms")
  }
  
  if ( ! missing(dep)) dep <- jmvcore::resolveQuo(jmvcore::enquo(dep))
  if ( ! missing(factors)) factors <- jmvcore::resolveQuo(jmvcore::enquo(factors))
  if ( ! missing(covs)) covs <- jmvcore::resolveQuo(jmvcore::enquo(covs))
  if ( ! missing(plotHAxis)) plotHAxis <- jmvcore::resolveQuo(jmvcore::enquo(plotHAxis))
  if ( ! missing(plotSepLines)) plotSepLines <- jmvcore::resolveQuo(jmvcore::enquo(plotSepLines))
  if ( ! missing(plotSepPlots)) plotSepPlots <- jmvcore::resolveQuo(jmvcore::enquo(plotSepPlots))
  if ( ! missing(simple_effects)) simple_effects <- jmvcore::resolveQuo(jmvcore::enquo(simple_effects))
  if ( ! missing(simple_moderators)) simple_moderators <- jmvcore::resolveQuo(jmvcore::enquo(simple_moderators))
  if (missing(data))
    data <- jmvcore::marshalData(
      parent.frame(),
      `if`( ! missing(dep), dep, NULL),
      `if`( ! missing(factors), factors, NULL),
      `if`( ! missing(covs), covs, NULL),
      `if`( ! missing(plotHAxis), plotHAxis, NULL),
      `if`( ! missing(plotSepLines), plotSepLines, NULL),
      `if`( ! missing(plotSepPlots), plotSepPlots, NULL),
      `if`( ! missing(simple_effects), simple_effects, NULL),
      `if`( ! missing(simple_moderators), simple_moderators, NULL))
  
  for (v in factors) if (v %in% names(data)) data[[v]] <- as.factor(data[[v]])
  if (inherits(model_terms, "formula")) model_terms <- jmvcore::decomposeFormula(model_terms)
  if (inherits(nested_terms, "formula")) nested_terms <- jmvcore::decomposeFormula(nested_terms)
  if (inherits(emmeans, "formula")) emmeans <- jmvcore::decomposeFormula(emmeans)
  if (inherits(posthoc, "formula")) posthoc <- jmvcore::decomposeFormula(posthoc)
  
  ## this allows passing the model terms as formula
  if (inherits(model_terms, "formula")) model_terms <- jmvcore::decomposeFormula(model_terms)
  
  ## if no formula nor model_terms are passed, model_terms are all main effects
  if (is.null(model_terms)) model_terms<-as.list(c(factors,covs))
  
  if (!missing(formula) & fixed_intercept==TRUE) {
    fixed_intercept=as.logical(attr(terms(formula),"intercept"))
  }
  
  
  ### fix some options when passed by R ####
  if (is.something(names(covs_scale))) 
    scaling<-lapply(names(covs_scale), function(a) list(var=a,type=covs_scale[[a]]))
  
  if (is.something(names(contrasts))) 
    contrasts<-lapply(names(contrasts), function(a) list(var=a,type=contrasts[[a]]))
  
  
  
  
  
  options <- gamljGzlmOptions$new(
    .caller = .caller,
    .interface = .interface,
    dep = dep,
    factors = factors,
    covs = covs,
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
    plotHAxis = plotHAxis,
    plotSepLines = plotSepLines,
    plotSepPlots = plotSepPlots,
    plotRaw = plotRaw,
    plotDvScale = plotDvScale,
    plotOriginalScale = plotOriginalScale,
    plotLinesTypes = plotLinesTypes,
    plotError = plotError,
    emmeans = emmeans,
    posthoc = posthoc,
    simple_effects = simple_effects,
    simple_moderators = simple_moderators,
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
    propodds_test = propodds_test)
  
  analysis <- gamljGzlmClass$new(
    options = options,
    data = data)
  
  analysis$run()
  
  analysis$results
}

