#' General Linear Model
#'
#' General Linear Model. Estimates models using \code{lm()} function and 
#' provides options to facilitate estimation of 
#' interactions, simple slopes, simple effects, post-hoc tests, contrast 
#' analysis, effect size indexes and visualization of the results.
#' 
#'
#' @examples
#' data('ToothGrowth')
#' gamlj::gamljGlm(formula = len ~ supp,  data = ToothGrowth)
#'
#' @param data the data as a data frame
#' @param caller .
#' @param dep a string naming the dependent variable from \code{data}; the
#'   variable must be numeric. Not needed if \code{formula} is used.
#' @param factors a vector of strings naming the fixed factors from
#'   \code{data}. Not needed if \code{formula} is used.
#' @param covs a vector of strings naming the covariates from \code{data}. Not
#'   needed if \code{formula} is used.
#' @param modelTerms a list of character vectors describing fixed effects
#'   terms. Not needed if \code{formula} is used.
#' @param fixedIntercept \code{TRUE} (default) or \code{FALSE}, estimates
#'   fixed intercept. Not needed if \code{formula} is used.
#' @param showParamsCI \code{TRUE} (default) or \code{FALSE} , parameters CI
#'   in table
#' @param showBetaCI \code{TRUE} (default) or \code{FALSE} , standardized
#'   parameters CI in table
#' @param cimethod .
#' @param bootR a number bootstrap repetitions.
#' @param semethod .
#' @param contrasts a named vector of the form \code{c(var1="type",
#'   var2="type2")} specifying the type of contrast to use, one of
#'   \code{'deviation'}, \code{'simple'}, \code{'dummy'}, \code{'difference'},
#'   \code{'helmert'}, \code{'repeated'} or \code{'polynomial'}. If NULL,
#'   \code{simple} is used. Can also be passed as a list of list of the form
#'   list(list(var="var1",type="type1")).
#' @param showRealNames \code{TRUE} or \code{FALSE} (default), shows raw names
#'   of the contrasts variables
#' @param showContrastCode \code{TRUE} or \code{FALSE} (default), shows
#'   contrast coefficients tables
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
#' @param ciWidth a number between 50 and 99.9 (default: 95) specifying the
#'   confidence interval width for the plots.
#' @param emmeans a rhs formula with the terms specifying the marginal means
#'   to estimate (of the form \code{'~x+x:z'})
#' @param posthoc a rhs formula with the terms specifying the table to apply
#'   the comparisons (of the form \code{'~x+x:z'})
#' @param simpleVariable The variable for which the simple effects (slopes)
#'   are computed
#' @param simpleModerators the variable that provides the levels at which the
#'   simple effects are computed
#' @param simpleInteractions should simple Interactions be computed
#' @param simpleScale \code{'mean_sd'} (default), \code{'custom'} , or
#'   \code{'percent'}. Use to condition the covariates (if any)
#' @param cvalue how many st.deviations around the means used to condition
#'   simple effects and plots. Used if \code{simpleScale}=\code{'mean_sd'}
#' @param percvalue offsett (number of percentiles) around the median used to
#'   condition simple effects and plots. Used if
#'   \code{simpleScale}=\code{'percent'}
#' @param simpleScaleLabels how the levels of a continuous moderator should
#'   appear in tables and plots: \code{labels}, \code{values} and
#'   \code{values_labels}, \code{ovalues}, `ovalues_labels. The latter two refer
#'   to the variable orginal levels, before scaling.
#' @param postHocCorr one or more of \code{'none'},
#'   \code{'bonf'},\code{'tukey'}  \code{'holm'}; provide no,  Bonferroni, Tukey
#'   and Holm Post Hoc corrections respectively.
#' @param posthocEffsize one or more of \code{'dm'},  \code{'ds'},\code{'g'}
#'   for Cohen's d (dm=model SD,ds=sample SD )  or Hedge's g
#' @param dci \code{TRUE} or \code{FALSE} (default), d confidence intervals
#' @param scaling a named vector of the form \code{c(var1='type',
#'   var2='type2')} specifying the transformation to apply to covariates, one of
#'   \code{'centered'} to the mean, \code{'standardized'},\code{'log'} or
#'   \code{'none'}. \code{'none'} leaves the variable as it is.
#' @param modelSelection .
#' @param effectSize a list of effect sizes to print out. They can be:
#'   \code{"eta"} for eta-squared, \code{'partEta'} for partial eta-squared,
#'   \code{'omega'} for partial omega-squared, \code{'epsilon'} for partial
#'   epsilon-squared, and \code{'beta'} for standardized coefficients (betas).
#'   Default is \code{"beta"} and \code{"parEta"}.
#' @param homoTest \code{TRUE} or \code{FALSE} (default), perform homogeneity
#'   tests
#' @param qqplot \code{TRUE} or \code{FALSE} (default), provide a Q-Q plot of
#'   residuals
#' @param normTest \code{TRUE} or \code{FALSE} (default), provide a test for
#'   normality of residuals
#' @param normPlot \code{TRUE} or \code{FALSE} (default), provide a histogram
#'   of residuals superimposed by a normal distribution
#' @param residPlot \code{TRUE} or \code{FALSE} (default), provide a
#'   scatterplot of the residuals against predicted
#' @param interceptInfo \code{TRUE} or \code{FALSE} (default), provide
#'   ìnformation about the intercept (F test, effect size indexes)
#' @param effectSizeInfo \code{TRUE} or \code{FALSE} (default), provide
#'   ìnformation about the effect size indexes
#' @param dep_scale Re-scale the dependent variable.
#' @param formula (optional) the formula to use, see the examples
#' @return A results object containing:
#' \tabular{llllll}{
#'   \code{results$storage} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$info} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$main$r2} \tab \tab \tab \tab \tab a table of R \cr
#'   \code{results$main$intercept} \tab \tab \tab \tab \tab a table of information for the model intercept \cr
#'   \code{results$main$anova} \tab \tab \tab \tab \tab a table of ANOVA results \cr
#'   \code{results$main$effectsizes} \tab \tab \tab \tab \tab a table of effect size indeces \cr
#'   \code{results$main$coefficients} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$main$contrastCodeTables} \tab \tab \tab \tab \tab an array of contrast coefficients tables \cr
#'   \code{results$posthoc} \tab \tab \tab \tab \tab an array of post-hoc tables \cr
#'   \code{results$posthocEffsize} \tab \tab \tab \tab \tab an array of post-hoc effect size \cr
#'   \code{results$simpleEffects$anova} \tab \tab \tab \tab \tab a table of ANOVA for simple effects \cr
#'   \code{results$simpleEffects$coefficients} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$simpleInteractions} \tab \tab \tab \tab \tab an array of simple interactions tables \cr
#'   \code{results$emmeans} \tab \tab \tab \tab \tab an array of predicted means tables \cr
#'   \code{results$mainPlots} \tab \tab \tab \tab \tab an array of results plots \cr
#'   \code{results$plotnotes} \tab \tab \tab \tab \tab a html \cr
#'   \code{results$assumptions$homotest} \tab \tab \tab \tab \tab a table of homogeneity tests \cr
#'   \code{results$assumptions$normtest} \tab \tab \tab \tab \tab a table of normality tests \cr
#'   \code{results$assumptions$qqplot} \tab \tab \tab \tab \tab a q-q plot \cr
#'   \code{results$assumptions$normPlot} \tab \tab \tab \tab \tab Residual histogram \cr
#'   \code{results$assumptions$residPlot} \tab \tab \tab \tab \tab Residual Predicted plot \cr
#'   \code{results$predicted} \tab \tab \tab \tab \tab an output \cr
#'   \code{results$residuals} \tab \tab \tab \tab \tab an output \cr
#' }
#'
#' Tables can be converted to data frames with \code{asDF} or \code{\link{as.data.frame}}. For example:
#'
#' \code{results$storage$asDF}
#'
#' \code{as.data.frame(results$storage)}
#'
#' @export
gamljGlm <- function(
  data,
  caller = "lm",
  dep = NULL,
  factors = NULL,
  covs = NULL,
  modelTerms = NULL,
  fixedIntercept = TRUE,
  showParamsCI = TRUE,
  showBetaCI = TRUE,
  cimethod = "wald",
  bootR = 1000,
  semethod = "standard",
  contrasts = NULL,
  showRealNames = TRUE,
  showContrastCode = FALSE,
  plotHAxis = NULL,
  plotSepLines = NULL,
  plotSepPlots = NULL,
  plotRaw = FALSE,
  plotDvScale = FALSE,
  plotOriginalScale = FALSE,
  plotLinesTypes = FALSE,
  plotError = "none",
  ciWidth = 95,
  emmeans = NULL,
  posthoc = NULL,
  simpleVariable = NULL,
  simpleModerators = NULL,
  simpleInteractions = FALSE,
  simpleScale = "mean_sd",
  cvalue = 1,
  percvalue = 25,
  simpleScaleLabels = "labels",
  postHocCorr = list(
    "bonf"),
  posthocEffsize=list(),
  dci = FALSE,
  scaling = NULL,
  modelSelection = "lm",
  effectSize = list(
    "beta",
    "etap"),
  homoTest = FALSE,
  qqplot = FALSE,
  normTest = FALSE,
  normPlot = FALSE,
  residPlot = FALSE,
  interceptInfo = FALSE,
  effectSizeInfo = FALSE,
  dep_scale = "none",
  formula) {
  
  if ( ! requireNamespace("jmvcore", quietly=TRUE))
    stop("gamljGlm requires jmvcore to be installed (restart may be required)")
  
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
    if (missing(modelTerms))
      modelTerms <- jmvcore::marshalFormula(
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
  if ( ! missing(simpleVariable)) simpleVariable <- jmvcore::resolveQuo(jmvcore::enquo(simpleVariable))
  if ( ! missing(simpleModerators)) simpleModerators <- jmvcore::resolveQuo(jmvcore::enquo(simpleModerators))
  if (missing(data))
    data <- jmvcore::marshalData(
      parent.frame(),
      `if`( ! missing(dep), dep, NULL),
      `if`( ! missing(factors), factors, NULL),
      `if`( ! missing(covs), covs, NULL),
      `if`( ! missing(plotHAxis), plotHAxis, NULL),
      `if`( ! missing(plotSepLines), plotSepLines, NULL),
      `if`( ! missing(plotSepPlots), plotSepPlots, NULL),
      `if`( ! missing(simpleVariable), simpleVariable, NULL),
      `if`( ! missing(simpleModerators), simpleModerators, NULL))
  
  for (v in factors) if (v %in% names(data)) data[[v]] <- as.factor(data[[v]])
  if (inherits(modelTerms, "formula")) modelTerms <- jmvcore::decomposeFormula(modelTerms)
  if (inherits(emmeans, "formula")) emmeans <- jmvcore::decomposeFormula(emmeans)
  if (inherits(posthoc, "formula")) posthoc <- jmvcore::decomposeFormula(posthoc)
  

  options <- gamljGlmOptions$new(
    caller = caller,
    dep = dep,
    factors = factors,
    covs = covs,
    modelTerms = modelTerms,
    fixedIntercept = fixedIntercept,
    showParamsCI = showParamsCI,
    showBetaCI = showBetaCI,
    cimethod = cimethod,
    bootR = bootR,
    semethod = semethod,
    contrasts = contrasts,
    showRealNames = showRealNames,
    showContrastCode = showContrastCode,
    plotHAxis = plotHAxis,
    plotSepLines = plotSepLines,
    plotSepPlots = plotSepPlots,
    plotRaw = plotRaw,
    plotDvScale = plotDvScale,
    plotOriginalScale = plotOriginalScale,
    plotLinesTypes = plotLinesTypes,
    plotError = plotError,
    ciWidth = ciWidth,
    emmeans = emmeans,
    posthoc = posthoc,
    simpleVariable = simpleVariable,
    simpleModerators = simpleModerators,
    simpleInteractions = simpleInteractions,
    simpleScale = simpleScale,
    cvalue = cvalue,
    percvalue = percvalue,
    simpleScaleLabels = simpleScaleLabels,
    postHocCorr = postHocCorr,
    posthocEffsize = posthocEffsize,
    dci = dci,
    scaling = scaling,
    modelSelection = modelSelection,
    effectSize = effectSize,
    homoTest = homoTest,
    qqplot = qqplot,
    normTest = normTest,
    normPlot = normPlot,
    residPlot = residPlot,
    interceptInfo = interceptInfo,
    effectSizeInfo = effectSizeInfo,
    dep_scale = dep_scale)
  
  analysis <- gamljGlmClass$new(
    options = options,
    data = data)
  
  analysis$run()
  
  analysis$results
}

