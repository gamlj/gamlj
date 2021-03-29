#' General Linear Model
#' 
#' 
#'  General Linear Model. Estimates models using `lm()` function and provides options to facilitate estimation of 
#'  interactions, simple slopes, simple effects, post-hoc tests, contrast analysis, effect size indexes and visualization of the results.
#'
#' @examples
#' data('ToothGrowth')
#' gamlj::gamljGlm(formula = len ~ supp,  data = ToothGrowth)
#'
#' @param data the data as a data frame
#' @param formula (optional) an object of class \code{\link[stats]{formula}} (or one that can be coerced to that class): a symbolic description of the model to be fitted. 
#' @param dep a string naming the dependent variable from \code{data}; the
#'   variable must be numeric. No needed if \code{formula} is used.
#' @param factors a vector of strings naming the fixed factors from
#'   \code{data}. No needed if \code{formula} is used.
#' @param covs a vector of strings naming the covariates from \code{data}. No
#'   needed if \code{formula} is used.
#' @param modelTerms a list of character vectors describing fixed effects
#'   terms. No needed if \code{formula} is used.
#' @param fixedIntercept \code{TRUE} (default) or \code{FALSE}, estimates
#'   fixed intercept. No needed if \code{formula} is used.
#' @param showParamsCI \code{TRUE} (default) or \code{FALSE} , parameters CI
#'   in table
#' @param paramCIWidth a number between 50 and 99.9 (default: 95) specifying
#'   the confidence interval width for the parameter estimates
#' @param contrasts   a named vector of the form \code{c(var1='type', var2='type2')} specifying the type of contrast to use,
#'    one of \code{'deviation'}, \code{'simple'}, \code{'dummy'}, \code{'difference'}, \code{'helmert'}, \code{'repeated'} or \code{'polynomial'}.
#'     If NULL, \code{'simple'} is used. Can also be passed as a list of list of the form \code{list(list(var='var1',type='type1'))}.

#' @param showRealNames \code{TRUE} or \code{FALSE} (default), shows raw names
#'   of the contrasts variables
#' @param showContrastCode \code{TRUE} or \code{FALSE} (default), shows
#'   contrast coefficients tables
#' @param plotHAxis a string naming the variable placed on the horizontal axis
#'   of the plot
#' @param plotSepLines a string naming the variable represented as separate
#'   lines in the plot
#' @param plotSepPlots a string naming the variable defining the levels for
#'   multiple plots
#' @param plotRaw \code{TRUE} or \code{FALSE} (default), plot raw data along
#'   the predicted values
#' @param plotDvScale \code{TRUE} or \code{FALSE} (default), set the Y-axis
#'   range equal to the range of the observed values.
#' @param plotError \code{'none'} (default), \code{'ci'}, or \code{'se'}. Use
#'   no error bars, use confidence intervals, or use standard errors on the
#'   plots, respectively.
#' @param ciWidth a number between 50 and 99.9 (default: 95) specifying the
#'   confidence interval width for the plots.
#' @param postHoc a rhs formula with the terms specifying the table to apply the comparisons (of the form \code{'~x+x:z'}
#' @param eDesc \code{TRUE} or \code{FALSE} (default), provide lsmeans
#'   statistics
#' @param eCovs \code{TRUE} or \code{FALSE} (default), provide lsmeans
#'   statistics conditioned to  different values of the continuous variables in
#'   the model. Which levels of the continuous variable should be used is set by
#'   the \code{simpleScale} option.
#' @param simpleVariable The variable for which the simple effects (slopes)
#'   are computed
#' @param simpleModerator the variable that provides the levels at which the
#'   simple effects are computed
#' @param simple3way a moderator of the two-way interaction which is probed
#' @param simpleScale \code{'mean_sd'} (default), \code{'custom'} , or
#'   \code{'percent'}. Use to condition the covariates (if any)
#' @param cvalue how many st.deviations around the means used to condition
#'   simple effects and plots. Used if \code{simpleScale}=\code{'mean_sd'}
#' @param percvalue offsett (number of percentiles) around the median used to
#'   condition simple effects and plots. Used if
#'   \code{simpleScale}=\code{'percent'}
#' @param simpleScaleLabels how the levels of a continuous moderator should
#'   appear in tables and plots: \code{labels}, \code{values} and
#'   \code{values_labels}.
#' @param postHocCorr one or more of \code{'none'},  \code{'bonf'}, or
#'   \code{'holm'}; provide no,  Bonferroni, and Holm Post Hoc corrections
#'   respectively.
#' @param scaling a named vector of the form \code{c(var1='type1',var2='type2')}. Types are
#'   \code{'centered'} to the mean, \code{'standardized'}, log-transformed \code{'log'} or \code{'none'}.
#'   \code{'none'} leaves the variable as it is. It can also be passed as a list of lists.
#' @param effectSize a list of effect sizes to print out. They can be:
#'   \code{'eta'} for eta-squared, \code{'partEta'} for partial eta-squared,
#'   \code{'omega'} for partial omega-squared, \code{'epsilon'} for partial epsilon-squared, and \code{'beta'} for standardized
#'   coefficients (betas). Default is \code{beta} and \code{partEta}.
#' @param homoTest \code{TRUE} or \code{FALSE} (default), perform homogeneity
#'   tests
#' @param qq \code{TRUE} or \code{FALSE} (default), provide a Q-Q plot of
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
#' @return A results object containing:
#' \tabular{llllll}{
#'   \code{results$model} \tab \tab \tab \tab \tab The underlying \code{lm} object \cr
#'   \code{results$info} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$main$interceptTable} \tab \tab \tab \tab \tab a table of information for the model intercept \cr
#'   \code{results$main$anova} \tab \tab \tab \tab \tab a table of ANOVA results \cr
#'   \code{results$main$effectSizeTable} \tab \tab \tab \tab \tab a table of effect size indeces \cr
#'   \code{results$main$fixed} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$main$contrastCodeTables} \tab \tab \tab \tab \tab an array of contrast coefficients tables \cr
#'   \code{results$postHocs} \tab \tab \tab \tab \tab an array of post-hoc tables \cr
#'   \code{results$simpleEffects$Anova} \tab \tab \tab \tab \tab a table of ANOVA for simple effects \cr
#'   \code{results$simpleEffects$Params} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$emeansTables} \tab \tab \tab \tab \tab an array of predicted means tables \cr
#'   \code{results$descPlot} \tab \tab \tab \tab \tab a descriptives plot \cr
#'   \code{results$descPlots} \tab \tab \tab \tab \tab an array of results plots \cr
#'   \code{results$assumptions$homoTest} \tab \tab \tab \tab \tab a table of homogeneity tests \cr
#'   \code{results$assumptions$normTest} \tab \tab \tab \tab \tab a table of normality tests \cr
#'   \code{results$assumptions$qq} \tab \tab \tab \tab \tab a q-q plot \cr
#'   \code{results$assumptions$normPlot} \tab \tab \tab \tab \tab Residual histogram \cr
#'   \code{results$assumptions$residPlot} \tab \tab \tab \tab \tab Residual Predicted plot \cr
#' }
#'
#' Tables can be converted to data frames with \code{asDF} or \code{\link{as.data.frame}}. For example:
#'
#' \code{results$info$asDF}
#'
#' \code{as.data.frame(results$info)}
#'
#' @export
gamljGlm <- function(
  data,
  formula,
  dep = NULL,
  factors = NULL,
  covs = NULL,
  modelTerms = NULL,
  fixedIntercept = TRUE,
  showParamsCI = TRUE,
  paramCIWidth = 95,
  contrasts = NULL,
  showRealNames = TRUE,
  showContrastCode = FALSE,
  plotHAxis = NULL,
  plotSepLines = NULL,
  plotSepPlots = NULL,
  plotRaw = FALSE,
  plotDvScale = FALSE,
  plotError = "none",
  ciWidth = 95,
  postHoc = NULL,
  eDesc = FALSE,
  eCovs = FALSE,
  simpleVariable = NULL,
  simpleModerator = NULL,
  simple3way = NULL,
  simpleScale = "mean_sd",
  cvalue = 1,
  percvalue = 25,
  simpleScaleLabels = "labels",
  postHocCorr = list(
    "bonf"),
  scaling = NULL,
  effectSize = list(
    "beta",
    "partEta"),
  homoTest = FALSE,
  qq = FALSE,
  normTest = FALSE,
  normPlot = FALSE,
  residPlot = FALSE,
  interceptInfo = FALSE,
  effectSizeInfo = FALSE,
  dep_scale = "none"
  ) {
  
  if ( ! requireNamespace('jmvcore'))
    stop('gamljGlm requires jmvcore to be installed (restart may be required)')
  
  if ( ! missing(formula)) {
    if (missing(dep))
      dep <- jmvcore::marshalFormula(
        formula=formula,
        data=`if`( ! missing(data), data, NULL),
        from='lhs',
        subset='1',
        required=TRUE)
    if (missing(factors))
      factors <- jmvcore::marshalFormula(
        formula=formula,
        data=`if`( ! missing(data), data, NULL),
        from='rhs',
        type='vars',
        permitted='factor')
    if (missing(covs))
      covs <- jmvcore::marshalFormula(
        formula=formula,
        data=`if`( ! missing(data), data, NULL),
        from='rhs',
        type='vars',
        permitted='numeric')
    if (missing(modelTerms))
      modelTerms <- jmvcore::marshalFormula(
        formula=formula,
        data=`if`( ! missing(data), data, NULL),
        from='rhs',
        type='terms')
  }
  
  if ( ! missing(dep)) dep <- jmvcore::resolveQuo(jmvcore::enquo(dep))
  if ( ! missing(factors)) factors <- jmvcore::resolveQuo(jmvcore::enquo(factors))
  if ( ! missing(covs)) covs <- jmvcore::resolveQuo(jmvcore::enquo(covs))
  if ( ! missing(plotHAxis)) plotHAxis <- jmvcore::resolveQuo(jmvcore::enquo(plotHAxis))
  if ( ! missing(plotSepLines)) plotSepLines <- jmvcore::resolveQuo(jmvcore::enquo(plotSepLines))
  if ( ! missing(plotSepPlots)) plotSepPlots <- jmvcore::resolveQuo(jmvcore::enquo(plotSepPlots))
  if ( ! missing(simpleVariable)) simpleVariable <- jmvcore::resolveQuo(jmvcore::enquo(simpleVariable))
  if ( ! missing(simpleModerator)) simpleModerator <- jmvcore::resolveQuo(jmvcore::enquo(simpleModerator))
  if ( ! missing(simple3way)) simple3way <- jmvcore::resolveQuo(jmvcore::enquo(simple3way))
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
      `if`( ! missing(simpleModerator), simpleModerator, NULL),
      `if`( ! missing(simple3way), simple3way, NULL))
  
  for (v in factors) if (v %in% names(data)) data[[v]] <- as.factor(data[[v]])
  if (inherits(modelTerms, 'formula')) modelTerms <- jmvcore::decomposeFormula(modelTerms)
  if (inherits(postHoc, 'formula')) postHoc <- jmvcore::decomposeFormula(postHoc)

  ### fix some options when passed by R ####
  if (is.something(names(scaling))) 
     scaling<-lapply(names(scaling), function(a) list(var=a,type=scaling[[a]]))
  
  if (is.something(names(contrasts))) 
    contrasts<-lapply(names(contrasts), function(a) list(var=a,type=contrasts[[a]]))
    options <- gamljGlmOptions$new(
    dep = dep,
    factors = factors,
    covs = covs,
    modelTerms = modelTerms,
    fixedIntercept = fixedIntercept,
    showParamsCI = showParamsCI,
    paramCIWidth = paramCIWidth,
    contrasts = contrasts,
    showRealNames = showRealNames,
    showContrastCode = showContrastCode,
    plotHAxis = plotHAxis,
    plotSepLines = plotSepLines,
    plotSepPlots = plotSepPlots,
    plotRaw = plotRaw,
    plotDvScale = plotDvScale,
    plotError = plotError,
    ciWidth = ciWidth,
    postHoc = postHoc,
    eDesc = eDesc,
    eCovs = eCovs,
    simpleVariable = simpleVariable,
    simpleModerator = simpleModerator,
    simple3way = simple3way,
    simpleScale = simpleScale,
    cvalue = cvalue,
    percvalue = percvalue,
    simpleScaleLabels = simpleScaleLabels,
    postHocCorr = postHocCorr,
    scaling = scaling,
    effectSize = effectSize,
    homoTest = homoTest,
    qq = qq,
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
