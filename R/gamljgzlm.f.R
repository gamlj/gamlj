#' Generalized Linear Models
#'
#' Generalized Linear Model
#'
#' @examples
#' data<-emmeans::neuralgia
#'  gamlj::gamljGzlm(
#'            formula = Pain ~ Duration,
#'            data = data,
#'             modelSelection = "logistic")
#'
#' @param data the data as a data frame
#' @param dep a string naming the dependent variable from \code{data},
#'   variable can be numeric or factor
#' @param factors a vector of strings naming the fixed factors from
#'   \code{data}
#' @param covs a vector of strings naming the covariates from \code{data}
#' @param modelTerms a list of character vectors describing fixed effects
#'   terms
#' @param fixedIntercept \code{TRUE} (default) or \code{FALSE}, estimates
#'   fixed intercept
#' @param showParamsCI \code{TRUE}  or \code{FALSE} (default), parameters CI
#'   in table
#' @param showExpbCI \code{TRUE} (default) or \code{FALSE} , exp(B) CI in
#'   table
#' @param paramCIWidth a number between 50 and 99.9 (default: 95) specifying
#'   the confidence interval width for the parameter estimates
#' @param contrasts   a named vector of the form \code{c(var1='type', var2='type2')} specifying the type of contrast to use,
#'    one of \code{'deviation'}, \code{'simple'}, \code{'dummy'}, \code{'difference'}, \code{'helmert'}, \code{'repeated'} or \code{'polynomial'}.
#'     If NULL, \code{'simple'} is used. Can also be passed as a list of list of the form \code{list(list(var='var1',type='type1'))}.
#' @param showRealNames \code{TRUE} or \code{FALSE} (default), provide raw
#'   names of the contrasts variables
#' @param showContrastCode \code{TRUE} or \code{FALSE} (default), provide
#'   contrast coefficients tables
#' @param plotHAxis a string naming the variable placed on the horizontal axis
#'   of the plot
#' @param plotSepLines a string naming the variable represented as separate
#'   lines on the plot
#' @param plotSepPlots a string naming the variable to separate over to form
#'   multiple plots
#' @param plotRaw \code{TRUE} or \code{FALSE} (default), provide descriptive
#'   statistics
#' @param plotDvScale .
#' @param plotError \code{'none'}, \code{'ci'} (default), or \code{'se'}. Use
#'   no error bars, use confidence intervals, or use standard errors on the
#'   plots, respectively
#' @param ciWidth a number between 50 and 99.9 (default: 95) specifying the
#'   confidence interval width
#' @param postHoc a list of terms to perform post-hoc tests on
#' @param eDesc \code{TRUE} or \code{FALSE} (default), provide lsmeans
#'   statistics
#' @param eCovs \code{TRUE} or \code{FALSE} (default), provide lsmeans
#'   statistics
#' @param simpleVariable The variable for which the simple effects (slopes)
#'   are computed
#' @param simpleModerator the variable that provides the levels at which the
#'   simple effects computed
#' @param simple3way a moderator of the two-way interaction which is probed
#' @param simpleScale \code{'mean_sd'} (default), \code{'custom'} , or
#'   \code{'custom_percent'}. Use to condition the covariates (if any)
#' @param cvalue offset value for conditioning
#' @param percvalue offset value for conditioning
#' @param simpleScaleLabels style for presenting condition values of a moderator. It can be \code{'labels'} (default), \code{'values'} or \code{'labels_values'} for both.   
#' @param postHocCorr one or more of \code{'none'},  \code{'bonf'}, or
#'   \code{'holm'}; provide no,  Bonferroni, and Holm Post Hoc corrections
#'   respectively
#' @param scaling a named vector of the form \code{c(var1='type1',var2='type2')}. Types are
#'   \code{'centered'} to the mean, \code{'standardized'}, log-transformed \code{'log'} or \code{'none'}.
#'   \code{'none'} leaves the variable as it is. It can also be passed as a list of lists.
#' @param effectSize .
#' @param modelSelection Select the generalized linear model:
#'   \code{linear},\code{poisson},\code{logistic},\code{multinomial}
#' @param custom_family Distribution family for the custom model, accepts
#'   gaussian, binomial, gamma and inverse_gaussian .
#' @param custom_link Distribution family for the custom model, accepts
#'   identity, log and inverse, onemu2 (for 1/mu^2).
#' @param formula (optional) the formula to use, see the examples
#' @return A results object containing:
#' \tabular{llllll}{
#'   \code{results$model} \tab \tab \tab \tab \tab The underlying \code{glm} object \cr
#'   \code{results$info} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$main$anova} \tab \tab \tab \tab \tab a table of ANOVA results \cr
#'   \code{results$main$fixed} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$main$relativerisk} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$main$contrastCodeTables} \tab \tab \tab \tab \tab an array of contrast coefficients tables \cr
#'   \code{results$postHocs} \tab \tab \tab \tab \tab an array of post-hoc tables \cr
#'   \code{results$simpleEffects$Anova} \tab \tab \tab \tab \tab a table of ANOVA for simple effects \cr
#'   \code{results$simpleEffects$Params} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$emeansTables} \tab \tab \tab \tab \tab an array of predicted means tables \cr
#'   \code{results$descPlot} \tab \tab \tab \tab \tab a descriptives plot \cr
#'   \code{results$descPlots} \tab \tab \tab \tab \tab an array of results plots \cr
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
  data,
  dep,
  factors = NULL,
  covs = NULL,
  modelTerms = NULL,
  fixedIntercept = TRUE,
  showParamsCI = FALSE,
  showExpbCI = TRUE,
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
    "expb"),
  modelSelection = "linear",
  custom_family = "gaussian",
  custom_link = "identity",
  formula) {
  
  if ( ! requireNamespace('jmvcore'))
    stop('gamljGzlm requires jmvcore to be installed (restart may be required)')
  
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
  
  
  
  
  options <- gamljGzlmOptions$new(
    dep = dep,
    factors = factors,
    covs = covs,
    modelTerms = modelTerms,
    fixedIntercept = fixedIntercept,
    showParamsCI = showParamsCI,
    showExpbCI = showExpbCI,
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
    modelSelection = modelSelection,
    custom_family = custom_family,
    custom_link = custom_link)
  
  analysis <- gamljGzlmClass$new(
    options = options,
    data = data)
  
  analysis$run()
  
  analysis$results
}
