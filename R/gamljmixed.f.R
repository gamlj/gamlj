#' Mixed Model
#'
#' Mixed Linear Model
#'
#' @examples
#' data(subjects_by_stimuli)
#'
#' gamlj::gamljMixed(
#'        formula = y ~ 1 + cond+( 1|subj ),
#'        data = subjects_by_stimuli)
#'
#' gamlj::gamljMixed(
#'        data = subjects_by_stimuli,
#'        dep = "y",
#'        factors = "cond",
#'        modelTerms = "cond",
#'        cluster = "subj",
#'        randomTerms=list(list(c("cond","subj"))))
#'
#' @param data the data as a data frame
#' @param dep a string naming the dependent variable from \code{data},
#'   variable must be numeric
#' @param factors a vector of strings naming the fixed factors from
#'   \code{data}
#' @param covs a vector of strings naming the covariates from \code{data}
#' @param modelTerms a list of character vectors describing fixed effects
#'   terms
#' @param fixedIntercept \code{TRUE} (default) or \code{FALSE}, estimates
#'   fixed intercept
#' @param showParamsCI \code{TRUE} (default) or \code{FALSE} , parameters CI
#'   in table
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
#' @param plotSepPlots the variable for whose levels multiple plots are
#'   computed
#' @param plotRaw \code{TRUE} or \code{FALSE} (default), provide descriptive
#'   statistics
#' @param plotDvScale \code{TRUE} or \code{FALSE} (default), scale the plot
#'   Y-Axis to the max and the min of the dependent variable observed scores.
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
#' @param simpleScaleLabels decide the labeling of simple effects in tables
#'   and plots.  \code{labels} indicates that only labels are used, such as
#'   \code{Mean} and  \code{Mean + 1 SD}. \code{values} uses the actual values
#'   as labels. \code{values_labels} uses both.
#' @param postHocCorr one or more of \code{'none'},  \code{'bonf'}, or
#'   \code{'holm'}; provide no,  Bonferroni, and Holm Post Hoc corrections
#'   respectively
#' @param scaling a named vector of the form \code{c(var1='type1',var2='type2')}. Types are
#'   \code{'centered'} to the mean, \code{'clusterbasedcentered'} to the mean of each cluster, \code{'standardized'},
#'    \code{'clusterbasedstandardized'} standardized within each cluster, log-transformed \code{'log'},  or \code{'none'}.
#'   \code{'none'} leaves the variable as it is. It can also be passed as a list of lists.
#' @param dep_scale Re-scale the dependent variable. It could be  \code{'centered'} to the mean, \code{'clusterbasedcentered'} to the mean of each cluster, \code{'standardized'},
#'    \code{'clusterbasedstandardized'} standardized within each cluster, log-transformed \code{'log'},  or \code{'none'} (default).
#' @param cluster a vector of strings naming the clustering variables from
#'   \code{data}
#' @param randomTerms a list of lists specifying the models random effects.
#' @param correlatedEffects \code{'nocorr'}, \code{'corr'} (default), or
#'   \code{'block'}. When random effects are passed as list of length 1, it
#'   decides whether the effects should be correlated,  non correlated. If
#'   \code{'randomTerms'} is a list of  lists of length > 1, the option is
#'   automatially set to \code{'block'}. The option is ignored if the model is
#'   passed using \code{formula}.
#' @param reml \code{TRUE} (default) or \code{FALSE}, should the Restricted ML
#'   be used rather than ML
#' @param lrtRandomEffects \code{TRUE} or \code{FALSE} (default), LRT for the
#'   random effects
#' @param plotRandomEffects \code{TRUE} or \code{FALSE} (default), add random
#'   effects predicted values in the plot
#' @param cimethod .
#' @param dfmethod .
#' @param qq \code{TRUE} or \code{FALSE} (default), provide a Q-Q plot of
#'   residuals
#' @param normTest \code{TRUE} or \code{FALSE} (default), provide a test for
#'   normality of residuals
#' @param normPlot \code{TRUE} or \code{FALSE} (default), provide a histogram
#'   of residuals superimposed by a normal distribution
#' @param residPlot \code{TRUE} or \code{FALSE} (default), provide a
#'   scatterplot of the residuals against predicted
#' @param clusterBoxplot \code{TRUE} or \code{FALSE} (default), provide a
#'   boxplot of random effects by the first defined clustering variable
#' @param randHist \code{TRUE} or \code{FALSE} (default), provide histogram of
#'   random Coefficients
#' @param formula (optional) the formula to use, see the examples
#' @return A results object containing:
#' \tabular{llllll}{
#'   \code{results$model} \tab \tab \tab \tab \tab The underlying \code{lm} object \cr
#'   \code{results$info} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$main$anova} \tab \tab \tab \tab \tab a table of ANOVA results \cr
#'   \code{results$main$fixed} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$main$random} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$main$randomCov} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$main$lrtRandomEffectsTable} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$main$contrastCodeTables} \tab \tab \tab \tab \tab an array of contrast coefficients tables \cr
#'   \code{results$postHocs} \tab \tab \tab \tab \tab an array of post-hoc tables \cr
#'   \code{results$simpleEffects$Anova} \tab \tab \tab \tab \tab a table of ANOVA for simple effects \cr
#'   \code{results$simpleEffects$Params} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$emeansTables} \tab \tab \tab \tab \tab an array of predicted means tables \cr
#'   \code{results$descPlot} \tab \tab \tab \tab \tab a descriptives plot \cr
#'   \code{results$descPlots} \tab \tab \tab \tab \tab an array of results plots \cr
#'   \code{results$plotnotes} \tab \tab \tab \tab \tab a html \cr
#'   \code{results$assumptions$normTest} \tab \tab \tab \tab \tab a table of normality tests \cr
#'   \code{results$assumptions$qq} \tab \tab \tab \tab \tab a q-q plot \cr
#'   \code{results$assumptions$normPlot} \tab \tab \tab \tab \tab Residual histogram \cr
#'   \code{results$assumptions$residPlot} \tab \tab \tab \tab \tab Residual Predicted plot \cr
#'   \code{results$assumptions$clusterBoxplot} \tab \tab \tab \tab \tab Residuals boxplot by cluster \cr
#'   \code{results$assumptions$randHist} \tab \tab \tab \tab \tab an array of random coefficients histograms \cr
#' }
#'
#' Tables can be converted to data frames with \code{asDF} or \code{\link{as.data.frame}}. For example:
#'
#' \code{results$info$asDF}
#'
#' \code{as.data.frame(results$info)}
#'
#' @export
gamljMixed <- function(
  data,
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
  dep_scale = "none",
  cluster = NULL,
  randomTerms = list(
    list()),
  correlatedEffects = "corr",
  reml = TRUE,
  lrtRandomEffects = FALSE,
  plotRandomEffects = FALSE,
  cimethod = "wald",
  dfmethod = "Satterthwaite",
  qq = FALSE,
  normTest = FALSE,
  normPlot = FALSE,
  residPlot = FALSE,
  clusterBoxplot = FALSE,
  randHist = FALSE,
  formula) {
  
  if ( ! requireNamespace('jmvcore'))
    stop('gamljMixed requires jmvcore to be installed (restart may be required)')
  
  if ( ! missing(formula)) {
    if (missing(dep))
      dep <- gamljMixedClass$private_methods$.marshalFormula(
        formula=formula,
        data=`if`( ! missing(data), data, NULL),
        name="dep")
    if (missing(factors))
      factors <- gamljMixedClass$private_methods$.marshalFormula(
        formula=formula,
        data=`if`( ! missing(data), data, NULL),
        name="factors")
    if (missing(covs))
      covs <- gamljMixedClass$private_methods$.marshalFormula(
        formula=formula,
        data=`if`( ! missing(data), data, NULL),
        name="covs")
    if (missing(cluster))
      cluster <- gamljMixedClass$private_methods$.marshalFormula(
        formula=formula,
        data=`if`( ! missing(data), data, NULL),
        name="cluster")
    if (missing(randomTerms))
      randomTerms <- gamljMixedClass$private_methods$.marshalFormula(
        formula=formula,
        data=`if`( ! missing(data), data, NULL),
        name="randomTerms")
    if (missing(modelTerms))
      modelTerms <- gamljMixedClass$private_methods$.marshalFormula(
        formula=formula,
        data=`if`( ! missing(data), data, NULL),
        name="modelTerms")
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
  if ( ! missing(cluster)) cluster <- jmvcore::resolveQuo(jmvcore::enquo(cluster))
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
      `if`( ! missing(simple3way), simple3way, NULL),
      `if`( ! missing(cluster), cluster, NULL))
  
  for (v in factors) if (v %in% names(data)) data[[v]] <- as.factor(data[[v]])
  if (inherits(modelTerms, 'formula')) modelTerms <- jmvcore::decomposeFormula(modelTerms)
  if (inherits(postHoc, 'formula')) postHoc <- jmvcore::decomposeFormula(postHoc)
  
  
  ### fix some options when passed by R ####
  if (is.something(names(scaling))) 
    scaling<-lapply(names(scaling), function(a) list(var=a,type=scaling[[a]]))
  if (is.something(names(contrasts))) 
    contrasts<-lapply(names(contrasts), function(a) list(var=a,type=contrasts[[a]]))

  
  options <- gamljMixedOptions$new(
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
    dep_scale = dep_scale,
    cluster = cluster,
    randomTerms = randomTerms,
    correlatedEffects = correlatedEffects,
    reml = reml,
    lrtRandomEffects = lrtRandomEffects,
    plotRandomEffects = plotRandomEffects,
    cimethod = cimethod,
    dfmethod = dfmethod,
    qq = qq,
    normTest = normTest,
    normPlot = normPlot,
    residPlot = residPlot,
    clusterBoxplot = clusterBoxplot,
    randHist = randHist)
  
  analysis <- gamljMixedClass$new(
    options = options,
    data = data)
  
  analysis$run()
  
  analysis$results
}
