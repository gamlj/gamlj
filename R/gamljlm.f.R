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
#' GAMLj3::gamlj_lm(formula = len ~ supp,  data = ToothGrowth)
#' @param formula (optional) the formula of the model, see the examples. If not passed
#'                model terms should be defined as a list in  \code{model_terms} option.
#' @param data the data as a data frame
#' @param fixed_intercept \code{TRUE} (default) or \code{FALSE}, estimates
#'   fixed intercept. Overridden if \code{formula} is used and contains \code{~1} or \code{~0}.
#' @param dep a string naming the dependent variable from \code{data}; the
#'   variable must be numeric. Not needed if \code{formula} is used.
#' @param factors a vector of strings naming the fixed factors from
#'   \code{data}. Not needed if \code{formula} is used.
#' @param covs a vector of strings naming the covariates from \code{data}. Not
#'   needed if \code{formula} is used.
#' @param model_terms a list of character vectors describing fixed effects
#'   terms. Not needed if \code{formula} is used.
#' @param nested_terms A right-hand formula for the nested model. It can be passed as a list of character vectors describing effects terms
#'   for the nested model. 
#' @param nested_intercept \code{TRUE} (default) or \code{FALSE}, estimates
#'   fixed intercept. Overridden if \code{model_ters} is a formula and contains \code{~1} or \code{~0}..
#' @param omnibus Omnibus tests are based on F-test \code{F} (default) or
#'   loglikelihood ration test \code{LRT}.
#' @param estimates_ci \code{TRUE} (default) or \code{FALSE} , parameters CI
#'   in table
#' @param betas_ci \code{TRUE} (default) or \code{FALSE} , parameters CI in
#'   table
#' @param ci_width a number between 50 and 99.9 (default: 95) specifying the
#'   confidence interval width for the plots.
#' @param ci_method the method to compute the confidence intervals. It can be `wald` (default) for
#'        large samples confidence intervals, `quantile` for percentile bootstrap method, or `bcai`
#'        for bias corrected accelarated method.
#' @param boot_r a number bootstrap repetitions.
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
#'   so \code{'x*z'} becomes \code{'x+z'} and not \code{'x+z+x:z'}. It can be
#'   passed also as a list of the form \code{list("x","z",c("x","z")}
#'   
#' @param simple_x The variable for which the simple effects (slopes)
#'   are computed
#' @param simple_mods a character vector with the variable(s) providing the levels at which the
#'   simple effects are computed
#' @param simple_interactions should simple Interactions be computed
#' @param covs_scale a named vector of the form \code{c(var1='type',
#'   var2='type2')} specifying the transformation to apply to covariates, one of
#'   \code{'centered'} to the mean, \code{'standardized'} or  \code{'none'}.
#'   \code{'none'} leaves the variable as it is.
#' @param covs_conditioning '\code{mean_sd}' (default), or '\code{percent}'.
#'   How to condition covariates in simple effects and plots. '\code{mean_sd}'
#'   for mean +/- '\code{ccp_value}' * sd. '\code{percent}' for median
#'   +/-'\code{ccp_value}' for percentiles. \code{range} conditions to min and max and \code{ccmm_steps} values in between.
#' @param ccm_value how many st.deviations around the means used to condition
#'   simple effects and plots. Used if \code{covs_conditioning}=\code{'mean_sd'}
#' @param ccp_value offsett (number of percentiles) around the median used to
#'   condition simple effects and plots. Used if
#'   \code{simpleScale}=\code{'percent'}
#' @param ccmm_value   Covariate condition min max steps (not very mnemonic): 
#'        At how many values between min and max should the covariate be conditioned \code{simpleScale}=\code{'range'}
#'   
#' @param covs_scale_labels how the levels of a continuous moderator should
#'   appear in tables and plots: \code{labels}, \code{values} and
#'   \code{values_labels}, \code{ovalues}, `ovalues_labels. The latter two refer
#'   to the variable orginal levels, before scaling.
#' @param adjust adjustment method for postho tests. One or more of
#'   \code{'none'},  \code{'bonf'},\code{'tukey'}  \code{'holm'}; provide no,
#'   Bonferroni, Tukey and Holm Post Hoc corrections respectively.
#' @param posthoc_es effect size indices for mean comparisons. One or more of
#'   \code{'dm'},  \code{'ds'},\code{'g'} for Cohen's d (dm=model SD,ds=sample
#'   SD )  or Hedge's g
#' @param d_ci \code{TRUE} or \code{FALSE} (default), d confidence intervals
#' @param es a list of effect sizes to print out. They can be:  \code{"eta"}
#'   for eta-squared, \code{'etap'} for partial eta-squared, \code{'omega'} for
#'   omega-squared, \code{'omegap'} for partial omega-squared, \code{'epsilon'}
#'   for epsilon-squared, \code{'epsilonp'} for partial epsilon-squared  and
#'   \code{'beta'} for standardized  coefficients (betas). Default is
#'   \code{"beta"} and \code{"parEta"}.
#' @param homo_test \code{TRUE} or \code{FALSE} (default), performs homogeneity
#'   tests
#' @param colli_test \code{TRUE} or \code{FALSE} (default), computes VIF and Tollerance 
#'        for the terms in the model

#' @param qq_plot \code{TRUE} or \code{FALSE} (default), provide a Q-Q plot of
#'   residuals
#' @param norm_test \code{TRUE} or \code{FALSE} (default), provide a test for
#'   normality of residuals
#' @param norm_plot \code{TRUE} or \code{FALSE} (default), provide a histogram
#'   of residuals superimposed by a normal distribution
#' @param resid_plot \code{TRUE} or \code{FALSE} (default), provide a
#'   scatterplot of the residuals against predicted
#' @param intercept_info \code{TRUE} or \code{FALSE} (default), provide
#'   ìnformation about the intercept (F test, effect size indexes)
#' @param es_info \code{TRUE} or \code{FALSE} (default), provide ìnformation
#'   about the effect size indexes
#' @param dep_scale Re-scale the dependent variable.
#' @param se_method Method to compute the standard error. 
#'                  Classical standard errors is the default \code{standard}. 
#'                  Four methods for  heteroschedasticy-consistent 
#'                  standard errors are available: \code{HC0},
#'                  \code{HC1},\code{HC2},\code{HC3}, from package \code{sandwich} .
#'                  See \code{\link[sandwich]{vcovHC}} for details.  
#'                  
#' @return A results object containing:
#' \tabular{llllll}{
#'   \code{results$model} \tab \tab \tab \tab \tab a property \cr
#'   \code{results$info} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$main$r2} \tab \tab \tab \tab \tab a table of R \cr
#'   \code{results$main$intercept} \tab \tab \tab \tab \tab a table of information for the model intercept \cr
#'   \code{results$main$anova} \tab \tab \tab \tab \tab a table of ANOVA results \cr
#'   \code{results$main$effectsizes} \tab \tab \tab \tab \tab a table of effect size indeces \cr
#'   \code{results$main$coefficients} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$main$vcov} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$main$contrastCodeTables} \tab \tab \tab \tab \tab an array of contrast coefficients tables \cr
#'   \code{results$posthoc} \tab \tab \tab \tab \tab an array of post-hoc tables \cr
#'   \code{results$posthocEffectSize} \tab \tab \tab \tab \tab an array of post-hoc effect size \cr
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
#' \code{results$info$asDF}
#'
#' \code{as.data.frame(results$info)}
#'
#' @export
gamlj_lm <- function(
    formula=NULL,
    data,
    dep = NULL,
    fixed_intercept = TRUE,
    factors = NULL,
    covs = NULL,
    model_terms = NULL,
    nested_terms = NULL,
    nested_intercept = NULL,
    omnibus = "F",
    estimates_ci = TRUE,
    betas_ci = FALSE,
    ci_width = 95,
    ci_method = "wald",
    boot_r = 1000,
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
    covs_scale = NULL,
    covs_conditioning = "mean_sd",
    ccm_value = 1,
    ccp_value = 25,
    ccmm_steps = 1,
    covs_scale_labels = "labels",
    adjust = list(
      "bonf"),
    posthoc_es = list(
      "dm"),
    d_ci = FALSE,
    es = list(
      "beta",
      "etap"),
    homo_test = FALSE,
    colli_test = FALSE,
    qq_plot = FALSE,
    norm_test = FALSE,
    norm_plot = FALSE,
    resid_plot = FALSE,
    intercept_info = FALSE,
    es_info = FALSE,
    dep_scale = "none",
    se_method = "standard") {
  
  if ( ! requireNamespace("jmvcore", quietly=TRUE))
    stop("gamljGlm requires jmvcore to be installed (restart may be required)")
  
  if ( ! missing(dep)) dep <- jmvcore::resolveQuo(jmvcore::enquo(dep))
  if ( ! missing(factors)) factors <- jmvcore::resolveQuo(jmvcore::enquo(factors))
  if ( ! missing(covs)) covs <- jmvcore::resolveQuo(jmvcore::enquo(covs))
  if ( ! missing(plot_x)) plot_x <- jmvcore::resolveQuo(jmvcore::enquo(plot_x))
  if ( ! missing(plot_z)) plot_z <- jmvcore::resolveQuo(jmvcore::enquo(plot_z))
  if ( ! missing(plot_by)) plot_by <- jmvcore::resolveQuo(jmvcore::enquo(plot_by))
  if ( ! missing(simple_x)) simple_x <- jmvcore::resolveQuo(jmvcore::enquo(simple_x))
  if ( ! missing(simple_mods)) simple_mods <- jmvcore::resolveQuo(jmvcore::enquo(simple_mods))
  if (missing(data))
    data <- jmvcore::marshalData(
      parent.frame(),
      `if`( ! missing(dep), dep, NULL),
      `if`( ! missing(factors), factors, NULL),
      `if`( ! missing(covs), covs, NULL),
      `if`( ! missing(plot_x), plot_x, NULL),
      `if`( ! missing(plot_z), plot_z, NULL),
      `if`( ! missing(plot_by), plot_by, NULL),
      `if`( ! missing(simple_x), simple_x, NULL),
      `if`( ! missing(simple_mods), simple_mods, NULL))
  

  for (v in factors) if (v %in% names(data)) data[[v]] <- as.factor(data[[v]])
  ##### custom code
  .caller = "lm"
  model_type="lm"
  .interface = "R"
  donotrun = FALSE
  ### model terms
  if (inherits(model_terms, "formula")) {
    f<-rFormula$new(model_terms,data)
    model_terms     <- f$terms
    if (missing(fixed_intercept))
       fixed_intercept<-f$intercept
    if (missing(factors))
         factors<-f$factors
    if (missing(covs))
      factors<-f$covs
  }
    
  if (!is.null(formula)) {
         f<-rFormula$new(formula,data)
         dep             <- f$dep
         factors         <- f$factors
         covs            <- f$covs
         fixed_intercept <- f$intercept
         model_terms     <- f$terms
  }
  # if no formula or no terms is passed, covs and factors are the terms
  if (is.null(model_terms)) model_terms<-as.list(c(factors,covs))
  
  # nested terms
  comparison = FALSE
  
  if (inherits(nested_terms, "formula")) {
    f<-rFormula$new(nested_terms)
    nested_intercept<-f$intercept
    nested_terms <-f$terms
  }
  
  if (is.something(nested_terms) | !is.null(nested_intercept))
    comparison<-TRUE

  ### other from formula to list  
  if (inherits(emmeans, "formula")) emmeans <- jmvcore::decomposeFormula(emmeans)
  if (inherits(posthoc, "formula")) posthoc <- jmvcore::decomposeFormula(posthoc)

  ### fix some options when passed by R ####
  if (is.something(names(covs_scale)))
    covs_scale<-lapply(names(covs_scale), function(a) list(var=a,type=covs_scale[[a]]))

  if (is.something(names(contrasts)))
    contrasts<-lapply(names(contrasts), function(a) list(var=a,type=contrasts[[a]]))
  
  ## end of custom code
  
  if (se_method!="standard") {
    se_method<-"robust"
    robust_method<-se_method
  }

  options <- gamljlmOptions$new(
    .caller = .caller,
    .interface = .interface,
    dep = dep,
    factors = factors,
    covs = covs,
    model_terms = model_terms,
    nested_terms = nested_terms,
    comparison = comparison,
    fixed_intercept = fixed_intercept,
    nested_intercept = nested_intercept,
    omnibus = omnibus,
    estimates_ci = estimates_ci,
    betas_ci = betas_ci,
    ci_width = ci_width,
    ci_method = ci_method,
    boot_r = boot_r,
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
    covs_scale = covs_scale,
    covs_conditioning = covs_conditioning,
    ccm_value = ccm_value,
    ccp_value = ccp_value,
    ccmm_steps = ccmm_steps,
    covs_scale_labels = covs_scale_labels,
    adjust = adjust,
    posthoc_es = posthoc_es,
    d_ci = d_ci,
    model_type = model_type,
    es = es,
    homo_test = homo_test,
    colli_test = homo_test,
    qq_plot = qq_plot,
    norm_test = norm_test,
    norm_plot = norm_plot,
    resid_plot = resid_plot,
    intercept_info = intercept_info,
    es_info = es_info,
    dep_scale = dep_scale,
    se_method = se_method)
  
  analysis <- gamljlmClass$new(
    options = options,
    data = data)
  
  analysis$run()
  
  analysis$results
}



