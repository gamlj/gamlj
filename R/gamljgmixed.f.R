#'
#' Generalized Mixed  Model
#' 
#'
#' @examples
#' data(schoolexam)
#' gamlj::gamlj_gmixed(formula = pass ~ 1 + math+( 1|school ),
#'                      data = schoolexam
#'                      )
#'        
#' @param formula (optional) the formula to use, see the examples
#' @param data the data as a data frame
#' @param model_type Select the generalized linear model:
#'                    \code{logistic},\code{probit},\code{nb} for negative binomial
#'                    \code{poisson},\code{multinomial}, \code{ordinal} for cumulative logit models.
#' @param dep a string naming the dependent variable from \code{data}; the
#'   variable must be numeric. Not needed if \code{formula} is used.
#' @param factors a vector of strings naming the fixed factors from
#'   \code{data}. Not needed if \code{formula} is used.
#' @param covs a vector of strings naming the covariates from \code{data}. Not
#'   needed if \code{formula} is used.
#' @param model_terms a list of character vectors describing fixed effects
#'   terms. Not needed if \code{formula} is used.
#' @param fixed_intercept \code{TRUE} (default) or \code{FALSE}, estimates
#'                         fixed intercept. Not needed if \code{formula} is used
#'                         and "1" is passed explicitly to the formula.#'                         
#' @param cluster a vector of strings naming the clustering variables from
#'   \code{data}, not needed if \code{formula} is defined.
#' @param re a list of lists specifying the models random effects.
#' @param re_lrt \code{TRUE} or \code{FALSE} (default), LRT for the random
#'   effects
#' @param re_ci \code{TRUE} or \code{FALSE} (default), confidence intervals
#'   for the random effects
#'                         
#' @param es Effect size indices. \code{expb} (default) exponentiates the
#'           coefficients. For dichotomous dependent variables relative risk indices
#'           (RR) can be obtained. \code{marginals} computes the marginal effects.
#' @param expb_ci \code{TRUE} (default) or \code{FALSE} , exp(B) CI in table
#' @param nested_terms a list of character vectors describing effects terms
#'   for the nested model. It can be passed as right-hand formula.
#' @param nested_intercept \code{TRUE} (default) or \code{FALSE}, estimates
#'   fixed intercept. Not needed if \code{formula} is used.
#' @param nested_re a list of lists specifying the models random effects.
#' @param re_corr \code{'all'}, \code{'none'} (default), or \code{'block'}.
#'   When random effects are passed as list of length 1, it decides whether the
#'   effects should be correlated,  non correlated. If \code{'re'} is a list of
#'   lists of length > 1, the option is automatially set to \code{'block'}. The
#'   option is ignored if the model is passed using \code{formula}.
#' @param estimates_ci \code{TRUE}  or \code{FALSE} (default), parameters CI
#'   in table
#' @param ci_method   The method used to compute the confidence intervals. \code{'wald'} uses the Wald method to compute standard 
#'    errors and confidence intervals. `profile` computes Profile Likelihood Based Confidence Interval, in which 
#'    the bounds are chosen based on the percentiles of the chi-square distribution around the maximum likelihood
#'    estimate. \code{'quantile'} performs a non-parametric boostrap, with \code{boot_r} repetitions, and compute
#'    the CI based on the percentiles of the boostrap distribution. \code{'bcai'} implements the bias-corrected bootstrap method.
#' @param ci_width a number between 50 and 99.9 (default: 95) specifying the
#'   confidence interval width for the plots.
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
#' @param plot_x a string naming the variable placed on the horizontal axis of
#'   the plot
#' @param plot_z a string naming the variable represented as separate lines in
#'   the plot
#' @param plot_by a list of string naming the variables defining the levels
#'   for multiple plots
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
#' @param plot_re \code{TRUE} or \code{FALSE} (default), add predicted values
#'   based on random effect in plot
#' @param plot_re_method .
#' @param plot_scale Plot ordinal model predicted values in as probabilities
#'   (\code{response}) or predicted class (\code{mean.class})
#' @param emmeans a rhs formula with the terms specifying the marginal means
#'   to estimate (of the form \code{'~x+x:z'})
#' @param posthoc a rhs formula with the terms specifying the table to apply
#'   the comparisons (of the form \code{'~x+x:z'}). The formula is not expanded,
#'   so \code{'x*z'} becomes \code{'x+z'} and not \code{'x+z+x:z'}. It can be
#'   passed also as a list of the form '`list("x","z",c("x","z")`'
#' @param simple_x The variable for which the simple effects (slopes)
#'   are computed
#' @param simple_mods the variable that provides the levels at which the
#'   simple effects are computed
#' @param simple_interactions should simple Interactions be computed
#' @param covs_conditioning \code{'mean_sd'} (default), \code{'custom'} , or
#'   \code{'percent'}. Use to condition the covariates (if any)
#' @param ccm_value Covariates conditioning mean offset value: how many
#'   st.deviations around the means used to condition simple effects and plots.
#'   Used if \code{covs_conditioning}=\code{'mean_sd'}
#' @param ccp_value Covariates conditioning percentile offset value: number of
#'   percentiles around the median used to condition simple effects and plots.
#'   Used if \code{covs_conditioning}=\code{'percent'}
#' @param covs_scale_labels how the levels of a continuous moderator should
#'   appear in tables and plots: \code{labels}, \code{values} and
#'   \code{values_labels}, \code{ovalues}, `ovalues_labels. The latter two refer
#'   to the variable orginal levels, before scaling.
#' @param adjust one or more of \code{'none'},  \code{'bonf'},\code{'tukey'}
#'   \code{'holm'},\code{'scheffe'}, \code{'tukey'}; provide no,  Bonferroni,
#'   Tukey and Holm Post Hoc corrections respectively.
#' @param covs_scale a list of lists specifying the covariates scaling, one of
#'   \code{'centered to the mean'}, \code{'standardized'}, or \code{'none'}.
#'   \code{'none'} leaves the variable as it is
#' @param scale_missing .
#' @param norm_test \code{TRUE} or \code{FALSE} (default), provide a test for
#'   normality of residuals
#' @return A results object containing:
#' \tabular{llllll}{
#'   \code{results$model} \tab \tab \tab \tab \tab a property \cr
#'   \code{results$info} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$main$r2} \tab \tab \tab \tab \tab a table of R \cr
#'   \code{results$main$fit} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$main$anova} \tab \tab \tab \tab \tab a table of omnibus \cr
#'   \code{results$main$coefficients} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$main$contrastCodeTables} \tab \tab \tab \tab \tab an array of contrast coefficients tables \cr
#'   \code{results$main$marginals} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$main$relativerisk} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$main$random} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$main$randomcov} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$main$multirandom} \tab \tab \tab \tab \tab an array \cr
#'   \code{results$main$ranova} \tab \tab \tab \tab \tab a table \cr
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
gamlj_gmixed <- function(
    formula=NULL,  
    data=NULL,
    model_type = "logistic",
    dep = NULL,
    factors = NULL,
    covs = NULL,
    model_terms = NULL,
    fixed_intercept = TRUE,
    cluster=cluster,
    re = NULL,
    re_corr = "all",
    re_lrt = FALSE,
    re_ci = FALSE,
    nested_terms = NULL,
    nested_intercept = NULL,
    nested_re = NULL,
    es = list("expb"),
    expb_ci = TRUE,
    estimates_ci = FALSE,
    ci_method = "wald",
    boot_r = 1000,
    ci_width = 95,
    contrasts = NULL,
    show_contrastnames = TRUE,
    show_contrastcodes = FALSE,
    plot_x = NULL,
    plot_z = NULL,
    plot_by = NULL,
    plot_raw = FALSE,
    plot_yscale = FALSE,
    plot_xoriginal = FALSE,
    plot_black = FALSE,
    plot_around = "none",
    plot_re = FALSE,
    plot_re_method = "average",
    plot_scale = "response",
    emmeans = NULL,
    posthoc = NULL,
    simple_x = NULL,
    simple_mods = NULL,
    simple_interactions = FALSE,
    covs_conditioning = "mean_sd",
    ccm_value = 1,
    ccp_value = 25,
    covs_scale_labels = "labels",
    adjust = list("bonf"),
    covs_scale = NULL,
    scale_missing = "complete",
    norm_test = FALSE
     ) {
  
  if ( ! requireNamespace("jmvcore", quietly=TRUE))
    stop("gamljGlmMixed requires jmvcore to be installed (restart may be required)")
  

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
      `if`( ! missing(simple_mods), simple_mods, NULL)
    )


  ## custom code
  .caller      = "glmer"
  .interface   = "R"
   donotrun    = FALSE
   mute        = FALSE
   re_terms    = NULL
   
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
     re_terms        <- f$random
     cluster         <- f$clusters
   }
   # if no formula or no terms is passed, covs and factors are the terms
   if (is.null(model_terms)) model_terms<-as.list(c(factors,covs))

   ### handle random effects if necessary
   if (is.null(re_terms)) {
     if (inherits(re,"formula")) {
       r<-rFormula$new(re)
       re_terms <- r$random
       cluster  <- r$clusters
     }
     
     if (inherits(re,"list")) {
       r<-gFormula$new()
       r$random <- re
       cluster  <- r$clusters
       re_terms <- re
       
     }
   } 
   if (is.null(re_terms)) re_terms<-list()
   
   
   # nested terms
   comparison = FALSE

   if (is.something(nested_terms) & !inherits(nested_terms, "formula"))   
       warning("Nested terms are ignored. Please specify them with a RHS formula.")
   
   if (inherits(nested_terms, "formula")) {
     f<-rFormula$new(nested_terms)
     nested_intercept<-f$intercept
     nested_terms <-f$terms
   } 

   if (inherits(nested_re, "formula")) {
     f<-rFormula$new(nested_re)
     nested_re=f$random
   }
   
   if (is.something(nested_terms) || !is.null(nested_intercept) || is.something(nested_re))
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
   
    
  options <- gamljgmixedOptions$new(
    .caller = .caller,
    .interface = .interface,
    dep = dep,
    factors = factors,
    covs = covs,
    model_terms = model_terms,
    fixed_intercept = fixed_intercept,
    es = es,
    expb_ci = expb_ci,
    nested_terms = nested_terms,
    comparison = comparison,
    nested_intercept = nested_intercept,
    estimates_ci = estimates_ci,
    donotrun = donotrun,
    ci_method = ci_method,
    boot_r = boot_r,
    ci_width = ci_width,
    contrasts = contrasts,
    show_contrastnames = show_contrastnames,
    show_contrastcodes = show_contrastcodes,
    plot_x = plot_x,
    plot_z = plot_z,
    plot_by = plot_by,
    plot_raw = plot_raw,
    plot_yscale = plot_yscale,
    plot_xoriginal = plot_xoriginal,
    plot_black = plot_black,
    plot_around = plot_around,
    plot_re = plot_re,
    plot_re_method = plot_re_method,
    plot_scale = plot_scale,
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
    model_type = model_type,
    covs_scale = covs_scale,
    mute = mute,
    scale_missing = scale_missing,
    norm_test = norm_test,
    cluster = cluster,
    re = re_terms,
    nested_re = nested_re,
    re_corr = re_corr,
    re_lrt = re_lrt,
    re_ci = re_ci)
  
  analysis <- gamljgmixedClass$new(
    options = options,
    data = data)
  
  analysis$run()
  
  analysis$results
}

