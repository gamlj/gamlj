#' R-squared and pseudo-rsquared for a list of (generalized) linear (mixed) models
#'
#' This function calls the generic \code{r.squared} function for each of the
#' models in the list and rbinds the outputs into one data frame
#'
#' @param obj single model or a list of fitted (generalized) linear (mixed) model objects
#' @return a dataframe with one row per model, and "Class",
#'         "Family", "Marginal", "Conditional" and "AIC" columns
#' @author: Jon Lefcheck
#' @references  Lefcheck, Jonathan S. (2015) piecewiseSEM: Piecewise structural equation modeling in R for ecology, evolution, and systematics. Methods in Ecology and Evolution. 7(5): 573-579. DOI: 10.1111/2041-210X.12512
#' 
#' 
#' @export
r.squared <- function(mdl){
  UseMethod("r.squared")
}

#' @export
rsquared.glmm <- function(obj) {
  if( class(obj) != "list" ) obj = list(obj) else obj
  # Iterate over each model in the list
  do.call(rbind, lapply(obj, r.squared))
}


#' @export
r.squared.lm <- function(mdl){
  data.frame(Class=class(mdl), Family="gaussian", Link="identity",
             Marginal=summary(mdl)$r.squared,
             Conditional=NA, AIC=stats::AIC(mdl))
}

#' @export
r.squared.merMod <- function(mdl){
  # Get variance of fixed effects by multiplying coefficients by design matrix
  VarF <- stats::var(as.vector(lme4::fixef(mdl) %*% t(mdl@pp$X)))
  # Get variance of random effects by extracting variance components
  # Omit random effects at the observation level, variance is factored in later
  VarRand <- sum(
    sapply(
      lme4::VarCorr(mdl)[!sapply(unique(unlist(strsplit(names(lme4::ranef(mdl)),":|/"))), function(l) length(unique(mdl@frame[,l])) == nrow(mdl@frame))],
      function(Sigma) {
        X <- stats::model.matrix(mdl)
        Z <- X[,rownames(Sigma)]
        sum(diag(Z %*% Sigma %*% t(Z)))/nrow(X) } ) )
  # Get the dispersion variance
  VarDisp <- unlist(lme4::VarCorr(mdl)[sapply(unique(unlist(strsplit(names(lme4::ranef(mdl)),":|/"))), function(l) length(unique(mdl@frame[,l])) == nrow(mdl@frame))])
  if(is.null(VarDisp)) VarDisp = 0 else VarDisp = VarDisp
  if(inherits(mdl, "lmerMod")){
    # Get residual variance
    VarResid <- attr(lme4::VarCorr(mdl), "sc")^2
    # Get ML model AIC
    mdl.aic <- stats::AIC(stats::update(mdl, REML=F))
    # Model family for lmer is gaussian
    family <- "gaussian"
    # Model link for lmer is identity
    link <- "identity"
  }
  else if(inherits(mdl, "glmerMod")){
    # Get the model summary
    mdl.summ <- summary(mdl)
    # Get the model's family, link and AIC
    family <- mdl.summ$family
    link <- mdl.summ$link
    mdl.aic <- stats::AIC(mdl)
    # Pseudo-r-squared for poisson also requires the fixed effects of the null model
    if(family=="poisson") {
      # Get random effects names to generate null model
      rand.formula <- stats::reformulate(sapply(lme4::findbars(stats::formula(mdl)),
                                         function(x) paste0("(", deparse(x), ")")),
                                  response=".")
      # Generate null model (intercept and random effects only, no fixed effects)
      null.mdl <- stats::update(mdl, rand.formula)
      # Get the fixed effects of the null model
      null.fixef <- as.numeric(lme4::fixef(null.mdl))
    }
  }
  # Call the internal function to do the pseudo r-squared calculations
  .rsquared.glmm(VarF, VarRand, VarResid, VarDisp, family = family, link = link,
                 mdl.aic = mdl.aic,
                 mdl.class = class(mdl),
                 null.fixef = null.fixef)
}

# r.squared.lme <- function(mdl){
#   # Get design matrix of fixed effects from model
#   Fmat <- model.matrix(eval(mdl$call$fixed)[-2], mdl$data)
#   # Get variance of fixed effects by multiplying coefficients by design matrix
#   VarF <- var(as.vector(nlme::fixef(mdl) %*% t(Fmat)))
#   # First, extract variance-covariance matrix of random effects
#   Sigma.list = lme4::VarCorr(mdl)[!grepl(" =",rownames(lme4::VarCorr(mdl))) & rownames(lme4::VarCorr(mdl)) != "Residual", colnames(lme4::VarCorr(mdl))=="Variance", drop=F]
#   corr.list = as.numeric(lme4::VarCorr(mdl)[!grepl(" =",rownames(lme4::VarCorr(mdl))) & rownames(lme4::VarCorr(mdl)) != "Residual" & rownames(lme4::VarCorr(mdl)) != "(Intercept)",colnames(lme4::VarCorr(mdl))=="Corr",drop=F])
#   Sigma.list2 = split(as.numeric(Sigma.list), cumsum(rownames(Sigma.list) == "(Intercept)"), drop=F)
#   Sigma.list2 = lapply(1:length(Sigma.list2), function(i) { 
#     mat = matrix(prod(Sigma.list2[[i]])*abs(corr.list[i]), ncol=length(Sigma.list2[[i]]), nrow=length(Sigma.list2[[i]]))
#     diag(mat) = Sigma.list2[[i]]
#     colnames(mat) = rownames(Sigma.list)[1:sum(cumsum(rownames(Sigma.list) == "(Intercept)") == 1)]
#     rownames(mat) = colnames(mat)
#     return(mat) } )
#   # Calculate variance of random effects
#   VarRand = sum(
#     sapply(
#       Sigma.list2,
#       function(Sigma) {
#         Z <- Fmat[,colnames(Sigma),drop=F]
#         sum(diag(Z %*% Sigma %*% t(Z)))/nrow(Fmat) } ) )
#   # Get residual variance
#   VarResid <- as.numeric(lme4::VarCorr(mdl)[rownames(lme4::VarCorr(mdl))=="Residual", 1])
#   # Call the internal function to do the pseudo r-squared calculations
#   .rsquared.glmm(VarF, VarRand, VarResid, VarDisp, family = "gaussian", link = "identity",
#                  mdl.aic = AIC(update(mdl, method="ML")),
#                  mdl.class = class(mdl))
# }

.rsquared.glmm <- function(varF, varRand, varResid = NULL, varDisp = NULL, family, link,
                           mdl.aic, mdl.class, null.fixef = NULL){
  varDist<-0
  if(family == "gaussian"){
    # Only works with identity link
    if(link != "identity")
      family_link.stop(family, link)
    # Calculate marginal R-squared (fixed effects/total variance)
    Rm <- varF/(varF+varRand+varResid)
    # Calculate conditional R-squared (fixed effects+random effects/total variance)
    Rc <- (varF+varRand)/(varF+varRand+varResid)
  }
  else if(family == "binomial"){
    # Get the distribution-specific variance
    if(link == "logit")
      varDist <- (pi^2)/3
    else if(link == "probit")
      varDist <- 1
    else
      family_link.stop(family, link)
    # Calculate marginal R-squared
    Rm <- varF/(varF+varRand+varDist+varDisp)
    # Calculate conditional R-squared (fixed effects+random effects/total variance)
    Rc <- (varF+varRand)/(varF+varRand+varDist+varDisp)
  }
  else if(family == "poisson"){
    # Get the distribution-specific variance
    if(link == "log")
      varDist <- log(1+1/exp(null.fixef))
    else if(link == "sqrt")
      varDist <- 0.25
    else
      family_link.stop(family, link)
    # Calculate marginal R-squared
    Rm <- varF/(varF+varRand+varDist+varDisp)
    # Calculate conditional R-squared (fixed effects+random effects/total variance)
    Rc <- (varF+varRand)/(varF+varRand+varDist+varDisp)
  }
  else
    family_link.stop(family, link)
  
  # Bind R^2s into a matrix and return with AIC values
  data.frame(Class=mdl.class, Family = family, Link = link,
             Marginal=Rm, Conditional=Rc, AIC=mdl.aic, varDist=varDist,varDisp=varDisp)
}

family_link.stop <- function(family, link){
  stop(paste("Don't know how to calculate variance for",
             family, "family and", link, "link."))
}
