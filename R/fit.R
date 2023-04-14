############# produces R2  ##########
## an object to obtain R2 and fit indices

gFit <- R6::R6Class(
  "gFit",
  class=TRUE, ## this and the next 
  cloneable=FALSE, ## should improve performance https://r6.r-lib.org/articles/Performance.html ###
  public = list(
    operator=NULL,
    initialize=function(operator) {
      self$operator<-operator 
    },
    r2table=function() {
      
      tab <- private$.r2list()
      tab <-c(tab,private$.r2nested())
      tab <-c(tab,private$.compare())
      return(tab)
    }

  ), ### end of public
  private=list(
     .r2=NULL,
     .ar2=NULL,
     .r2n=NULL,
     .ar2n=NULL,
     
    .r2list=function() {
      
      obj<-try_hard(r2(self$operator$model,self$operator))
      
      if (!isFALSE(obj$error)) {
        self$operator$warning<-list(topic="main_r2",message="Model R2 cannot be computed.")
        return(obj$obj)
      }
      if (!isFALSE(obj$warning)) {
        self$operator$warning<-list(topic="main_r2",message=obj$warning)
      }

      if (self$operator$options$comparison) {
           r2list <- lapply(obj$obj, function(x) {
                               x$note <- "R^2 of the full model"
                               x$model <- "Full"
                               x
                      })
        } else {
           r2list <- lapply(obj$obj, function(a) {
                           a$note <- "R^2"
                           a
                        })
        }
      
      
      private$.r2<-r2list[[1]]$r2
      private$.ar2<-r2list[[1]]$ar2
      return(r2list)
      
    }, # end of .r2list
    .r2nested=function() {
      
      if (!self$operator$options$comparison) 
           return(NULL)
        
      obj<-try_hard(r2(self$operator$nested_model,self$operator))
      if (!isFALSE(obj$error)) {
        self$operator$warning<-list(topic="main_r2",message="Nested model R2 cannot be computed.")
        return(obj$obj)
      }
      if (!isFALSE(obj$warning)) {
        self$operator$warning<-list(topic="main_r2",message=obj$warning)
      }
      
        r2list <- lapply(obj$obj, function(x) {
          x$note <- "R^2 of the nested model"
          x$model <- "Nested"
          x
        })
        
        private$.r2n<-r2list[[1]]$r2
        private$.ar2n<-r2list[[1]]$ar2
        
        return(r2list)
        
    }, # end of .r2list
    
    .compare=function() {
      
      if (!self$operator$options$comparison)
          return()
      if (self$operator$option(".caller", c("glmer")) & self$operator$option("model_type", c("multinomial"))) {
           self$operator$warning <- list(topic = "main_r2",
                                                     message = "Inferential test for multinomial models
                                                                comparison not available. 
                                                                Deviances from quasi-likelihoods are not comparable.")
           return(NULL)
      }

      omnibus<-"Chisq"
      if (self$operator$option("omnibus")) omnibus<-self$operator$optionValue("omnibus")
      
      if (self$operator$option(".caller", c("lmer", "glmer")) || self$operator$option("omnibus", "LRT")) {
         comp <- try_hard(as.data.frame(performance::test_likelihoodratio(self$operator$nested_model, self$operator$model)))
      } else {
         comp <- try_hard(stats::anova(self$operator$nested_model, self$operator$model, test = omnibus))
      }
      
      comp <- comp$obj
      r2comp <- as.list(comp[2, ])
      .names <- list(
                     df2 = c("Res.Df", "Resid. Df"),
                     df1 = c("Df", "df_diff","df"), p = c("Pr(>Chi)", "Pr(>F)","Pr(Chi)"),
                     f = "F",
                     test = c("Deviance", "Chi2","LR stat.")
                    )
      
      names(r2comp) <- transnames(names(r2comp), .names)

      r2comp$model <- paste0(greek_vector[["Delta"]], "R", "\u00B2")
      r2comp$type <- "Comparison"
      ### give some warning
      if (r2comp$df1 < 0) 
        self$operator$warning <- list(topic = "main_r2", message = "Nested model is not actually nested in the full model ")
      
      
      if (r2comp$df1 == 0) 
        self$operator$warning <- list(topic = "main_r2", message = "Nested and full models are identical, try removing some term from the nested model")
      
      
      r2comp$r2 <- private$.r2-private$.r2n
      if (length(r2comp$r2) == 0)  r2comp$r2 <- NA
      
      r2comp$note <- "R^2 difference "
      if (is.something(private$.ar2)) r2comp$ar2 <- private$.ar2-private$.ar2n
      if (length(r2comp$ar2) == 0)  r2comp$ar2 <- NA
      

    return(list(r2comp))
    } # end of compare
  ) # end of private
) ### end of class
    
### functions to compute R2

r2 <- function(model, ...) UseMethod(".r2")

.r2.default <- function(model,obj) {
  
  performance::r2(model, tolerance = 0)
  
}

.r2.lm <- function(model,obj) {
  
  ss <- summary(model)
  results <- list()
  results$df1 <- ss$fstatistic[["numdf"]]
  results$df2 <- ss$fstatistic[["dendf"]]
  results$r2 <- ss$r.squared
  results$ar2 <- ss$adj.r.squared
  if (utils::hasName(ss, "fstatistic")) {
    if (obj$option("omnibus", "LRT")) {
      ssres <- stats::sigma(model)^2 * model$df.residual
      ### here we estimate the sum of squares of the null model
      ssnull <- (ss$fstatistic[[1]] * ss$fstatistic[[2]] * ssres / ss$fstatistic[[3]]) + ssres
      n <- sum(ss$df[1:2])
      ### compute the loglik of the null model
      loglik0 <- -0.5 * n * (log(2 * pi) + log(ssnull / n) + 1)
      loglik1 <- as.numeric(stats::logLik(model))
      results$lrt <- 2 * (loglik1 - loglik0)
      results$test <- results$lrt
      results$p <- stats::pchisq(results$lrt, results$df1, lower.tail = FALSE)
    } else {
      results$f <- ss$fstatistic[["value"]]
      results$test <- results$f
      results$p <- stats::pf(results$f, results$df1, results$df2, lower.tail = FALSE)
    }
  } else {
    results$test <- NA
  }
  
  list(results)
}



.r2.glm <- function(model,obj) {
  
  jinfo(" .glm R2 is used for object of class",paste(class(model)))
  
  alist <- list()
  # mcFadden and adjusted
  alist$r2 <- 1 - (model$deviance / model$null.deviance)
  alist$ar2 <- 1 - ((model$deviance + 2 * length(model$coefficients)) / model$null.deviance)

  if (alist$ar2 < 0) {
    alist$ar2 <- 0
  }
  results <- fit.compare_null_model(model)
  alist$test <- results$test
  alist$df1 <- results$df1
  alist$p <- results$p
  alist$type <- "R-squared"
  list(alist)
}




.r2.polr <- function(model, obj) {
  
  jinfo(".polr R2 is used")
  alist <- list()
  results <- fit.compare_null_model(model)

  # mcFadden

  alist$r2 <- 1 - (results$deviance / results$null.deviance)
  alist$test <- results$test
  alist$df1 <- results$df1
  alist$p <- results$p

  return(list(alist))
}

.r2.clm <- function(model, obj) {
  .r2.polr(model,obj)
}




.r2.multinom <- function(model, obj) {
  llfull <- stats::logLik(model) ### model loglikelihood
  data <- insight::get_data(model)
  nullmodel <- stats::update(model, ~1, data = data, evaluate = T)
  #  nullmodel <- eval.parent(nullmodel)
  llnull <- stats::logLik(nullmodel)
  r2 <- as.numeric(1 - (llfull / llnull))
  compare <- stats::anova(nullmodel, model)
  alist <- list()
  # mcFadden
  alist$r2 <- as.numeric(1 - (llfull / llnull))
  alist$test <- compare$`LR stat.`[2]
  alist$df1 <- compare$`   Df`[2]
  alist$p <- compare$`Pr(Chi)`[2]

  return(list(alist))
}

.r2.mblogit <- function(model, obj) {
  jinfo("mblogit R2 is used")
  llfull <- model$deviance
  llnull <- model$null.deviance
  ss <- mclogit::getSummary.mmblogit(model)
  alist <- list()
  # mcFadden
  alist$r2 <- as.numeric(1 - (llfull / llnull))
  alist$test <- ss$sumstat[1]
  alist$df1 <- ss$sumstat[2]
  alist$p <- pchisq(alist$test, df = alist$df1, lower.tail = FALSE)
  list(alist)
}


.r2.lmerModLmerTest <- function(model, obj) {

  r2 <- .r2.default(model, obj)
  if (is.null(r2) || is.na(r2)) {
    r2 <- list(R2_conditional = NA, R2_marginal = NA)
  }

  cond <- fit.compare_null_model(model, type = "c")
  cond$type <- "Conditional"
  cond$r2 <- r2$R2_conditional
  marg <- fit.compare_null_model(model, type = "m")
  if (is.null(marg)) {
    marg <- list()
  }

  marg$type <- "Marginal"
  marg$r2 <- r2$R2_marginal
  list(cond, marg)
}


.r2.glmerMod <- function(model, obj) {
  .r2.lmerModLmerTest(model, obj)
}

.r2.clmm <- function(model, obj) {
  .r2.lmerModLmerTest(model, obj)
}

######### model comparisons for one model ########

fit.compare_null_model <- function(x, ...) UseMethod(".compare_null_model")


.compare_null_model.default <- function(model) {
  
  jinfo("Default null model comparison is used for object of class ",paste(class(model),collapse = " "))
  
  data <- insight::get_data(model)
  int <- attr(stats::terms(model), "intercept")
  form <- stats::as.formula(paste("~", int))
  model0 <- stats::update(model, form, data = data, evaluate = T)
  results <- try_hard(stats::anova(model0, model, test = "LRT"))
  results <- results$obj

 .names<-c(test=c("Deviance","LR.stat","LR stat.",""),
           df1=c("Df","   Df","df"),
           p=c("Pr(Chi)","Pr(>Chisq)","Pr(>Chi)"))
  names(results)<-transnames(names(results),.names)
  results$deviance <- stats::deviance(model)
  results$null.deviance <- stats::deviance(model0) 
  results[2, ]
}




.compare_null_model.lmerModLmerTest <- function(model, type = "c") {
  data <- insight::get_data(model)

  int <- attr(stats::terms(model), "intercept")


  if (type == "c") {
    form <- stats::as.formula(paste(stats::formula(model)[[2]], "~", int))
    model0 <- stats::lm(form, data = data)
  } else {
    if (int == 0) {
      return(NULL)
    }

    re <- lme4::findbars(stats::formula(model))
    re <- paste("(", re, ")", collapse = "+")
    dep <- insight::model_info(model)$model_terms$response
    form <- paste(dep, "~", int, " + ", re)
    model0 <- stats::update(model, formula = form)
  }
  ### here we use performance::test_likelihoodratio, which compute the LRT
  ### on the estimated models, no matter what REML is. If one compares the results with
  ### lmerTest::anova() they are slightly different because the latter re-estimate the models
  ### with ML, not REML. We do not see why re-estimaing is necessary, given these results: .https://www.jstor.org/stable/2533680

  results <- as.data.frame(performance::test_likelihoodratio(model0, model))
  names(results) <- c("nothing1", "nothing2", "nothing3", "df1", "test", "p")
  results[2, c("df1", "test", "p")]
}

.compare_null_model.glmerMod <- function(model, type = "c") {
  data <- insight::get_data(model)

  int <- attr(stats::terms(model), "intercept")


  if (type == "c") {
    form <- stats::as.formula(paste(stats::formula(model)[[2]], "~", int))
    model0 <- stats::glm(form, data = data, family = insight::get_family(model))
  } else {
    if (int == 0) {
      return(NULL)
    }

    re <- lme4::findbars(stats::formula(model))
    re <- paste("(", re, ")", collapse = "+")
    dep <- insight::find_response(model)
    form <- paste(dep, "~", int, " + ", re)
    model0 <- stats::update(model, formula = form)
  }
  ### please note that here we use performance::test_likelihoodratio, which compute the LRT
  ### on the estimated models, no matter what REML is. If one compares the results with
  ### lmerTest::anova() they are slightly different because the latter re-estimate the models
  ### with ML, not REML. We do not see why re-estimaing is necessary, given these results: .https://www.jstor.org/stable/2533680

  results <- as.data.frame(performance::test_likelihoodratio(model0, model))
  names(results) <- c("nothing1", "nothing2", "nothing3", "df1", "test", "p")
  results[2, c("df1", "test", "p")]
}


.compare_null_model.clmm <- function(model, type = "c") {
  data <- insight::get_data(model)

  int <- attr(stats::terms(model), "intercept")
  dep <- insight::find_response(model)

  if (type == "c") {
    int <- 1
    form <- stats::as.formula(paste(dep, "~", int))
    model0 <- ordinal::clm(form, data = data)
  } else {
    if (int == 0) {
      return(NULL)
    }

    re <- lme4::findbars(stats::formula(model))
    re <- paste("(", re, ")", collapse = "+")
    form <- as.formula(paste(dep, "~", int, " + ", re))
    model0 <- ordinal::clmm(formula = form, data = data)
  }
  ### here we use performance::test_likelihoodratio, which compute the LRT
  ### on the estimated models, no matter what REML is. If one compares the results with
  ### lmerTest::anova() they are slightly different because the latter re-estimate the models
  ### with ML, not REML. We do not see why re-estimaing is necessary, given these results: .https://www.jstor.org/stable/2533680

  results <- as.data.frame(performance::test_likelihoodratio(model0, model))
  names(results) <- c("nothing1", "nothing2", "nothing3", "df1", "test", "p")
  results[2, c("df1", "test", "p")]
}

.compare_null_model.polr <- function(model) {
  
  jinfo(".polr null model comparison is used")
  
  data <- insight::get_data(model)
  int <- attr(stats::terms(model), "intercept")
  form <- stats::as.formula(paste("~", int))
  model0 <- stats::update(model, form, data = data, evaluate = T)
  results <- stats::anova(model0, model, test = "Chisq")
  .names<-c(test=c("Deviance","LR.stat","LR stat."),df1=c("Df","   Df"),p=c("Pr(Chi)","Pr(>Chisq)"))
  names(results)<-transnames(names(results),.names)
  results$deviance <- stats::deviance(model)
  results$null.deviance <- stats::deviance(model0) 
  results[2, ]
}



null.deviance <- function(model) {
  int <- attr(stats::terms(model), "intercept")
  form <- stats::as.formula(paste("~", int))
  model0 <- stats::update(model, form, evaluate = T)
  stats::deviance(model0)
}


deviance <- function(object, ...) UseMethod("stats::deviance")

deviance.clm <- function(object) as.numeric(-2 * stats::logLik(object))

#### additional fit indices

fit.indices <- function(model, ...) UseMethod(".fit")

.fit.default <- function(model, obj) {
  return(NULL)
}

.fit.glm <- function(model, obj) {
  alist <- list()
  alist[[length(alist) + 1]] <- list(value = as.numeric(stats::logLik(model)))
  alist[[length(alist) + 1]] <- list(value = stats::extractAIC(model)[2])
  alist[[length(alist) + 1]] <- list(value = stats::BIC(model))
  alist[[length(alist) + 1]] <- list(value = model$deviance)
  alist[[length(alist) + 1]] <- list(value = model$df.residual)
  value <- sum(stats::residuals(model, type = "pearson")^2)
  result <- value / stats::df.residual(model)
  alist[[length(alist) + 1]] <- list(value = result)
  alist
}

.fit.multinom <- function(model, obj) {
  alist <- list()
  alist[[length(alist) + 1]] <- list(value = as.numeric(stats::logLik(model)))
  alist[[length(alist) + 1]] <- list(value = stats::extractAIC(model)[2])
  alist[[length(alist) + 1]] <- list(value = stats::BIC(model))
  alist[[length(alist) + 1]] <- list(value = model$deviance)
  alist
}
