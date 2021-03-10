
#' Update a GAMLj model by passing only the new options
#'
#' Re-estimates a GAMLj model applying new options to the original model
#'
#' @param  object of class `gamlj*Results` 
#' @param  ... any parameter to be passed to \code{\link{gamljGlm}},  \code{\link{gamljMixed}},  \code{\link{gamljGzlm}}, or  \code{\link{gamljGlmMixed}} 
#' @return an object of class gamlj*Results as the input object
#' @author Marcello Gallucci
#' @rdname update
#' @export 

update.gamljGlmResults<-function(object,...) {
  fun<-gamljGlm
  .gamlj_update(fun,object,...)
}

#' @rdname update
#' @export 

update.gamljGzlmResults<-function(object,...) {
  fun<-gamljGzlm
  .gamlj_update(fun,object,...)
}

#' @rdname update
#' @export 

update.gamljMixedResults<-function(object,...) {
  fun<-gamljMixed
  .gamlj_update(fun,object,...)
}

#' @rdname update
#' @export 

update.gamljGlmMixedResults<-function(object,...) {
  fun<-gamljGlmMixed
  .gamlj_update(fun,object,...)
}

.gamlj_update<-function(fun,object,...) {

  params<-list(...)
  if (is.null(names(params)))
      params<-params[[1]]
  mark(params)
  
  forms<-formals(fun)
  alist<-list()
  for (f in names(forms)) {
    if  (f %in% names(object$options)) 
      alist[[f]]<-object$options[[f]]
  }
  for (p in names(params)) {
    alist[[p]]<-params[[p]]     
  }
  data<-object$options$.getData()
  alist[["data"]]<-data
  do.call(fun,alist)
}

#'  GAMLj plot
#'
#' This function re-estimates a GAMLj model adding a new plot. If no options is passed, extracts the 
#' plots present in the `gamlj*Results` object. If one plot is present, it is returned as a ggplot2 object,
#'  if more than one is present, a list of plots is returned. FALSE is returned if no plot is present or defined. 

#' @param x a gamlj results object of the class `gamlj`
#' @param formula a right hand side formula specifying the effect to plot, of the form `~x`, `~x*z` or `~x*z*w`. 
#' It has prevalence on other options defining a plot.
#' @param ... all options accepted by a gamlj model function. Relevant for new plots are `plotHAxis`, `plotSepLines` and `plotSepPlots`
#' @return an object of class ggplot or a list of ggplot objects
#' @author Marcello Gallucci
#' @examples
#' data(qsport)
#' gmod<-gamlj::gamljGlm(
#'   formula = performance ~ hours,
#'   data = qsport)
#' 
#' plot(gmod,plotHAxis = "hours")
#' plot(gmod,formula=~hours)
#' @rdname plot
#' @export

plot.gamlj<-function(x,formula=NULL,...) {

   if (is.something(formula)) {
     .calls<-jmvcore::marshalFormula(formula,from='rhs',type='terms',data=NULL)
     plots<-lapply(.calls, function(.call) {
       haxis<-.call[1]
       if (!is.na(.call[2])) sepLines<-.call[2] else sepLines<-NULL 
       if (!is.na(.call[3])) sepPlots<-.call[3] else sepPlots<-NULL
       args<-list(plotHAxis=haxis,plotSepLines=sepLines,plotSepPlots=sepPlots,...)
       object<-stats::update(x,args)
       .extract_plots(object)
      }) 
     }  else {
        if (is.something(list(...))) 
              x<-stats::update(x,...)
        
      plots<-.extract_plots(x)
     }
   if ("list" %in% class(plots) && length(plots)==1)
       plots<-plots[[1]]
    plots
   }


.extract_plots<-function(object) {
  
    if (length(object$descPlots)==0) {
          return(object$descPlot$plot$fun())
    }
     else {
       alist<-list()
       for (i in 1:length(object$descPlots)) {
         title<-(object$descPlots[[i]]$title)
         gplot<-object$descPlots[[i]]$plot$fun()+ggplot2::ggtitle(title)
         alist[[i]]<-gplot
       }
       return(alist)
     }
}
  
  #' Extract assumptions checking plots from a GAMLj results object 
  #' 
  #' This function returns a list of plots as a ggplot objects produced by the assumptions checking options, such
  #' as `residPlot`, `normPlot` (for continuous dependent variable models), `randHist` and `clusterBoxplot` (for mixed models).
  #' @param object a gamlj results object of the class `gamlj*Results``
  #' @return a list of lists with titles and ggplot objects
  #' @author Marcello Gallucci
  #' @export
  
gamlj_assumptionsPlots<-function(object) {

    if (!("assumptions" %in% names(object)))
       stop("No assumptions checking plot is contained in the GAMLj model")
  
    pnames<-names(object$assumptions)
    if (!is.something(pnames))
       stop("No assumptions checking plots is contained in the GAMLj model")
    res<-list()
    for (pname in pnames) {
      obj<-object$assumptions[[pname]]
      if ("Image" %in% class(obj)) {
        plot<-obj$plot$fun()
        if (is.something(plot)) {
          title<-obj$title
          name<-obj$name
          plot<-obj$plot$fun()
          res[[length(res)+1]]<-list(name=name,title=title,plot=plot)
        }
      }
      if ("Array" %in% class(obj)) {
        groupname<-obj$name
        j<-0
        for (key in obj$itemKeys) {
          j<-j+1
          one<-obj$get(key=key)
          title<-one$title
          name<-paste0(groupname,j)
          plot<-one$plot$fun()
          res[[length(res)+1]]<-list(name=name,title=title,plot=plot)
        }
      }
      
    }
    
    res
  
}

#' Deprecated simple effects
#'
#' Deprecated, please use `simpleffects`
#' #'
#' @param object a gamlj results object of the class `gamlj*Results`
#' @param variable the independent variable name
#' @param moderator the moderator variable name 
#' @param threeway the name of the additional moderator for three way interactions 
#' @param ... any other options accepted by the gamlj_* function  

#' @return Two tables with simple effects parameters estimates and ANOVA tables 
#' @author Marcello Gallucci
#' @export 

gamlj_simpleEffects<-function(object,variable=NULL,moderator=NULL,threeway=NULL,...) {
  mod<-stats::update(object,simpleVariable=variable,simpleModerator=moderator,simple3way=threeway,...)  
  mod$simpleEffects
}



#' Extract data transformed by GAMLj to run R analyses in line with gamlj setup
#'
#' This function returns a dataset with the variables in the GAMLj model 
#' transformed according to GAMLj options. It is usefull to run additional 
#' models in R with other R packages with the same setup used by GAMLj
#'
#' @param object a gamlj results object of the class `gamlj*Results`
#'  
#' @return a dataset
#' @author Marcello Gallucci
#' @examples 
#' data("qsport")
#' gmod<-gamlj::gamljGlm(formula = performance ~ hours,
#'                 data = qsport,
#'                 scaling = c(hours="standardized"))
#' 
#' gdata<-gamlj_data(gmod)
#' lm(performance ~ hours,data=gdata)
#' @export

gamlj_data<-function(object) {
  if (isS4(object$model))
     data<-object$model@frame
  else
    data<-object$model$model
  names(data)<-jmvcore::fromB64(names(data))
  data
}

#' Predicted values from GAMLj models 
#'
#' Returns predicted values from the estimated model
#' 
#' @name predict
#' @rdname predict
#' @aliases predict.gamljGlmResults
#' @param object a gamlj results object of the class `gamlj*Results`
#' @param re.form (formula, NULL, or NA) specify which random effects to condition on when predicting. If NULL, include all random effects; if NA or ~0, include no random effects. Used only for the mixed models.  
#' @param type the type of prediction required. The default is on the scale of the response variables ("response"); Thus for  binomial models the default is to compute the predicted probabilities.  "link" gives the scale of the linear predictors; 
#'             is on the scale of the linear predictors;  The "terms" option returns a matrix giving the fitted values of each term in the model formula on the linear predictor scale. 
#'             Cf. \code{\link[stats:predict]{stats::predict()}}, \code{\link[stats:predict.lm]{stats::predict.lm()}}
#' @param ...  additional arguments for specific predict methods other than the ones specified here.
#' @return a R object of the class of the estimated model
#' @author Marcello Gallucci
#' @examples 
#' data("qsport")
#' gmod<-gamlj::gamljGlm(
#'    formula = performance ~ hours,
#'    data = qsport)
#'  preds<-predict(gmod)
#'  
#' @export

predict.gamljGlmResults<-function(object,...) {
    stats::predict(object$model,...)
}


#'  preds<-predict(obj)
#' @rdname predict  
#' @export

predict.gamljGzlmResults<-function(object, type="response", ...) {
  stats::predict(object$model,type=type,...)
}

#' @rdname predict  
#' @export


predict.gamljMixedResults<-function(object, re.form=NULL, type="response", ...) {
  stats::predict(object$model,re.form=re.form, type=type,...)
}

#' @rdname predict  
#' @export

predict.gamljGlmMixedResults<-function(object, re.form=NULL, type="response", ...) {
  stats::predict(object$model,re.form=re.form,type=type,...)
}

#' Predicted values from GAMLj models 
#'
#' Deprecated. Please use `predict()`

#' @param gobj a gamlj results object of the class `gamlj*Results`
#' @param re.form if not NULL, specifies the random effect to be included in the computation of the predicted values. Used only for the mixed models.  
#' @param type the type of prediction required. The default is on the scale of the response variables ("response"); Thus for  binomial models the default is to compute the predicted probabilities.  "link" gives the scale of the linear predictors; 
#'             is on the scale of the linear predictors;  The "terms" option returns a matrix giving the fitted values of each term in the model formula on the linear predictor scale. 
#'             Cf. \code{\link[stats:predict]{stats::predict()}}, \code{\link[stats:predict.lm]{stats::predict.lm()}}
#' @return a R object of the class of the estimated model
#' @author Marcello Gallucci
#' @examples 
#' data("qsport")
#' gmod<-gamlj::gamljGlm(
#'    formula = performance ~ hours,
#'    data = qsport)
#'  preds<-gamlj_predict(gmod)
#'  
#' @export

gamlj_predict<-function(gobj,re.form=NULL, type="response") {
  warning("Deprecated. Please use `predict()`")
  if (!is.null(re.form))
    stats::predict(gobj$model,re.form=re.form, type=type)
  else
    stats::predict(gobj$model,type=type)
}



#' Residuals values of a GAMLj model 
#'
#' Deprecated. Please use `residuals()`
#' 
#' \code{\link[stats:residuals]{stats::predict()}}, \code{\link[stats:residuals.lm]{stats::residuals.lm()}}, \code{\link[stats:residuals.glm]{stats::residuals.glm()}} 
#'
#' @param gobj a gamlj results object of the class `gamlj*Results`
#' @param type the type of the residuals for generalized models. The alternatives are: "deviance" (default), "pearson", "working", "response", and "partial". Can be abbreviated., 
#' cf.  \code{\link[stats:residuals.lm]{stats::residuals.lm()}}
#' @return a list of value
#' @author Marcello Gallucci
#' @examples 
#' data("qsport")
#' gmod<-gamlj::gamljGlm(
#'    formula = performance ~ hours,
#'    data = qsport)
#'  preds<-gamlj_residuals(gmod)
#'  
#' @export

gamlj_residuals<-function(gobj, type="response") {
    stats::residuals(gobj$model,type=type)
}


#' Residuals values from GAMLj models 
#'
#' Returns residuals values from the estimated model
#' 
#' @name residuals
#' @rdname residuals
#' @aliases residuals.gamljGlmResults
#' @param object a gamlj results object of the class `gamlj*Results`
#' @param type the type of residuals for generalized models. The alternatives are: "deviance" (default), "pearson", "working", "response", and "partial". Can be abbreviated.  
#'             Cf. \code{\link[stats:residuals]{stats::residuals()}}, \code{\link[stats:residuals.lm]{stats::residuals.lm()}}, \code{\link[stats:residuals.glm]{stats::residuals.glm()}}
#' @param ...  additional arguments for specific residuals methods.
#' @return a R object of the class of the estimated model
#' @author Marcello Gallucci
#' @examples 
#' data("qsport")
#' gmod<-gamlj::gamljGlm(
#'    formula = performance ~ hours,
#'    data = qsport)
#'  preds<-residuals(gmod)
#'  
#' @export

residuals.gamljGlmResults<-function(object,...) {
  stats::residuals(object$model,...)
}

#' @rdname residuals  
#' @export

residuals.gamljGzlmResults<-function(object, type="deviance", ...) {
  stats::residuals(object$model,type=type,...)
}

#' @rdname residuals  
#' @export


residuals.gamljMixedResults<-function(object, ...) {
  stats::residuals(object$model,...)
}

#' @rdname residuals  
#' @export

residuals.gamljGlmMixedResults<-function(object, type="deviance", ...) {
  stats::residuals(object$model,type=type,...)
}




#' Extract the R model object contained in the GAMLj results 
#'
#' Return the estimated model object of the class lm, lmer, glm depending on the model.
#'
#' @param object a gamlj results object of the class `gamlj*Results`
#' @return a R object from lm(), glm(), lmer() or glmer()
#' @author Marcello Gallucci
#' @examples 
#' data("qsport")
#' obj<-gamlj::gamljGlm(
#'    formula = performance ~ hours,
#'    data = qsport)
#'  model<-gamlj_model(obj)
#'  
#' @export

gamlj_model<-function(object) {

  rf<-attr(object$model,"refit")
  command<-rf$command
  if (!is.null(rf$lib))
      command<-paste0(rf$lib,"::",rf$command)
  rf$eoptions[["data"]]<-gamlj_data(object)
  fun<-getfun(command)
  model<-do.call(fun,rf$eoptions)
  model<-mf.setModelCall(model,rf)
  model  
}



#'  Post-hoc test on GAMLj results 
#'
#' This is a convenience function to re-estimates a GAMLj model adding posthoc tests. If no options is passed, extracts the 
#' post-hoc tests tables already in the model results (if any). If new post-hoc are defined, the post-hoc tests tables 
#' are returned.

#' @param object a gamlj results object of the class `gamlj`
#' @param formula a right hand side formula specifying the factors or factors combinations to test, of the form `~x+z`, `~x:z` or `~x*z`. 
#' It has prevalence on other options defining a post-hoc test via character options.
#' @param ... all options accepted by a gamlj model function. Relevant for new tests are 
#'   `postHoc` (a list of list of terms), `postHocCorr`, a list of correction to apply:
#'    one or more of 'none', 'bonf', or 'holm'; provide no, Bonferroni, and Holm Post Hoc corrections respectively. 
#' @return an object of class ggplot or a list of ggplot objects
#' @author Marcello Gallucci
#' @examples
#' data(fivegroups)
#' fivegroups$Group<-factor(fivegroups$Group)
#' gmod<-gamlj::gamljGlm(
#'   formula = Score ~Group,
#'   data = fivegroups)
#' 
#' posthoc(gmod,formula =~Group)
#' @rdname posthoc
#' @export

posthoc<-function(object,...) UseMethod("posthoc")

#' @rdname posthoc
#' @export

posthoc.gamlj<-function(object,formula=NULL,...) {

    if (is.something(formula))
        object<-stats::update(object,postHoc=formula,...)
    else
       if (is.something(list(...)))
          object<-stats::update(object,...)
    if (length(object$postHocs)==0)
         return(FALSE)
       
    object$postHocs
}
  
 
#'  Simple Effects on GAMLj results 
#'
#' This is a convenience function to re-estimates a GAMLj model adding simple effect analysis. If no options is passed, extracts the 
#' simple effects tables already in the model results (if any). If new tests are defined, the simple effects tests tables 
#' are returned.

#' @param object a gamlj results object of the class `gamlj`
#' @param formula a right hand side formula specifying the variables to test, of the form `~x:z`, `~x:z:w` or `~x*z`. 
#' The formula is not exanded, so the first variable is the simple effect variable, the second is the moderator, 
#' the third an optional additional moderator
#' It has prevalence on other options defining a simple effects test via character options.
#' @param ... all options accepted by a gamlj model function. Relevant for new tests are 
#'   `simpleVariable` (the simple effect variable), `simpleModerator` (the moderator), and `simple3way` for the second moderator. 
#' @return an object of class gamlj results
#' @author Marcello Gallucci
#' @examples
#' data(winkel)
#' wicksell$time<-factor(wicksell$time)
#' wicksell$group<-factor(wicksell$group) 
#' gmod<-gamlj::gamljMixed(
#'    formula = dv ~ 1 +group+ time:group+ time+( 1 | subj ),
#'    data = wicksell)
#' 
#' simpleEffects(gmod,formula =~time:group)
#' @rdname simpleeffects
#' @export

simpleEffects<-function(object,...) UseMethod("simpleEffects")

#' @rdname simpleeffects
#' @export

simpleEffects.gamlj<-function(object,formula=NULL,...) {

  if (is.something(formula)) {
       .call<-all.vars(formula)
       .simpleVariable<-.call[1]
       .simpleModerator<-.call[2] 
        if (!is.na(.call[3])) .simple3way<-.call[3] else .simple3way<-NULL
        args<-list(simpleVariable=.simpleVariable,simpleModerator=.simpleModerator,simple3way=.simple3way,...)
        object<-stats::update(object,args)
  }  else
        if (is.something(list(...)))
             object<-stats::update(object,...)

     if (dim(object$simpleEffects$Anova$asDF)[1]==0)
           return(FALSE)

     object$simpleEffects
}


