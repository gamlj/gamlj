
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

update.gamljMixedResults<-function(object,...) {
  fun<-gamljMixed
  .gamlj_update(fun,object,...)
}

update.gamljGlmMixedResults<-function(object,...) {
  fun<-gamljGlmMixed
  .gamlj_update(fun,object,...)
}

#' @rdname update
#' @export 

.gamlj_update<-function(fun,object,...) {

  params<-list(...)
  if (is.null(names(params)))
      params<-params[[1]]
  
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

#' Update a GAMLj model plot by passing new plot directives
#'
#' This function re-estimates a GAMLj model with a new plot. If not options is passed, extracts the 
#' plots present in the `gamlj*Results` object. If one plot is present, it is returned, if more than one is present, 
#' a list of plots is returned. An empty list is returned if no plot is present or defined. 

#' @param object a gamlj results object of the class `gamlj*Results`
#' @param formula a right hand side formula specifying the effect to plot, of the form `~x`, `~x*z` or `~x*z*w`. 
#' It can be combined with the other options but it has prevalence.
#' @param haxis horizontal axis variable
#' @param sepLines variable defining the levels for separate lines 
#' @param sepPlots variable defining the levels for which separate plots are produced 
#' @param ... ignored
#' @return an object of class GAMLj* as the input object
#' @author Marcello Gallucci
#' @examples
#' data(qsport)
#' mod<-gamlj::gamljGlm(
#'   formula = performance ~ hours,
#'   data = qsport)
#' 
#' plot(mod,haxis = "hours")
#' plot(mod,formula=~hours)
#' @rdname plot
#' @export

plot.gamljGlmResults<-function(object,formula=NULL,haxis=NULL,sepLines=NULL,sepPlots=NULL,...) {

  if (is.something(formula)) {
    .vars<-all.vars(formula)
    haxis<-.vars[1]
    if (!is.na(.vars[2])) sepLines<-.vars[2] 
    if (!is.na(.vars[3])) sepPlots<-.vars[3] 
    
  }
  if (is.something(haxis)) {
      args<-list(plotHAxis=haxis,plotSepLines=sepLines,plotSepPlots=sepPlots)
      object<-update(object,args)
  }
 .gamlj_ggplot(object)
}


.gamlj_ggplot<-function(object) {

    if (length(object$descPlots)==0)
            return(object$descPlot$plot$fun())
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

#' Update a GAMLj results by passing new simple effects directives
#'
#' This function re-estimates a GAMLj model with new simple effects directives
#'
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
#' obj<-gamlj::gamljGlm(formula = performance ~ hours,
#'                 data = qsport,
#'                 scaling = c(hours="standardized"))
#' 
#' gdata<-gamlj_data(obj)
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
#' obj<-gamlj::gamljGlm(
#'    formula = performance ~ hours,
#'    data = qsport)
#'  preds<-predict(obj)
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
#' obj<-gamlj::gamljGlm(
#'    formula = performance ~ hours,
#'    data = qsport)
#'  preds<-gamlj_predict(obj)
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
#' @rdname residuals
#' @param gobj a gamlj results object of the class `gamlj*Results`
#' @param type the type of the residuals for generalized models. The alternatives are: "deviance" (default), "pearson", "working", "response", and "partial". Can be abbreviated., 
#' cf.  \code{\link[stats:residuals.lm]{stats::residuals.lm()}}
#' @return a list of value
#' @author Marcello Gallucci
#' @examples 
#' data("qsport")
#' obj<-gamlj::gamljGlm(
#'    formula = performance ~ hours,
#'    data = qsport)
#'  preds<-gamlj_residuals(obj)
#'  
#' @export

gamlj_residuals<-function(gobj, type="response") {
    stats::residuals(gobj$model,type=type)
}


#' Residdials values from GAMLj models 
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
#' obj<-gamlj::gamljGlm(
#'    formula = performance ~ hours,
#'    data = qsport)
#'  preds<-residuals(obj)
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



