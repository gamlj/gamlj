
#' Update a GAMLj model by passing only the new options
#'
#' This function re-estimates a GAMLj model applying new options to the original model
#'
#' @param  gobj of class \code{\link{gamljGLM}},  \code{\link{gamljMixed}}, or  \code{\link{gamljGzlm}} 
#' @param  ... any parameter to be passed to \code{\link{gamljGLM}},  \code{\link{gamljMixed}},  \code{\link{gamljGzlm}}, or  \code{\link{gamljGlmMixed}} 

#' @return an object of class gamlj* as the input object
#' @author Marcello Gallucci
#' @export 

gamlj_update<-function(gobj,...) {
  params<-list(...)
  funs<-list("gamljMixedOptions"=gamlj::gamljMixed,
             "gamljGLMOptions"=gamlj::gamljGLM,
             "gamljGzlmOptions"=gamlj::gamljGzlm)
  cl<-class(gobj$options)[1]
  fun<-funs[[cl]]
  forms<-formals(fun)
  alist<-list()
  for (f in names(forms)) {
    if  (f %in% names(gobj$options)) 
      alist[[f]]<-gobj$options[[f]]
  }
  for (p in names(params)) {
    alist[[p]]<-params[[p]]     
  }
  data<-gobj$options$.getData()
  alist[["data"]]<-data
  do.call(fun,alist)
}

#' Update a GAMLj model plot by passing new plot directives
#'
#' This function re-estimates a GAMLj model with a new plot

#' @param gobj a gamlj results object of the class GAMLj*
#' @param haxis horizontal axis variable
#' @param sepLines variable defining the levels for separate lines 
#' @param sepPlots variable defining the levels for which separate plots are produced 
#' @param ... any other options accepted by the gamlj_* function  
#' @return an object of class GAMLj* as the input object
#' @examples
#' data(qsport)
#' mod<-gamlj::gamljGLM(
#'   formula = performance ~ hours,
#'   data = qsport)
#' 
#' gamlj_plot(mod,haxis = "hours")
#' 
#' @author Marcello Gallucci
#' @export
gamlj_plot<-function(gobj,haxis,sepLines=NULL,sepPlots=NULL,...) {
 args<-list(plotHAxis=haxis,plotSepLines=sepLines,plotSepPlots=sepPlots,...)
 gamlj_update(gobj,args)  
 
}

#' Extract a ggplot2 plot from a GAMLj results object 
#' 
#' This function returns the plot as a ggplot object contained in the gamlj object (the model). If the model 
#' contains one plot, it is returned. If there are more plots, a list is returned with plots as elements of the list.

#' @param gobj a gamlj results object of the class GAMLj*#'
#' @return an object of ggplot
#' @author Marcello Gallucci
#' @export
 
gamlj_ggplot<-function(gobj) {

    if (length(gobj$descPlots)==0)
            return(gobj$descPlot$plot$fun())
     else {
       alist<-list()
       for (i in 1:length(gobj$descPlots)) 
         alist[[i]]<-gobj$descPlots[[i]]$plot$fun()
       return(alist)
     }
          

  
  
}

#' Update a GAMLj results by passing new simple effects directives
#'
#' This function re-estimates a GAMLj model with new simple effects directives
#'
#' @param gobj a gamlj results object of the class GAMLj*
#' @param variable the independent variable name
#' @param moderator the moderator variable name 
#' @param threeway the name of the additional moderator for three way interactions 
#' @param ... any other options accepted by the gamlj_* function  

#' @return an object of class GAMLj* as the input object
#' @author Marcello Gallucci
#' @export 

gamlj_simpleEffects<-function(gobj,variable=NULL,moderator=NULL,threeway=NULL,...) {
  gamlj_update(gobj,simpleVariable=variable,simpleModerator=moderator,simple3way=threeway,...)  
}


#' Update GAMLj results by removing a block or results
#'
#' This function re-estimates a GAMLj model removing some of the directives passed to the orginal model
#'
#' @param gobj a gamlj results object of the class GAMLj*
#' @param analysis which analysis needs to be removed. It can be `simpleEffects`, `emmans` (estimated means for factors),`plots`, or `posthoc`
#'  
#' @return an object of class GAMLj* as the input object
#' @author Marcello Gallucci
#' @export

gamlj_drop<-function(gobj,analysis) {
  if (analysis=="simpleEffects") {
    args<-list(simpleVariable=NULL,simpleModerator=NULL,simple3way=NULL)
  }
  if (analysis=="emeans") {
    args<-list(eDesc=FALSE)
  }
  if (analysis=="plots") {
    args<-list(plotHAxis=NULL)
  }
  if (analysis=="posthoc") {
    args<-list(postHoc=NULL)
  }
  
  gamlj_update(gobj,args)  
}

#' Extract data transformed by GAMLj to run R analyses in line with gamlj setup
#'
#' This function returns a dataset with the variables in the GAMLj model 
#' transformed according to GAMLj options. It is usefull to run additional 
#' models in R with other R packages with the same setup used by GAMLj
#'
#' @param gobj a gamlj results object of the class GAMLj*
#'  
#' @return a dataset
#' @author Marcello Gallucci
#' @examples 
#' data("qsport")
#' obj<-gamlj::gamljGLM(formula = performance ~ hours,
#'                 data = qsport,
#'                 scaling = list(list(var="hours",type="standardized")))
#' 
#' gdata<-gamlj_data(obj)
#' lm(performance ~ hours,data=gdata)
#' @export

gamlj_data<-function(gobj) {
  if (isS4(gobj$model))
     data<-gobj$model@frame
  else
    data<-gobj$model$model
  names(data)<-jmvcore::fromB64(names(data))
  data
}

#' Extract the predicted values of the model estimated by GAMLj 
#'
#' This function returns the predicted values model as a R object. It is usefull to run additional 
#' analysis in R with model fitted scores. 
#'
#' @param gobj a gamlj results object of the class GAMLj*
#' @param re.form if not NULL, specifies the random effect to be included in the computation of the predicted values. Used only for the mixed models.  
#' @param type the type of prediction required. The default is on the scale of the response variables ("response"); Thus for  binomial model the default are the predicted probabilities. The "link" gives the scale of the linear predictors; 
#'             is on the scale of the linear predictors; The "terms" option returns a matrix giving the fitted values of each term in the model formula on the linear predictor scale. 
#'             Cf. \code{\link[stats:predict]{stats::predict()}}, \code{\link[stats:predict.lm]{stats::predict.lm()}}
#' @return a R object of the class of the estimated model
#' @author Marcello Gallucci
#' @examples 
#' data("qsport")
#' obj<-gamlj::gamljGLM(
#'    formula = performance ~ hours,
#'    data = qsport)
#'  preds<-gamlj_predict(obj)
#'  
#' @export

gamlj_predict<-function(gobj,re.form=NULL, type="response") {
  if (!is.null(re.form))
      stats::predict(gobj$model,re.form=re.form, type=type)
  else
    stats::predict(gobj$model,type=type)
}


#' Extract the residuals values of the model estimated by GAMLj 
#'
#' This function returns the residuals of a model as a R object. It is usefull to run additional 
#' analysis in R on model assumptions. 
#' \code{\link[stats:residuals]{stats::predict()}}, \code{\link[stats:residuals.lm]{stats::residuals.lm()}}, \code{\link[stats:residuals.glm]{stats::residuals.glm()}} 
#'
#' @param gobj a gamlj results object of the class GAMLj*
#' @param type the type of the residuals. cf. [stats::residuals()]
#' @return a list of value
#' @author Marcello Gallucci
#' @examples 
#' data("qsport")
#' obj<-gamlj::gamljGLM(
#'    formula = performance ~ hours,
#'    data = qsport)
#'  preds<-gamlj_residuals(obj)
#'  
#' @export

gamlj_residuals<-function(gobj, type="working") {
    stats::residuals(gobj$model)
}

