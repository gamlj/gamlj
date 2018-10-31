#' GAMLj models options
#'
#' This function sets GAMLj suite general options
#'
#' @param opt a value for the corresponding option 
#' @details 
#' Options are: debug (TRUE or FALSE). When TRUE some additional warnings and info are printed in output
#'     
#' @author Marcello Gallucci
gamlj_options<-function(opt,value) {
  if (opt=="debug")
    GAMLj_DEBUG<<-value
}

#' Update a GAMLj model by passing only the new options
#'
#' This function re-estimates a GAMLj model applying new options to the original model
#'
#' @param a single obj of class \code{\link{gamljGLM}},  \code{\link{gamljMixed}}, or  \code{\link{gamljGZLM}} 
#' @return an object of class GAMLj* as the input object
#' @author Marcello Gallucci
#' @export 

gamlj_update<-function(gobj,...) {
  params<-as.list(c(...))
  funs<-list("gamljMixedOptions"=gamlj::gamljMixed,
             "gamljGLMOptions"=gamlj::gamljGLM)
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

#' Update a GAMLj model plots by passing new plots directives
#'
#' This function re-estimates a GAMLj model with a new plot

#' @param gobj a gamlj results object of the class GAMLj*#'
#' @param haxis horizontal axis variable
#' @param sepLines variable defining the levels for separate lines 
#' @param sepPlot variable defining the levels for which separate plots are produced 
#' @param ... any other options accepted by the gamlj_* function  
#' @return an object of class GAMLj* as the input object
#' @author Marcello Gallucci
#' @export
gamlj_plot<-function(gobj,haxis,sepLines=NULL,sepPlots=NULL,...) {
 news<-list(plotHAxis=haxis,plotSepLines=sepLines,plotSepPlots=sepPlots,...)
 gamlj_update(gobj,args)  
 
}

#' This function returns the plot as a ggplot object

#' @param gobj a gamlj results object of the class GAMLj*#'
#' @param haxis horizontal axis variable
#' @param sepLines variable defining the levels for separate lines 
#' @param sepPlot variable defining the levels for which separate plots are produced 
#' @param theme a theme for the ggplot plot. Default is the jamovi default theme
#' @param ... any other options accepted by the gamlj_* function  
#' @return an object of class GAMLj* as the input object
#' @author Marcello Gallucci
#' @export
gamlj_ggplot<-function(gobj,haxis,sepLines=NULL,sepPlots=NULL,...) {
  options<-list(...)
  if ("theme" %in% options)
     theme<-options$theme
  else
     theme<-jmvcore::theme_default()

    dep<-gobj$analysis$options$dep
    errorType <- gobj$options$plotError
    ciWidth   <- gobj$options$ciWidth
    image<-gobj$descPlot
    
    if (errorType=="ci")
      errorType<-paste0(ciWidth,"% ",toupper(errorType))
    
    if ( ! is.null(sepLines)) {
      p<-gplots.twoWaysPlot(image,theme,dep,sepLines,sepPlots,errorType)
    } else {
      p<-gplots.oneWayPlot(image,theme,dep,sepLines,errorType)
    }       
    p
    
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
  args<-list(simpleVariable=variable,simpleModerator=moderator,simple3way=threeway,...)
  gamlj_update(gobj,args)  
}


#' Update a GAMLj results by removing a block or results
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
