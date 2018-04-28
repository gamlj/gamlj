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
#' @param a single obj of class \code{\link{gamljMixed}} 
#' @return an object of class GAMLj* as the input object
#' @author Marcello Gallucci

gamlj_update<-function(gobj,params) {
  funs<-list("gamljMixedOptions"=gamlj::gamljMixed)
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
#'
#' @param haxis horizontal axis variable
#' @param seplines variable defining the levels for separate lines 
#' @param sepplot variable defining the levels for which separate plots are produced 
#'  
#' @return an object of class GAMLj* as the input object
#' @author Marcello Gallucci
gamlj_plot<-function(gobj,haxis,seplines=NULL,sepplots=NULL) {
 news<-list(plotHAxis=haxis,plotSepLines=seplines,plotSepPlots=sepplots)
 gamlj_update(gobj,news)  
}

