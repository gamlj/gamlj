Comparator <- R6::R6Class(
  "Comparator",
  cloneable=FALSE,
  class=FALSE,
  inherit = Dispatch,
  public=list(
      semPathsOptions=NULL,
      initialize=function(options,model) {
            super$initialize(options=options,vars=NULL)
  
      },
      save=function(model) {
        if (is.null(model))
           return()
        if (isFALSE(self$options$modelcomparison))
          return()
        
        if (is.null(self$options$modelid))
           return()
        if (self$options$modeldescription & trimws(self$options$modeldescription)=="")
          self$errors<-list(topic="modelcomparison",message="Model not stored for comparison. Please specify a description for this model")
      
          
      }

      
  ), # end of public
  private = list(

  ) # end of private
) # end of class
    