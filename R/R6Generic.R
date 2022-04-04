
Dispatch <- R6::R6Class(
  "Dispatch",
  class=FALSE, ## this and the next 
  cloneable=FALSE, ## should improve performance https://r6.r-lib.org/articles/Performance.html ###
  public=list(
    vars=NULL,
    options=NULL,
    initialize=function(options,vars) {
      self$vars<-vars
      self$options<-options
    }
    
  ),
  active=list(
        warnings=function(obj) {

              if (missing(obj))
                           return(private$.warnings)
              if (is.null(obj$message))
                           return()
              if (obj$message==FALSE)
                           return()
              topic<-private$.warnings[[obj$topic]]
              topic[[length(topic)+1]]<-private$.translate(obj$message)
              topic<-unique(topic)
              private$.warnings[[obj$topic]]<-topic
              
          },
        errors=function(obj) {
 
                    if (missing(obj))
                            return(private$.errors)
                     if (is.null(obj))
                            return()
                     if (obj==FALSE)
                           return()
          
                     private$.errors[[length(private$.errors)+1]]<-private$.translate(obj)
        }
        
  ),
  
  private = list(
     .warnings=list(),
     .errors=list(),
     .translate=function(msg) {
       return(msg)
     }
  )
)

