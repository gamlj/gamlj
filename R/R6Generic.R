
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
    },
    ### this requires that sooner or later something should be a Dispatch object ###
    absorbe_warnings=function(obj) {

      if (!is.something(obj))
         return()
      
      if (inherits(obj,"Dispatch")) {
              wars<-obj$warnings
              if (!is.something(wars))
                    return()
              for (topicname in names(wars)) {
                      topic<-wars[[topicname]]
                      for (t in topic) 
                              self$warnings<-list(topic=topicname,message=t)
              }
       } else
            for (aobj in obj)
                 self$absorbe_warnings(aobj)
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
              obj<-fromb64(obj,self$vars)
              topic<-private$.warnings[[obj$topic]]
              topic[[length(topic)+1]]<-obj$message
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
                     obj<-fromb64(obj,self$vars)
                     private$.errors[[length(private$.errors)+1]]<-obj
        }
        
  ),
  
  private = list(
     .warnings=list(),
     .errors=list()
  )
)



