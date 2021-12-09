
Dispatch <- R6::R6Class(
  "Dispatch",
  class=TRUE, ## this and the next 
  cloneable=FALSE, ## should improve performance https://r6.r-lib.org/articles/Performance.html ###
  public=list(
    initialize=function() { }
  ),
  active=list(
        warnings=function(obj) {

              if (missing(obj))
                           return(private$.warnings)
              if (is.null(obj$message))
                           return()
              if (obj$message==FALSE)
                           return()
              obj<-fromb64(obj)
              topic<-private$.warnings[[obj$topic]]
              msg<-private$.translate(obj$message)
              topic[[length(topic)+1]]<-msg
              topic<-unique(topic)
              private$.warnings[[obj$topic]]<-topic
          },
        errors=function(obj) {
          
          if (missing(obj))
            return(private$.errors)

          if (!is.list(obj))
             stop("Errors require a named list with topic and message")
          
          if (!hasName(obj,"topic"))
              stop("Errors require a named list with topic and message")
  
          if (!hasName(obj,"message"))
            stop("Errors require a named list with topic and message")
          
          if (is.null(obj$message))
            return()
          
          if (obj$message==FALSE)
            return()
          
          obj<-fromb64(obj,self$vars)
          topic<-private$.errors[[obj$topic]]
          topic[[length(topic)+1]]<-obj$message
          topic<-unique(topic)
          private$.errors[[obj$topic]]<-topic

        },
        warnings_topics=function() {return(names(private$.warnings))},
        errors_topics=function() {return(names(private$.errors))}
        
        
  ),
  
  private = list(
     .warnings=list(),
     .errors=list(),
     .translate=function(msg) {
      
       for (w in TRANSWARNS) {
         if (length(grep(w$original,msg,fixed = T))>0)
             return(w$new)
       }
       return(msg)

     }
  )
)



