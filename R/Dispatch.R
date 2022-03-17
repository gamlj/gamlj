
Dispatch <- R6::R6Class(
  "Dispatch",
  class=TRUE, ## this and the next 
  cloneable=FALSE, ## should improve performance https://r6.r-lib.org/articles/Performance.html ###
  public=list(
    tables=NULL,
    initialize=function(results) { 
    self$tables<-results
    }
  ),
  active=list(
        warnings=function(obj) {

          
              if (missing(obj)) return()
              if (is.null(obj$message)) return()
              if (isFALSE(obj$message)) return()
          
              if (is.null(obj$topic)) stop("warnings messages should have a topic (a table path)")
              obj$message<-fromb64(obj$message)
              
              path<-stringr::str_split(obj$topic,"_")[[1]]
              tableobj<-self$tables
              found<-FALSE
              for (aname in path)
                 if (hasName(tableobj,aname)) {
                       found<-TRUE
                       tableobj<-tableobj[[aname]]
                 }
              
               if (!found) stop("a message was sent to a non-existing result object ",obj$topic)
               state<-as.list(tableobj$state)

               if (!hasName(obj,"id")) obj$id<-jmvcore::toB64(obj$message)
               
               if (!inherits(tableobj,"Table")) 
                     what<-obj$id
               else
                     what<-length(state$notes)+1
                              
               state$notes[[what]]<-obj
               tableobj$setState(state)
          
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
         if (is.there(w$original,msg))
             return(w$new)
       }
       return(msg)

     }
  )
)



