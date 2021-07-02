
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
    ####### eval option that may not exist ####
    
    option=function(val,spec=NULL) {
      
        res<-hasName(self$options,val)
        if (res) {
          if (is.logical(self$options[[val]]))
              res<-self$options[[val]]
          else
              res<-is.something(self$options[[val]])

          if (!is.null(spec))
              res<-(spec %in% self$options[[val]])
        }
        res      
      
    },
    
    ### this requires that sooner or later something is a Dispatch object ###
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

        }
        
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



