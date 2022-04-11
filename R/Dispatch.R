#### send messages to tables from anywhere in the code
#### warnings can be sent to tables or array. The table needs not to be created (like a keyed table within an array)
#### to receive a warning. The warning is stored in the parent and then passed to the tabe
#### Errors must be sent to existing (already defined) objects
#### warnings can be transient (get remove after init) when init=TRUE is passed
#### warnings and error are passed only to visible tables.
#### errors are passed directly to the jamovi object. If option final=TRUE, a `stop()` is issued

Dispatch <- R6::R6Class(
            "Dispatch",
            class=TRUE, 
            cloneable=FALSE, ## should improve performance https://r6.r-lib.org/articles/Performance.html ###
            public=list(
                        tables=NULL,
                        initialize=function(results) { 
                          
                                  self$tables<-results
                                  
                        },
                        print=function() {
     
                                 print(private$.warnings)
                                 print(private$.errors)
      
                        }
                        ),
            active=list(
                        warnings=function(obj) {

                                if (missing(obj)) return()
                                if (is.null(obj$message)) return()
                                if (isFALSE(obj$message)) return()
                                if (is.null(obj$topic)) stop("SCAFFOLD:  a message should have a topic (a table path)")
                                path<-stringr::str_split(obj$topic,"_")[[1]]
                                
                                table<-private$.find_table(path)
                                
                                if (!is.something(table)) stop("SCAFFOLD: a message was sent to a non-existing result object: ",obj$topic)
                                state<-as.list(table$state)
                                if (!hasName(obj,"id")) obj$id<-jmvcore::toB64(obj$message)
                                
                                obj$message<-private$.translate(obj$message)
                                
                                if (inherits(table,"Html")) {
                                  content<-table$content
                                  content<-table$setContent(paste(content,"<div><i>Note:</i>",obj$message,"</div>"))
                                  table$setVisible(TRUE)
                                  return()
                                }
                                     
                                
                                if (!inherits(table,"Table")) 
                                     what<-obj$id
                                else
                                     what<-length(state$notes)+1
                                
                               state$notes[[what]]<-obj
                               table$setState(state)
                               
                        },
                        errors=function(obj) {
          
                               if (missing(obj))
                                     return(private$.errors)

                               if (!is.list(obj))
                                     stop("SCAFFOLD: Error requires a named list with `topic` and `message`")
          
                               if (!hasName(obj,"topic") | !hasName(obj,"message"))
                                    stop("SCAFFOLD:: Error requires a named list with `topic` and `message`")
  

                               if (is.null(obj$message) || obj$message==FALSE)
                                    return()
          
                               obj$message<-private$.translate(obj$message)
                          
                               if (hasName(obj,"final") && (obj$final))
                                   stop(obj$message)
                          
                               path<-stringr::str_split(obj$topic,"_")[[1]]
                               table<-private$.find_table(path)
                               table$setError(obj$message)

                       },
                       warnings_topics=function() {return(names(private$.warnings))},
                       errors_topics=function() {return(names(private$.errors))}
        
        
            ),
            private = list(
                      .warnings=list(),
                      .errors=list(),
                      .find_table=function(path) {
                        
                        tableobj<-self$tables
                        found<-FALSE
                        for (aname in path)
                          if (hasName(tableobj,aname)) {
                            found<-TRUE
                            tableobj<-tableobj[[aname]]
                          }
                        if (found)
                             return(tableobj)
                        else
                             return(NULL)
                        
                      },
                      .translate=function(msg) {
      
                            for (w in TRANS_WARNS) {
                                 msg<-gsub(w$original,w$new,msg,fixed=T)
                            }
                           return(msg)

                       }
                       
            ) #end of private
) #end of class



