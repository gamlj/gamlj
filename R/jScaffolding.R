Scaffold <- R6::R6Class("Scaffold",
                          cloneable=FALSE,
                          class=FALSE,
                          public=list(
                            analysis =NULL,  
                            options  =NULL,
                            ok       =TRUE,    # can be used to halt the process without calling stop() 
                            initialize=function(object) {
                                 
                              self$analysis<-object
                              self$options<-object$options

                            },
                            option=function(val,spec=NULL) {
                              
                              res<-is.joption(self$options,val)
                              if (res) {
                                if (is.logical(self$options[[val]]))
                                  res<-self$options[[val]]
                                else
                                  res<-is.something(self$options[[val]])
                                
                                
                                if (!is.null(spec))
                                  res<-any(spec %in% self$options[[val]])
                              }
                              res      
                              
                            },
                            optionValue=function(val) {
                              
                              test<-is.joption(self$options,val)
                              if (test) 
                                return(self$options[[val]])
                              else
                                return(NULL)
                            },
                            
                            stop=function(msg, return=TRUE) {
                            
                                if (self$option(".interface","R")) stop(msg,call.=FALSE)
                                    
                                if (exists("ERROR_TABLE")) {
                                 self$warning<-list(topic=ERROR_TABLE,message=msg,head="error")
                                 self$ok <- FALSE
                                 if (return) {
                                       call <- rlang::expr(return()) 
                                       rlang::eval_bare(call, env = parent.frame())
                                 }
                              } else
                                 stop(msg,call.=FALSE)
                              
                            }
                          ), ## end of public
                          active=list(
                            
                            storage=function(obj) {

                              if (missing(obj))
                                 return(private$.storage)
                              
                              private$.storage<-obj
                              
                            },
                            warning=function(alist) {
                                
                                if (is.null(private$.dispatcher))
                                    private$.create_dispatcher()
                              
                              lapply(alist$message, function(msg) {
                                onelist<-alist
                                onelist$message<-msg
                                if (!isFALSE(msg))
                                     private$.dispatcher$warnings<-onelist
                              })
                            },
                            error=function(alist) {
                                if (is.null(private$.dispatcher))
                                    private$.create_dispatcher()
                                if (!isFALSE(alist$message))
                                    private$.dispatcher$errors<-alist
                            }
                            
                          ), #end of active
                          private=list(
                            .storage=NULL,
                            .dispatcher=NULL,
                            .create_dispatcher=function() {
                                private$.dispatcher<-Dispatch$new(self$analysis$results)
                            }
                          ) #end of private
) ## end of class
