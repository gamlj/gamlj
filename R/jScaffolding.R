Scaffold <- R6::R6Class("Scaffold",
                          cloneable=FALSE,
                          class=FALSE,
                          public=list(
                            analysis=NULL,  
                            options=NULL,
                            initialize=function(object) {
                                 
                              self$analysis<-object
                              self$options<-object$options

                            },
                            option=function(val,spec=NULL) {
                              
                              res<-utils::hasName(self$options,val)
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
                              
                              test<-utils::hasName(self$options,val)
                              if (test) 
                                return(self$options[[val]])
                              else
                                return(NULL)
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
                              init<-(hasName(alist,"initOnly") && alist[["initOnly"]]) 
                              lapply(alist$message, function(msg) private$.dispatcher$warnings<-list(topic=alist$topic,message=msg, initOnly=init))
                            },
                            error=function(alist) {
                                if (is.null(private$.dispatcher))
                                    private$.create_dispatcher()
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
