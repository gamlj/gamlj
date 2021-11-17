SmartTable <- R6::R6Class("SmartTable",
                 inherit = Dispatch,
                 cloneable=FALSE,
                 class=FALSE,
                 public=list(
                        name=NULL,
                        table=NULL,
                        nickname=NULL,
                        expandable=FALSE,
                        expandaSuperTitle=NULL,
                        activated=NULL,
                        initialize=function(table,estimator=NULL) {
                            self$name       <-  table$name
                            self$table      <-  table
                            self$nickname   <-  gsub("/","_",table$path,fixed = TRUE)
                            private$.estimator  <-   estimator
                            private$.init_function<-paste0("init_",self$nickname)
                            private$.fill_function<-paste0("fill_",self$nickname)
                            ginfo("table",self$name,"initialized..")
                            self$activated<-self$table$visible

                        },
                        initTable=function() {
                          
                          if (private$.noinit())
                              return()

                          ginfo("table",self$name,"inited..")
                          
                          output<-private$.estimator[[private$.init_function]]()
                          
                          results<-output$obj
                          error<-output$error
                          warning<-output$warning
                          
                          if (error!=FALSE) {
                            self$table$setError(error)
                            return()
                          }
                          
                          if (warning!=FALSE) {
                            len<-length(self$table$notes)
                            self$table$setNote(len+1,warning)
                          }                          
                          if (self$expandable) private$.expand(self$table,results)
                          private$.fill(self$table,results)

                        },

                        fillTable=function() {
                
                          if (private$.nofill())
                                return()
                          
                          ginfo("table",self$name,"filled..")
                          
                          output<-private$.estimator[[private$.fill_function]]()
                          results<-output$obj
                          error<-output$error
                          warning<-output$warning
                          
                          if (error!=FALSE) {
                                self$table$setError(error)
                                return()
                          }

                          if (warning!=FALSE) {
                              len<-length(self$table$notes)
                              self$table$setNote(len+1,warning)
                          }                          
                          
                          len<-length(self$table$notes)
                          for (i in seq_along(private$.estimator$warnings[[self$nickname]])) 
                               self$table$setNote(i+len,private$.estimator$warnings[[self$nickname]][[i]])
                          private$.fill(self$table,results)
                        },
                        
                        fillFunction=function(obj) {
                             private$.fill_function<-obj
                          },
                        ci=function(alist,ciwidth=95,ciformat="{}% Confidence Intervals") {
                          if (is.null(names(alist))) {
                               .names<-alist
                               .labels<-rep("",length(alist))
                          } else {
                            .names<-names(alist)
                            .labels<-alist
                            
                          }
                    
                          for (i in seq_along(alist)) {
                            label <-paste(.labels[[i]],ciformat)
                            name  <-.names[[i]]
                            l<-paste0(name,".ci.lower")
                            u<-paste0(name,".ci.upper")
                            self$table$getColumn(l)$setSuperTitle(jmvcore::format(label, ciwidth))
                            self$table$getColumn(u)$setSuperTitle(jmvcore::format(label, ciwidth))
                          }
                          
                          
                        }

                 ), ## end of public
                 
                    private=list(
                              .estimator=NULL,
                              .init_function=NULL,
                              .fill_function=NULL,
                              .nofill=function() {
                                
                                if (!self$activated)
                                  return(TRUE)
                                
                                .no<-!self$table$isNotFilled()
                                if (is.null(private$.fill_function)) 
                                     .no<-TRUE
                                if (!(private$.fill_function %in% names(private$.estimator)))
                                     .no<-TRUE
                                .no
                              },
                              .noinit=function() {
                                
                                if (!self$activated)
                                   return(TRUE)
                                
                                .no<-FALSE
                                if (is.null(private$.init_function)) 
                                  .no<-TRUE
                                if (!(private$.init_function %in% names(private$.estimator)))
                                  .no<-TRUE
                                .no
                              },
                              .fill=function(atable,aresult) {
                                
                                maxrow<-atable$rowCount
                                
                                .insert<-function(i,w) {
                                  if (i>maxrow)
                                      atable$addRow(rowKey=i,w)
                                  else
                                      atable$setRow(rowNo=i,w)
                                }
                                
                                fixNA<-TRUE
                                
                                if (inherits(aresult,"data.frame")) 
                                  aresult<-lapply(1:nrow(aresult),function(i) aresult[i,])
                                
                                for (i in seq_along(aresult)) {
                                  t<-aresult[[i]]
                                  if (fixNA) 
                                    t[which(is.na(t))]<-""
                                  .insert(i,t)
                                }
                              },
                              .expand=function(table,result) {
                                
                                if (inherits(result, "list"))
                                         result<-do.call(rbind,result)
                                  .names<-names(result)
                                  .types<-unlist(lapply(result,class))
                                  .types<-gsub("numeric","number",.types)
                                  .types<-gsub("integer","number",.types)
                                  .types<-gsub("factor","text",.types)
                                
                                
                                .present<-names(table$columns)
                                .names<-setdiff(.names,.present)
                                superTitle<-NULL
                                for (i in seq_along(.names)) {
                                  table$addColumn(index=i,name = .names[[i]], title = .names[[i]], superTitle = superTitle, type=.types[i])
                                }
                                
                                
                                
                                
                              }
                              
                              
                          ) #end of private
                              

) ## end of class
 
SmartArray <- R6::R6Class("SmartArray",
                          inherit = SmartTable,
                          cloneable=FALSE,
                          class=TRUE,
                        
                          public=list(
                            itemNames=NULL,
                            itemKeys=NULL,
                            
                            initialize=function(tables,estimator=NULL) {
                              super$initialize(tables,estimator)
                              self$itemKeys<-tables$itemKeys
                              self$itemNames<-tables$itemNames
                        
                            },
                            initTable=function() {
                              
                              if (private$.noinit())
                                 return()
                              
                              ginfo("Array",self$name,"inited...")
                              output<-private$.estimator[[private$.init_function]]()

                              results<-output$obj
                              error<-output$error
                              warning<-output$warning
                              
                              for (i in seq_along(self$itemKeys)) {
                                key     <-  self$itemKeys[[i]]
                                aresult <-  results[[i]]
                                atable  <-  self$table$get(key = key)
                                
                                if (self$expandable) private$.expand(atable,aresult)
                                private$.fill(atable,aresult)
                                
                                if (error!=FALSE) {
                                  atable$setError(error)
                                  return()
                                }
                                
                                if (warning!=FALSE) {
                                  len<-length(atable$notes)
                                  atable$setNote(len+1,warning)
                                }           
                                

                              }

                            },
                            fillTable=function() {
                              
                              if (private$.nofill())
                                return()
                              
                              ginfo("array",self$name,"filled..")
                              
                              output<-private$.estimator[[private$.fill_function]]()
                              results<-output$obj
                              error<-output$error
                              warning<-output$warning
                              
                              for (i in seq_along(self$itemKeys)) {
                                key     <-  self$itemKeys[[i]]
                                aresult <-  results[[i]]
                                atable  <-  self$table$get(key = key)
                                
                                private$.fill(atable,aresult)
                                
                                if (error!=FALSE) {
                                  atable$setError(error)
                                  return()
                                }
                                
                                if (warning!=FALSE) {
                                  len<-length(atable$notes)
                                  atable$setNote(len+1,warning)
                                }           
                              }
                            }
                            
                            
      ) ## end of public
      
) # end of class