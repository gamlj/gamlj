SmartTable <- R6::R6Class("SmartTable",
                 inherit = Dispatch,
                 cloneable=FALSE,
                 class=FALSE,
                 public=list(
                        name=NULL,
                        table=NULL,
                        nickname=NULL,
                        expandable=FALSE,
                        expandSuperTitle=NULL,
                        expandFromBegining=FALSE,
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
                            test<-grep("___key___",table$title,fixed = TRUE)
                            if (length(test)>0) {
                               keys<-jmvcore::stringifyTerm(table$key)
                               table$setTitle(gsub("___key___",keys,table$title))
                            }
                                
                                

                        },
                        initTable=function() {
                          
                          if (private$.noinit())
                              return()

                          ginfo("table",self$name,"inited..")
                          
                          if (inherits(private$.init_function,"character") ) {
                            
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
                                 
                          }
                          else 
                                 results<-private$.init_function
                          
                          if (self$expandable) private$.expand(results)
                          private$.fill(self$table,results)

                        },

                        fillTable=function() {
                
                          if (private$.nofill())
                                return()
                          
                          ginfo("table",self$name,"filled..")
                          
                          if (inherits(private$.fill_function,"character") ) {
                            
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
                            
                          }
                          else 
                            results<-private$.fill_function
                          

                          len<-length(self$table$notes)
                          for (i in seq_along(private$.estimator$warnings[[self$nickname]])) 
                               self$table$setNote(i+len,private$.estimator$warnings[[self$nickname]][[i]])
                          private$.fill(self$table,results)
                        },
                        
                        initFunction=function(aObject) {
                             private$.init_function<-aObject
                          },
                        fillFunction=function(aObject) {
                          private$.fill_function<-aObject
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
                                if (is.character(private$.init_function) & !(private$.init_function %in% names(private$.estimator))  )
                                  .no<-TRUE
                                .no
                              },
                              .fill=function(atable,aresult) {
                                
                                names(aresult)<-make.names(names(aresult),unique = TRUE)
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
                              .expand=function(results) {
                                
                                if (inherits(results, "list"))
                                         result<-do.call(rbind,result)
                                
                                  .titles    <-  names(results)
                                  .names    <-  make.names(.titles,unique = TRUE)
                                  .types<-unlist(lapply(results,class))
                                  .types<-gsub("numeric","number",.types)
                                  .types<-gsub("integer","number",.types)
                                  .types<-gsub("factor","text",.types)
                                
                                  .present  <-  names(self$table$columns)
                                  .names   <-  setdiff(.names,.present)

                               last<-ifelse(self$expandFromBegining,0,(length(self$table$columns)-1))
                               for (i in seq_along(.names)) {
                                  self$table$addColumn(index=(i+last),name = .names[[i]], title = .titles[[i]], superTitle = self$expandSuperTitle, type=.types[i])
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
                                aSmartTable<-SmartTable$new(atable,self)
                                aSmartTable$expandable<-self$expandable
                                aSmartTable$expandFromBegining<-self$expandFromBegining
                                aSmartTable$expandSuperTitle<-self$expandSuperTitle
                                
                                if (is.something(private$.ci)) {
                                  aSmartTable$ci(private$.ci,private$.ciwidth,private$.ciformat)
                                }
                                aSmartTable$initFunction(aresult)
                                aSmartTable$initTable()
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
                            },
                            ci=function(alist,ciwidth=95,ciformat="{}% Confidence Intervals"){
                              
                              private$.ci<-alist
                              private$.ciwidth<-ciwidth
                              private$.ciformat<-ciformat
                              
                            }
                            
                            
      ), ## end of public
      private=list(
        .ci=NULL,
        .ciwidth=NULL,
        .ciformat=NULL
        
        
        
      ) #end of private
) # end of class