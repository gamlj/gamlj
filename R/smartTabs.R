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
                            self$activated<-self$table$visible
                            test<-grep("___key___",table$title,fixed = TRUE)
                            if (length(test)>0) {
                               keys<-jmvcore::stringifyTerm(table$key)
                               table$setTitle(gsub("___key___",keys,table$title))
                            }
                                
                            ginfo("Table",self$nickname,"initialized..")
                            

                        },
                        initTable=function() {
                          
                          if (private$.noinit())
                              return()

                          ginfo("Table",self$nickname,"inited..")
                          
                          self$table$setVisible(TRUE)
                          
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
                          
                          ginfo("Table",self$nickname,"filled..")
                          
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
                            if (l %in% names(self$table$columns))
                                    self$table$getColumn(l)$setSuperTitle(jmvcore::format(label, ciwidth))
                            if (u %in% names(self$table$columns))
                                    self$table$getColumn(u)$setSuperTitle(jmvcore::format(label, ciwidth))
                          }
                          
                          
                        }

                 ), ## end of public
                    active=list(
                      
                        superTitle=function(alist) {
                          
                          if (missing(alist))
                              return(private$.superTitle)
                          
                          if (!is.list(alist))
                             stop("SuperTitle must be a list")
                          .names<-names(self$table$columns)
                          for (stn in names(alist)) {
                            where<-grep(stn,.names,fixed=T)
                            if (length(where)>0)
                               for (i in where) {
                                  self$table$getColumn(.names[[i]])$setSuperTitle(alist[[stn]])
                               }
                          }
                        }
                      
                    ), #end of active
                    private=list(
                              .estimator=NULL,
                              .init_function=NULL,
                              .fill_function=NULL,
                              .superTitle=NULL,
                              .nofill=function() {
                                
                                if (!self$activated)
                                  return(TRUE)
                                
                                if (is.null(private$.fill_function)) 
                                  return(TRUE)
                                if (is.character(private$.fill_function) & !(private$.fill_function %in% names(private$.estimator))  )
                                  return(TRUE)
                                
                                return(!self$table$isNotFilled())
                                
                                
                              },
                              .noinit=function() {
                                
                                if (!self$activated)
                                   return(TRUE)
                                if (is.null(private$.init_function)) 
                                   return(TRUE)
                                if (is.character(private$.init_function) & !(private$.init_function %in% names(private$.estimator))  )
                                   return(TRUE)
                                
                                return(FALSE)

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
                                  
                                  .titles   <-  names(results)
                                  .titles   <- sapply(.titles,function(t) ifelse(is.b64(t),fromb64(t),t))
                                  .names    <-  make.names(.titles,unique = TRUE)
                                  .types<-unlist(lapply(results,class))
                                  .types<-gsub("numeric","number",.types)
                                  .types<-gsub("integer","number",.types)
                                  .types<-gsub("factor","text",.types)
                                
                                  .present  <-  names(self$table$columns)
                                  .names   <-  setdiff(.names,.present)

                               if (self$expandFromBegining) {
                                  for (i in seq_along(.names)) {
                                    self$table$addColumn(index=i,name = .names[[i]], title = .titles[[i]], superTitle = self$expandSuperTitle, type=.types[i])
                                  }
                               } else {
                                 for (i in seq_along(.names)) {
                                   self$table$addColumn(name = .names[[i]], title = .titles[[i]], superTitle = self$expandSuperTitle, type=.types[i])
                                 }
                                 
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
                              ginfo("Array",self$nickname,"initiazialased")
                              self$itemNames<-try_hard(tables$itemNames)$obj
                              self$itemKeys<-try_hard(tables$itemKeys)$obj

                            },
                            initTable=function() {
                              
                              if (private$.noinit())
                                 return()
                              
                              ginfo("Array",self$nickname,"inited...")
                              output<-private$.estimator[[private$.init_function]]()

                              results<-output$obj
                              error<-output$error
                              warning<-output$warning
                              
                              if (!isFALSE(error))
                                  self$table$setError(error)

                              for (i in seq_along(self$itemKeys)) {
                                
                                key     <-  self$itemKeys[[i]]
                                aresult <-  results[[i]]
                                atable  <-  self$table$get(key = key)
                                aSmartTable<-SmartTable$new(atable,self)
                                aSmartTable$expandable<-self$expandable
                                aSmartTable$expandFromBegining<-self$expandFromBegining
                                aSmartTable$expandSuperTitle<-self$expandSuperTitle
                                aSmartTable$activated<-self$activated
                                if (is.something(private$.ci)) {
                                  aSmartTable$ci(private$.ci,private$.ciwidth,private$.ciformat)
                                }
                                
                                aSmartTable$initFunction(aresult)
                                aSmartTable$initTable()
                              }
                              self$table$setVisible(TRUE)
                            },
                            fillTable=function() {
                              
                              if (private$.nofill())
                                return()
                              
                              ginfo("Array",self$nickname,"filled")
                              
                              output<-private$.estimator[[private$.fill_function]]()
                              results<-output$obj
                              error<-output$error
                              warning<-output$warning
                              
                              if (error!=FALSE) {
                                self$table$setError(error)
                                return()
                              }
                              
                              if (warning!=FALSE) {
                                len<-length(atable$notes)
                                self$table$setNote(len+1,warning)
                              }           
                              
                              for (i in seq_along(self$itemKeys)) {
                                key     <-  self$itemKeys[[i]]
                                aresult <-  results[[i]]
                                atable  <-  self$table$get(key = key)
                                
                                private$.fill(atable,aresult)
                                
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