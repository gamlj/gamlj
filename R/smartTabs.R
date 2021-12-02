SmartTable <- R6::R6Class("SmartTable",
                          cloneable=FALSE,
                          class=FALSE,
                          public=list(
                            name=NULL,
                            table=NULL,
                            dispatcher=NULL,
                            nickname=NULL,
                            expandable=FALSE,
                            expandSuperTitle=NULL,
                            expandFromBegining=TRUE,
                            activated=NULL,
                            key=NULL,
                            keys_sep=NULL,
                            keys_raise=TRUE,
                            spaceBy=NULL,
                            spaceAt=NULL,
                            spaceMethod="cb",
                            combineBelow=0,
                            ciroot=NULL,
                            ciwidth=NULL,
                            ciformat="{}% Confidence Intervals",
                            initialize=function(table,estimator=NULL) {
                              self$name       <-  table$name
                              self$table      <-  table
                              self$nickname   <-  make.names(gsub('"','',gsub("/","_",table$path,fixed = TRUE),fixed=TRUE))
                              private$.estimator  <-   estimator
                              private$.init_function<-paste0("init_",self$nickname)
                              private$.run_function<-paste0("run_",self$nickname)
                              self$activated<-self$table$visible
                              
                              
                              if (inherits(estimator,"SmartArray")) {
                                self$activated              <- estimator$activated
                                self$expandable             <- estimator$expandable
                                self$expandFromBegining     <- estimator$expandFromBegining
                                self$expandSuperTitle       <- estimator$expandSuperTitle
                                self$ciroot                 <- estimator$ciroot
                                self$ciwidth                <- estimator$ciwidth
                                self$ciformat               <- estimator$ciformat
                                self$keys_sep               <- estimator$keys_sep
                                self$keys_raise             <- estimator$keys_raise
                                self$combineBelow           <- estimator$combineBelow
                                self$spaceMethod            <- estimator$spaceMethod
                                self$spaceAt                <- estimator$spaceAt
                                self$spaceBy                <- estimator$spaceBy
                              }
                              
                              if ("key" %in% names(table) )
                                if (is.something(table$key))
                                  self$key<-table$key
                              
                            },
                            initTable=function() {
                              private$.phase<-"init"
                              
                              if (private$.stop()) {
                                return()
                              }
                              self$table$setVisible(TRUE)
                              rtable<-private$.getData()
                              if (self$expandable) private$.expand(rtable)
                              private$.ci()
                              private$.fill(self$table,rtable)
                              self$title
                              for (col in self$table$columns)
                                col$setTitle(fromb64(col$title))
                              tinfo("SmartTable",self$nickname,"inited")
                            },
                            runTable=function() {
                              
                              private$.phase<-"run"
                              
                              if (private$.stop()) {
                                return()
                              }
                              rtable<-private$.getData()
                              private$.fill(self$table,rtable)
                              private$.spaceBy()
                              
                              if (is.something(self$dispatcher))
                                 for (w in self$dispatcher$warnings[[self$nickname]]) {
                                      n<-length(self$table$notes)
                                    self$table$setNote(as.character(n+1),w)
                                 }
                              
                              tinfo("SmartTable",self$nickname,"run")
                              
                            },
                            
                            ci=function(alist,ciwidth=95,ciformat="{}% Confidence Intervals"){
                              
                              self$ciroot<-alist
                              self$ciwidth<-ciwidth
                              self$ciformat<-ciformat
                              
                            },
                            
                            initFunction=function(aObject) {
                              private$.init_function<-aObject
                            },
                            runFunction=function(aObject) {
                              private$.run_function<-aObject
                            }
                            
                          ), ## end of public
                          active=list(
                            
                            title=function(aname) {
                              
                              if (missing(aname)) {
                                
                                test<-grep("___key___",self$table$title,fixed = TRUE)
                                if (length(test)>0 & is.something(self$key)) {
                                  if (is.something(self$keys_sep))
                                    key<-jmvcore::stringifyTerm(self$key,sep=self$keys_sep,raise=self$keys_raise)
                                  else
                                    key<-jmvcore::stringifyTerm(self$key,raise=self$keys_raise)
                                  
                                  
                                  aname<-gsub("___key___",key,self$table$title)
                                  self$table$setTitle(aname)
                                  
                                }
                                return()
                              }
                              self$table$setTitle(aname)
                              
                              
                            },
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
                            .run_function=NULL,
                            .superTitle=NULL,
                            .phase="init",
                            .new_columns=NULL,
                            
                            .stop=function() {
                              
                              if (private$.phase=="init") {
                                fun<-private$.init_function
                                filled<-FALSE
                              } else {
                                
                                fun<-private$.run_function
                                filled<-!self$table$isNotFilled()
                                if (filled & ("rowCount" %in% names(self$table))) 
                                  filled<-self$table$rowCount>0
                                
                                
                              }
                              
                              if (!self$activated)
                                return(TRUE)
                              
                              if (is.null(fun)) 
                                return(TRUE)
                              
                              if (is.character(fun) & !(fun %in% names(private$.estimator))  ) {
                                return(TRUE)
                                
                              }
                              
                              return(filled)
                              
                            },
                            
                            .getData=function(when) {
                              
                              ## check in which phase we are
                              if (private$.phase=="init")
                                fun<-private$.init_function
                              else
                                fun<-private$.run_function
                              
                              ### check how to retrieve the data
                              if (inherits(fun,"character") ) {
                                
                                output<-private$.estimator[[fun]]()
                                
                                rtable<-output$obj
                                error<-output$error
                                warning<-output$warning
                                
                                if (error!=FALSE) {
                                  tinfo("Error in ",fun,error)
                                  self$table$setError(error)
                                  return()
                                }
                                
                                if (warning!=FALSE) {
                                  if ("notes" %in% names(self$table)) {
                                    len<-length(self$table$notes)
                                    self$table$setNote(len+1,warning)
                                  } 
                                }
                                
                              }
                              else 
                                rtable<-fun
                              
                              rtable
                            },
                            .fill=function(jtable,rtable) {
                              
                              maxrow<-jtable$rowCount
                              
                              .insert<-function(i,w) {
                                if (i>maxrow)
                                  jtable$addRow(rowKey=i,w)
                                else
                                  jtable$setRow(rowNo=i,w)
                              }
                              
                              rlist <-private$.listify(rtable)
                              for (i in seq_along(rlist)) {
                                t<-rlist[[i]]
                                t[which(is.na(t))]<-""
                                t[which(t==".")]<-NA
                                .insert(i,t)
                              }
                              
                              
                              
                            },
                            .expand=function(rtable) {
                              
                              rtable<-private$.framefy(rtable)
                              .names    <-  names(rtable)
                              
                              if (is.something(attr(rtable,"titles")))
                                .titles<-attr(rtable,"titles")
                              else
                                .titles<-.names
                              .types<-unlist(lapply(rtable,class))
                              .types<-gsub("numeric","number",.types)
                              .types<-gsub("integer","number",.types)
                              .types<-gsub("factor","text",.types)
                              .present<-names(self$table$columns)
                              .names<-setdiff(.names,.present)
                              
                              if (self$expandFromBegining) {
                                for (i in seq_along(.names)) {
                                  cb<-ifelse(i %in% self$combineBelow,TRUE,FALSE)
                                  if (self$combineBelow=="new") cb<-TRUE
                                  self$table$addColumn(index=i,
                                                       name = .names[[i]], 
                                                       title = .titles[[i]], 
                                                       superTitle = self$expandSuperTitle, 
                                                       type=.types[i],
                                                       combineBelow=cb)
                                }
                                private$.new_columns<-seq_along(.names)
                                
                              } else {
                                last<-length(self$table$columns)
                                for (i in seq_along(.names)) {
                                  cb<-ifelse(i %in% self$combineBelow,TRUE,FALSE)
                                  self$table$addColumn(name = .names[[i]], 
                                                       title = .titles[[i]], 
                                                       superTitle = self$expandSuperTitle, 
                                                       type=.types[i],
                                                       combineBelow=cb)
                                }
                                private$.new_columns<-seq_along(.names)+last
                              }
                            },
                            
                            .spaceBy=function() {
                              
                              try_hard({
                                
                                if (self$spaceBy=="new")
                                  .spaceBy=private$.new_columns
                                else 
                                  .spaceBy=self$spaceBy
                                
                                for (sb in .spaceBy) {
                                  
                                  col<-self$table$asDF[[sb]]
                                  rows<-unlist(lapply(unlist(unique(col)),function(x) min(which(col==x))))
                                  rows<-which(!col==c(col[-1],col[[length(col)]]))+1
                                  if (self$spaceMethod=="cb" & length(rows)==length(col)-1)
                                    rows<-NULL
                                  for (j in rows)
                                    self$table$addFormat(rowNo=j,col=1,jmvcore::Cell.BEGIN_GROUP)
                                  
                                }
                                
                                for (j in self$spaceAt)
                                  self$table$addFormat(rowNo=j,col=1,jmvcore::Cell.BEGIN_GROUP)
                                  
                              })
                              
                            },
                            .ci=function() {
                              
                              if (is.null(self$ciroot))
                                return()
                              
                              alist<-self$ciroot
                              ciwidth<-self$ciwidth
                              ciformat<-self$ciformat
                              if (is.null(names(alist))) {
                                .names<-alist
                                .labels<-rep("",length(alist))
                              } else {
                                .names<-names(alist)
                                .labels<-alist
                                
                              }
                              
                              for (i in seq_along(alist)) {
                                
                                .label <-paste(.labels[[i]],ciformat)
                                .name  <-.names[[i]]
                                l<-paste0(.name,".ci.lower")
                                u<-paste0(.name,".ci.upper")
                                if (l %in% names(self$table$columns))
                                  self$table$getColumn(l)$setSuperTitle(jmvcore::format(.label, ciwidth))
                                if (u %in% names(self$table$columns))
                                  self$table$getColumn(u)$setSuperTitle(jmvcore::format(.label, ciwidth))
                                
                              }
                            }, 
                            
                            .listify = function(adata) {
                              
                              if (is.null(adata)) {
                                if ("notes" %in% names(self$table)) {
                                  .len<-length(self$table$notes)
                                  self$table$setNote(.len+1,"No result found")
                                }
                                return()
                              }
                              
                              .attr<-private$.getAttributes(adata)
                              .res<-NULL
                              if (inherits(adata,"data.frame")) {
                                .res <- lapply(1:dim(adata)[1], function(a) {
                                  .al<-as.list(adata[a, ])
                                  names(.al)<-names(adata)
                                  .al
                                })
                                names(.res) <- rownames(adata)
                                res<-private$.setAttributes(.res,.attr)
                                return(res)
                              }
                              if (inherits(adata,"list")) {
                                .res<-adata
                                if (!is.listOfList(.res))
                                  .res<-list(.res)
                                res<-private$.setAttributes(.res,.attr)
                                return(res)
                              }
                              self$table$setError(paste("SmartTabs input table should be list of named lists or data.frame, found ", paste(class(adata),collapse = ",")))
                            },
                            
                            .framefy = function(alist) {
                              
                              .attr<-private$.getAttributes(alist)
                              
                              if (inherits(alist,"list"))
                                alist<-do.call(rbind,alist)
                              
                              aframe<-as.data.frame(alist,stringsAsFactors = F,optional = T)
                              for (n in names(aframe))
                                aframe[[n]]<-unlist(aframe[[n]])
                              
                              aframe<-private$.setAttributes(aframe,.attr)
                              
                              aframe
                            },
                            
                            .getAttributes=function(obj) {
                              
                              list(keys=attr(obj,"keys"),
                                   titles=attr(obj,"titles"))
                              
                            },
                            .setAttributes=function(obj,attrs) {
                              
                              attr(obj,"keys")<-attrs[["keys"]]
                              attr(obj,"titles")<-attrs[["titles"]]
                              obj
                            }
                            
                            
                            
                          ) #end of private
                          
                          
) ## end of class

SmartArray <- R6::R6Class("SmartArray",
                          inherit = SmartTable,
                          cloneable=FALSE,
                          class=TRUE,
                          public=list(
                            children=NULL,
                            childrenObjs=list(),
                            initialize=function(tables,estimator=NULL) {
                              
                              super$initialize(tables,estimator)
                              itemNames<-try_hard(tables$itemNames)$obj
                              itemKeys<-try_hard(tables$itemKeys)$obj
                              if (is.something(itemKeys)) self$children<-itemKeys else self$children<-itemNames
                              
                              
                            },
                            initTable=function() {
                              
                              if (private$.stop())
                                return()
                              
                              tinfo("Array",self$nickname,"inited")
                              
                              self$table$setVisible(TRUE)
                              self$title
                              rtables<-private$.getData()
                              .keys<-attr(rtables,"keys")
                              
                              if (!is.something(self$table$items)) {
                                
                                
                                for (i in seq_along(rtables)) {
                                  
                                  
                                  if (is.something(.keys))
                                    .key<-.keys[[i]]
                                  else 
                                    .key<-i
                                  self$table$addItem(key = .key)
                                  
                                }
                                
                                self$children<-seq_along(rtables)
                                
                              }
                              
                              for (i in seq_along(self$table$items)) {
                                
                                
                                jtable  <-  self$table$items[[i]]
                                if (inherits(jtable,"Group")) {
                                  aSmartArray<-SmartArray$new(jtable,self)
                                  aSmartArray$dispatcher<-self$dispatcher
                                  aSmartArray$initFunction(rtables[[i]])
                                  aSmartArray$key<-.keys[[i]]
                                  self$childrenObjs<-append_list(self$childrenObjs,aSmartArray)
                                  
                                } else { 
                                  
                                  ### if we are here, children are tables
                                  aSmartTable<-SmartTable$new(jtable,self)
                                  aSmartTable$dispatcher<-self$dispatcher
                                  aSmartTable$initFunction(rtables[[i]])
                                  self$childrenObjs<-append_list(self$childrenObjs,aSmartTable)
                                  
                                }
                              }
                              for (obj in self$childrenObjs)
                                obj$initTable()
                            },
                            
                            runTable=function() {
                              
                              private$.phase<-"run"
                              
                              if (private$.stop())
                                return()
                              
                              tinfo("Array",self$nickname,"run")
                              
                              rtables<-private$.getData()
                              
                              for (i in seq_along(self$childrenObjs)) {
                                obj<-self$childrenObjs[[i]]
                                obj$runFunction(rtables[[i]])
                                obj$runTable()
                                
                              }
                              
                            }
                            
                            
                            
                          ), ## end of public
                          private=list(
                            .ci=NULL,
                            .ciwidth=NULL,
                            .ciformat=NULL
                            
                            
                            
                          ) #end of private
) # end of class