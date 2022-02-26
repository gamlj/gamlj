### for warnings and error, tables responds to:
###  tablename
###  groupname 
###  groupname_tablename
###  arrayname
###  arrayname_key
###  arrayname_key_tablename
###  arrayname_tablename




SmartTable <- R6::R6Class("SmartTable",
                          cloneable=FALSE,
                          class=FALSE,
                          public=list(
                            name=NULL,
                            table=NULL,
                            topics=NULL,
                            nickname=NULL,
                            expandable=FALSE,
                            expandSuperTitle=NULL,
                            expandFrom=1,
                            activated=NULL,
                            key=NULL,
                            keys_sep=NULL,
                            keys_raise=TRUE,
                            spaceBy=NULL,
                            spaceAt=NULL,
                            spaceMethod="cb",
                            combineBelow=0,
                            ci_info=list(),
                            columnTitles=list(),

                            initialize=function(table,estimator=NULL) {

                              self$name       <-  table$name
                              self$table      <-  table
                              self$nickname   <-  gsub('"','',gsub("/","_",table$path,fixed = TRUE),fixed=TRUE)
                              self$nickname   <-  stringr::str_replace_all(self$nickname,'[\\]"\\[]',"")
                              self$nickname   <-  make.names(self$nickname)
                              private$.estimator  <-   estimator
                              private$.init_function<-paste0("init_",self$nickname)
                              private$.run_function<-paste0("run_",self$nickname)
                              self$activated<-self$table$visible

                              if ("key" %in% names(table) ) 
                                self$key<-table$key
                              

                              if (inherits(private$.estimator,"SmartArray")) {
                                self$activated              <- private$.estimator$activated
                                self$expandable             <- private$.estimator$expandable
                                self$expandFrom             <- private$.estimator$expandFrom
                                self$expandSuperTitle       <- private$.estimator$expandSuperTitle
                                self$ci_info                <- private$.estimator$ci_info
                                self$keys_sep               <- private$.estimator$keys_sep
                                self$keys_raise             <- private$.estimator$keys_raise
                                self$combineBelow           <- private$.estimator$combineBelow
                                self$spaceMethod            <- private$.estimator$spaceMethod
                                self$spaceAt                <- private$.estimator$spaceAt
                                self$spaceBy                <- private$.estimator$spaceBy
                                self$columnTitles          <-  private$.estimator$columnTitles
                              }

                            },
                            initTable=function() {
                              private$.phase<-"init"
                              
                              if (private$.stop()) {
                                return()
                              }
                              
                              ### init warnings topics ####
                                private$.set_topics()

                              ### ###

                              self$table$setVisible(TRUE)
                              rtable<-private$.getData()
                              if (self$expandable) private$.expand(rtable)
                              private$.setColumnTitle()
                              private$.ci()
                              private$.fill(self$table,rtable)
                              self$title
                              state<-as.list(self$table$state)
                              state$notes<-list()
                              private$.spaceBy()
                              
                              
                              tinfo("TABLES: table",self$nickname,"inited")
                            },
                            
                            runTable=function() {
                            
                              tinfo("TABLES: table",self$nickname,"checked for run")
                              self$retrieveNotes()
                              private$.phase<-"run"

                              if (private$.stop()) {
                                return()
                              }
                              
                              rtable<-private$.getData()
                              private$.fill(self$table,rtable)
                              tinfo("TABLES: table",self$nickname,"run")
                              
                            },
                            
                            ci=function(aroot,width=95,label=NULL,format="{}% Confidence Intervals"){
                              
                              if (is.null(label)) label="" 
                              alist<-list(root=aroot,label=label,width=width,format=format)
                              self$ci_info<-append_list(self$ci_info,alist)

                            },
                            
                            initFunction=function(aObject) {
                              private$.init_function<-aObject
                            },
                            runFunction=function(aObject) {
                              private$.run_function<-aObject
                            },
                            
                            retrieveNotes=function(dispatcher=NULL) {
                              
                                if (is.something(self$table$state$notes))
                                    for (i in seq_along(self$table$state$notes))
                                        private$.setNote(self$table$state$notes[i])
                            },
                            
                            setNotes=function(dispatcher=NULL) {
                              
                                if (is.something(dispatcher)) {
                                    topics<-intersect(dispatcher$warnings_topics,self$topics)
                                    for (t in topics) {
                                      for (i in seq_along(dispatcher$warnings[[t]])) {
                                        m<-dispatcher$warnings[[t]][i]
                                        private$.setNote(m)

                                      } 
                                    }

                                }
                            },
                            setColumnTitle=function(name,title) {
                              
                                self$columnTitles[[name]]<-title
                                
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
                            .column_title=list(),
                            
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

                              if (is.character(fun))
                                   if (!(fun %in% names(private$.estimator))) 
                                        return(TRUE)

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
                                
                                output<-try_hard(private$.estimator[[fun]]())
                                rtable<-output$obj
                                error<-output$error
                                warning<-output$warning
                                
                                if (error!=FALSE) {
                                  ginfo("TABLES: Error in ",fun,error)
                                  self$table$setError(error)
                                  return()
                                }
                                
                                if (warning!=FALSE) {
                                  if ("notes" %in% names(self$table)) {
                                      n<-list()
                                      n[[tob64(warning)]]<-warning
                                      private$.setNote(n)
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
                                .titles<-fromb64(names(rtable))
                              
                              .types<-unlist(lapply(rtable,class))
                              .types<-gsub("numeric","number",.types)
                              .types<-gsub("integer","number",.types)
                              .types<-gsub("factor","text",.types)
                              .present<-names(self$table$columns)
                              .names<-setdiff(.names,.present)
                              
                                k<-self$expandFrom-1
                                for (i in seq_along(.names)) {
                                  j<-i+k
                                  cb<-FALSE
                                  if (inherits(self$combineBelow,"integer")) {
                                         if(i %in% self$combineBelow) cb<-TRUE
                                  } else
                                         if (.names[i] %in% c("new!",self$combineBelow)) cb<-TRUE
                                         
                                  self$table$addColumn(index=j,
                                                       name = .names[[i]], 
                                                       title = .titles[[i]], 
                                                       superTitle = self$expandSuperTitle, 
                                                       type=.types[i],
                                                       combineBelow=cb)
                                }
                                
                                private$.new_columns<-.names
                                
                            },
                            .setColumnTitle=function() {
                              what<-names(self$table$columns)
                              
                              for (col in names(self$columnTitles))
                                   if (col %in% what)
                                      self$table$getColumn(col)$setTitle(self$columnTitles[[col]])
                              
                            },
                            .spaceBy=function() {
                              
                              k<-names(self$table$asDF)[1]
                              
                              for (j in self$spaceAt) {
                                mark(str(self$table))
                                if (j<0) j<-self$table$rowCount+j
                                self$table$addFormat(rowNo=j,col=k,jmvcore::Cell.END_GROUP)
                                self$table$addFormat(rowNo=j+1,col=k,jmvcore::Cell.BEGIN_GROUP)
                                
                              }
                              
                              if (is.null(self$spaceBy))
                                   return()
                              
                              if (self$spaceBy==0)
                                return()
                              
                              try_hard({
                                
                                if (self$spaceBy=="new!")
                                     .spaceBy=private$.new_columns
                                else 
                                     .spaceBy=self$spaceBy
                                
                                jnames<-names(self$table$columns)

                                for (sb in .spaceBy) {
                                  col<-self$table$asDF[[sb]]
                                  rows<-unlist(lapply(unlist(unique(col)),function(x) min(which(col==x))))
                                  rows<-which(!col==c(col[-1],col[[length(col)]]))+1
                                  
                                  if (self$spaceMethod=="cb" & length(rows)==length(col)-1)
                                    rows<-NULL
                                
                                  for (j in rows) {
                                    mark(j)
                                    self$table$addFormat(rowNo=j-1,col=k,jmvcore::Cell.END_GROUP)
                                    self$table$addFormat(rowNo=j,col=k,jmvcore::Cell.BEGIN_GROUP)
                                  }

                                }
                                  
                              })
                              
                            },
                            .ci=function() {
                              
                              if (!is.something(self$ci_info))
                                return()
                              
                              for (info in self$ci_info) {

                                .label <-paste(info$label,info$format)
                                .name  <-info$root
                                l<-paste0(.name,".ci.lower")
                                u<-paste0(.name,".ci.upper")
                                if (l %in% names(self$table$columns))
                                  self$table$getColumn(l)$setSuperTitle(jmvcore::format(.label, info$width))
                                if (u %in% names(self$table$columns))
                                  self$table$getColumn(u)$setSuperTitle(jmvcore::format(.label, info$width))
                                
                              }
                            }, 
                            .setNote=function(note) {
                              id<-names(note)
                              note<-as.character(note)
                              self$table$setNote(id,note)
                              state<-self$table$state
                              state$notes[[id]]<-note
                              self$table$setState(state)
                            },

                            .listify = function(adata) {
                              
                              if (is.null(adata)) {
                                # if ("notes" %in% names(self$table)) {
                                #   .len<-length(self$table$notes)
                                #   private$.setNote("No result found")
                                # }
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
                            },
                            .nice_name=function(aname) {
                              a<-stringr::str_replace_all(aname,'[\\]"\\[]',"")
                              a<-strsplit(a,".",fixed = T)
                              a<-make.names(a)
                              paste(a,collapse = ".")
                            },
                            .set_topics=function() {
                              
                              test<-TRUE
                              who<-self$table
                              .topics<-list(private$.nice_name(who$name))
                              while(test) {
                                if (inherits(who$parent,c("Group","Array"))) {
                                  .topics<-append_list(.topics,private$.nice_name(who$parent$name))
                                  who<-who$parent
                                  test<-TRUE
                                } else
                                  test<-FALSE
                              }
                              
                              .topics<-.topics[-length(.topics)]
                              l<-length(.topics)
                              i<-1
                              while(i<=l) {
                                self$topics<-append_list(self$topics,paste(.topics[l:i],collapse = "_"))
                                i<-i+1
                              }
                              
                              if (is.null(self$key))
                                 return()
                              .key<-paste(self$key,collapse = ".")
                              test<-which(.topics %in% .key)
                              .topics<-.topics[-test]
                              l<-length(.topics)
                              i<-1
                              while(i<l) {
                                self$topics<-append_list(self$topics,paste(.topics[l:i],collapse = "_"))
                                i<-i+1
                              }
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
                              if (hasName(tables,"itemKeys"))
                                    self$children<-tables$itemKeys 
                              else 
                                if (hasName(tables,"itemNames"))
                                  self$children<-tables$itemNames 
                                

                              
                            },
                            initTable=function() {
                              
                              if (private$.stop())
                                return()
                              
                              tinfo("TABLES: array",self$nickname,"initiating")
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
                                  aSmartArray$initFunction(rtables[[i]])
                                  aSmartArray$key<-.keys[[i]]
                                  self$childrenObjs<-append_list(self$childrenObjs,aSmartArray)
                                  
                                } else { 
                                  
                                  ### if we are here, children are tables
                                  aSmartTable<-SmartTable$new(jtable,self)
                                  aSmartTable$initFunction(rtables[[i]])
                                  self$childrenObjs<-append_list(self$childrenObjs,aSmartTable)
                                  
                                }
                              }
                              for (obj in self$childrenObjs)
                                obj$initTable()
                              
                              tinfo("TABLES: array",self$nickname,"inited")
                              
                            },
                            
                            runTable=function() {
                              
                              private$.phase<-"run"
                              tinfo("TABLES: array",self$nickname,"checked for run")
                              self$retrieveNotes()

                              if (private$.stop())
                                return()
                              
                              tinfo("TABLES: array",self$nickname,"run")
                              
                              rtables<-private$.getData()
                              
                              for (i in seq_along(self$childrenObjs)) {
                                obj<-self$childrenObjs[[i]]
                                obj$runFunction(rtables[[i]])
                                obj$runTable()
                                
                              }
                              
                            },
                            retrieveNotes=function() {

                              for (child in self$childrenObjs)
                                child$retrieveNotes()
                              
                            },
                              
                            setNotes=function(dispatcher=NULL) {
                               for (child in self$childrenObjs)
                                    child$setNotes(dispatcher)
                            }
                            
                            
                            
                            
                          ), ## end of public
                          private=list(
                            .ci=NULL,
                            .ciwidth=NULL,
                            .ciformat=NULL
                            
                            
                            
                          ) #end of private
) # end of class