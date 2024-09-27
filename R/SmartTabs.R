### for warnings and error, tables responds to:
###  tablename
###  groupname 
###  groupname_tablename
###  arrayname
###  arrayname_tablename
###  arrayname_tablename_[n] n is the index of the table in the array
###  This is version 0.9.2



SmartTable <- R6::R6Class("SmartTable",
                          cloneable=FALSE,
                          class=TRUE,
                          public=list(
                            name=NULL,
                            table=NULL,
                            topics=NULL,
                            nickname=NULL,
                            expandOnInit=FALSE,
                            expandOnRun=FALSE,
                            expandSuperTitle=NULL,
                            expandFrom=1,
                            activated=NULL,
                            key=NULL,
                            keys_sep=NULL,
                            keys_raise=TRUE,
                            spaceBy=NULL,
                            spaceAt=NULL,
                            spaceMethod="cb",
                            indent=NULL,
                            combineBelow=0,
                            ci_info=list(),
                            columnTitles=list(),
                            mutenotes=FALSE,
                            initialize=function(table,estimator=NULL) {

                              if (exists("t_INFO")) private$.debug<-t_INFO
                              
                              OK<-FALSE

                              if(!inherits(table,"ResultsElement"))
                                  stop("Table does not exits")

                              self$name       <-  table$name
                              self$table      <-  table
                              self$nickname   <-  gsub('"','',gsub("/","_",table$path,fixed = TRUE),fixed=TRUE)
                              self$nickname   <-  stringr::str_replace_all(self$nickname,'[\\]"\\[]',"")
                              self$nickname   <-  make.names(self$nickname)
                              private$.init_source<-paste0("init_",self$nickname)
                              private$.run_source<-paste0("run_",self$nickname)
                              
                              self$activated<-self$table$visible
                              
                              if ("key" %in% names(table) ) 
                                self$key<-table$key
                              
                              private$.estimator  <-   estimator

                              if (inherits(private$.estimator,"SmartArray")) {
                                ## here the estimator is not group or array of results
                                self$activated              <- private$.estimator$activated
                                self$expandOnInit           <- private$.estimator$expandOnInit
                                self$expandOnRun            <- private$.estimator$expandOnRun
                                self$expandFrom             <- private$.estimator$expandFrom
                                self$expandSuperTitle       <- private$.estimator$expandSuperTitle
                                self$ci_info                <- private$.estimator$ci_info
                                self$keys_sep               <- private$.estimator$keys_sep
                                self$keys_raise             <- private$.estimator$keys_raise
                                self$combineBelow           <- private$.estimator$combineBelow
                                self$spaceMethod            <- private$.estimator$spaceMethod
                                self$spaceAt                <- private$.estimator$spaceAt
                                self$spaceBy                <- private$.estimator$spaceBy
                                self$columnTitles           <- private$.estimator$columnTitles
                                self$indent                 <- private$.estimator$indent                            
                                self$activateOnData         <- private$.estimator$activateOnData
                                self$hideOn                 <- private$.estimator$hideOn
                                
                              }
                              ## some coherence setting
                              if (isTRUE(self$activateOnData)) self$activated<-TRUE

                            },
                            initTable=function() {

                              private$.debug_msg("checked for init.")
                              
                              # prepare some fast stuff anyway, because the table may be already saved 
                              # and should be inited properly
                              if (isFALSE(self$activated)) 
                                 return()
                              
                              private$.setColumnTitle()
                              private$.ci()
                              self$title

                              ### fill with initial values###

                              rtable<-private$.getData()
                              
                              if (is.null(rtable))
                                  return()

                              ### expand it if needed
                              if (self$expandOnInit) private$.expand(rtable)
                              private$.fill(self$table,rtable)
                              private$.indent()
                              private$.spaceBy()
                              ## in case is a go, the table may be invisible (if activatedOnData). turn visibility on
                              self$table$setVisible(TRUE)
                              private$.debug_msg("inited")
                            },
                            
                            runTable=function() {
                            
                              private$.debug_msg("checked for run")

                              if (!self$activated) {
                                private$.debug_msg("Not run Not activated")
                                return()
                              }
                              
                              if (self$table$isFilled() && self$table$rowCount>0) {
                                  private$.debug_msg("Not run: protBuff available")
                                  self$cleanNotes()
                                  return()
                              }

                              private$.phase<-"run"
                              rtable<-private$.getData()
                         
                              self$cleanNotes()
                              if (is.null(rtable)) {
                                  private$.debug_msg("Not run: not filling data")
                                  return()
                              }
                              
                              ### check if new column titles are passed
                              .attr <- private$.getAttributes(rtable)
                              if (utils::hasName(.attr,"titles"))
                                  for (.name in names(.attr$titles)) {
                                         self$setColumnTitle(.name,.attr$titles[[.name]])
                                  }

                              if (self$expandOnRun) private$.expand(rtable)
                              
                              private$.fill(self$table,rtable)
                              private$.finalize()
                              private$.debug_msg("run")

                            },
                            
                            ci=function(aroot,width=95,label=NULL,format="{}% Confidence Intervals"){
                              
                              if (is.null(label)) label="" 
                              alist<-list(root=aroot,label=label,width=width,format=format)
                              ladd(self$ci_info)<-alist

                            },
                            cleanNotes=function(dispatcher=NULL) {

                                  notes<-self$table$notes
                                  lapply(notes,function(x) {
                                    if (self$mutenotes)
                                       self$table$setNote(x$key,NULL)
                                    else
                                       if (isTRUE(x$init))
                                          self$table$setNote(x$key,NULL)
                                  })

                            },
                            
                            setColumnTitle=function(name,title) {
                              
                                self$columnTitles[[name]]<-title
                                
                            }
                            
                          ), ## end of public
                          active=list(
                            initSource= function(value) {
                              
                              if (missing(value)) 
                                private$.init_source 
                              else 
                                private$.init_source<-value 
                            },
                            runSource= function(value) {
                              
                              if (missing(value)) 
                                private$.run_source 
                              else 
                                private$.run_source<-value 
                              
                            },
                            
                            activateOnData=function(value) {
                              
                              if (missing(value)) {
                               return(private$.activateOnData) 
                              }
                              private$.activateOnData<-value
                              if (isTRUE(value)) self$activated<-TRUE                    
                            },
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
                            },
                            setColumnVisible=function(varnames) {
                              
                              if (missing(varnames))
                                  return()
                              
                              for (name in varnames)
                                self$table$getColumn(name)$setVisible(TRUE)
                            },
                            hideOn=function(alist) {
                              
                              if (missing(alist))
                                 private$.hideOn
                              else
                                private$.hideOn<-alist
                            } ,
                            debug=function(value) {
                              if (missing(value))
                                   private$.debug
                              else
                                   private$.debug<-value
                            }
                            
                          ), #end of active
                          private=list(
                            .error=NULL,
                            .estimator=NULL,
                            .init_source=NULL,
                            .run_source=NULL,
                            .superTitle=NULL,
                            .phase="init",
                            .new_columns=NULL,
                            .activateOnData=FALSE,
                            .column_title=list(),
                            .hideOn=NULL,
                            .debug=FALSE,
                            .getData=function(when) {
                              
                              ## check in which phase we are
                              if (private$.phase=="init")
                                fun<-private$.init_source
                              else
                                fun<-private$.run_source

                              ### check how to retrieve the data
                              if (inherits(fun,"character") ) {
                                
                                if (!(fun %in% names(private$.estimator)))
                                    return(NULL)
                                
                                if ("ok" %in% names(private$.estimator))
                                    if (isFALSE(private$.estimator$ok))
                                        return(NULL)
                                
                                output<-try_hard(private$.estimator[[fun]]())
                                rtable<-output$obj
                                error<-output$error
                                warning<-output$warning
                                
                                if (!isFALSE(error)) {
                                  dispatch<-Dispatch$new(self)
                                  private$.debug_msg("ERROR",fun,error)
                                  if (exists("fromb64")) error<-fromb64(error)
                                  error<-dispatch$translate(error)
                                  self$table$setError(error)
                                  private$.error<-TRUE
                                   return()
                                }
                                if (!isFALSE(warning)) {
                                    dispatch<-Dispatch$new(self)
                                    warning<-lapply(warning, function(x) {
                                        if (exists("fromb64")) 
                                             x<-fromb64(x)
                                        dispatch$translate(x)
                                    })
                                    
                                    warning<-warning[sapply(warning,function(x) is.something(x))]
                                    
                                    if (inherits(self$table,"Table")) 
                                      for (w in warning)
                                         self$table$setNote(jmvcore::toB64(w),w,init=FALSE)
                                      
                                    if (inherits(self$table,"Array"))
                                      for (obj in self$table$items)
                                          for (w in warning)
                                              obj$setNote(jmvcore::toB64(w),w,init=FALSE)

                                }
                                return(rtable) 
                              }
                              if (inherits(fun,"function") ) {
                                private$.debug_msg("function is ",class(fun))
                                return(fun())
                              }
                              ## if here, fun is a table (data.frame or list)
                              private$.debug_msg("function is ",class(fun))
                              return(fun)

                              
                              
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
                            .finalize=function() {
                              
                              private$.setColumnTitle()
                              private$.setHideOn()
                              self$table$setVisible(TRUE)

                            },
                            .setHideOn=function() {
                              
                              if (is.something(private$.hideOn)) {
                                rtable<-self$table$asDF
                                what<-names(rtable)
                                for (col in names(private$.hideOn))
                                  if (col %in% what) {
                                    test<-all(rtable[[col]] %in% private$.hideOn[[col]])
                                    if (test) 
                                      self$table$getColumn(col)$setVisible(FALSE)
                                    else
                                      self$table$getColumn(col)$setVisible(TRUE)
                                  }
                              }
                            },
                            .expand=function(rtable) {
                              
                              rtable<-private$.framefy(rtable)
                              .names    <-  names(rtable)
                              .types<-unlist(lapply(rtable,class))

                              .types<-gsub("numeric","number",.types)
                              .types<-gsub("integer","number",.types)
                              .types<-gsub("factor","text",.types)
                              .present<-names(self$table$columns)
                              .ncols<-length(.present)
                              .names<-setdiff(.names,.present)
                              if (is.something(attr(rtable,"types")))
                                .types<-attr(rtable,"types")

                              if (is.something(attr(rtable,"titles")))
                                .titles<-attr(rtable,"titles")
                              else 
                                 if (exists("fromb64"))
                                           .titles<-fromb64(.names)
                                 else 
                                            .titles<-.names
                              
                                k<-self$expandFrom-1

                                for (i in seq_along(.names)) {
                                  j<-i+k
                                  cb<-FALSE
                                  if (inherits(self$combineBelow,"integer")) {
                                         if(i %in% self$combineBelow) cb<-TRUE
                                  } else {
                                         if (.names[i] %in% c("new!",self$combineBelow)) cb<-TRUE
                                  }

                                  if (j>.ncols) j<-NA
                                  self$table$addColumn(index=j,
                                                       name = .names[[i]], 
                                                       title = .titles[[i]], 
                                                       superTitle = self$expandSuperTitle, 
                                                       type=.types[i],
                                                       combineBelow=cb)
                                  .ncols<-length(self$table$columns)
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
                              try_hard({
                              for (j in self$spaceAt) {
                                if (j<0) j<-self$table$rowCount+j
                                if (j==0 || j>self$table$rowCount)
                                  next
                                
                                self$table$addFormat(rowNo=j,col=k,jmvcore::Cell.END_GROUP)
                                self$table$addFormat(rowNo=j+1,col=k,jmvcore::Cell.BEGIN_GROUP)
                              }})
                              
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
                                    self$table$addFormat(rowNo=j-1,col=k,jmvcore::Cell.END_GROUP)
                                    self$table$addFormat(rowNo=j,col=k,jmvcore::Cell.BEGIN_GROUP)
                                  }

                                }
                                  
                              })
                              
                            },
                            .indent=function() {

                                 if (!is.null(self$indent)) {
                                    trows<-1:length(self$table$getRows())

                                    if (length(trows)==0)
                                         return()
                                    
                                    rows<-trows[self$indent]
                                    for (j in rows)
                                       self$table$addFormat(rowKey=j,col=1,jmvcore::Cell.INDENTED)
                                 }  

                            },
                            .ci=function() {
                              
                              if (!is.something(self$ci_info))
                                return()
                        
                              for (info in self$ci_info) {

                                .label <-paste(info$label,info$format)
                                .name  <-info$root
                                tail1<-".ci.lower"
                                tail2<-".ci.upper"
                                if (length(.name)==0) {
                                             tail1<-"ci.lower"
                                             tail2<-"ci.upper"
                                }
                                l<-paste0(.name,tail1)
                                u<-paste0(.name,tail2)
                                if (l %in% names(self$table$columns))
                                  self$table$getColumn(l)$setSuperTitle(jmvcore::format(.label, info$width))
                                if (u %in% names(self$table$columns))
                                  self$table$getColumn(u)$setSuperTitle(jmvcore::format(.label, info$width))
                                
                              }
                            }, 

                            .listify = function(adata) {
                             
                              if (is.null(adata)) {
                                return()
                              }
                           
                              .attr<-private$.getAttributes(adata)
                              .res<-NULL
                              if (inherits(adata,"data.frame")) {
                                
                                for (var in names(adata))
                                    if (is.factor(adata[[var]])) 
                                         adata[[var]]<-as.character(adata[[var]])
                                
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
                            .debug_msg=function(...) {
                              if (private$.debug) {
                                msg<-paste(list(...))
                                cat(paste0("(",class(self)[1],")"),self$nickname,":",msg)
                                cat("\n")
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

                              private$.debug_msg("check for init")

                              if (isFALSE(self$activated))
                                return()

                              self$table$setVisible(TRUE)
                              self$title

                              rtables<-private$.getData()
                              if (is.null(rtables))
                                return()
                              
                              .keys<-names(rtables)
                              if (is.something(attr(rtables,"keys")))
                                    .keys<-attr(rtables,"keys")


                              if (!is.something(self$table$items)) {
                                library(jmvcore)
                                  
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
                                  aSmartArray$initSource<-rtables[[i]]
                                  aSmartArray$key<-.keys[[i]]
                                  ladd(self$childrenObjs)<-aSmartArray
                                  
                                } else { 
                                  
                                  ### if we are here, children are tables
                                  aSmartTable<-SmartTable$new(jtable,self)
                                  aSmartTable$initSource<-rtables[[i]]
                                  ladd(self$childrenObjs)<-aSmartTable
                                  
                                }
                              }
                              for (obj in self$childrenObjs)
                                obj$initTable()

                              private$.debug_msg("inited")
                              
                            },
                            
                            runTable=function() {

                              private$.phase<-"run"
                              private$.debug_msg("checked for run")

                              if (private$.stop()) {
                                  private$.debug_msg("not run: filled or empty")
                                  self$cleanNotes()
                                  return()
                              }
                              
                              if (!is.something(self$childrenObjs))
                                self$initTable()
                              
                              rtables<-private$.getData()
                              self$cleanNotes()
                              nfound<-length(rtables)
                              for (i in seq_along(self$childrenObjs)) {
                                 obj<-self$childrenObjs[[i]]
                                 if (i<=nfound) {
                                     obj$runSource<-rtables[[i]]
                                     obj$runTable()
                                 } else {
                                   if (!private$.error)
                                         obj$table$setVisible(FALSE)
                                 }
                             }
                              private$.debug_msg("run")
                            },
                            cleanNotes=function() {
                              
                              for (child in self$childrenObjs) {
                                    child$cleanNotes()
                              }
                              
                            }
                              
                            
                          ), ## end of public
                          private=list(
                            .ci=NULL,
                            .ciwidth=NULL,
                            .ciformat=NULL,
                            .stop=function() {

                              if (private$.activateOnData)
                                return(FALSE)
                              if (!self$activated)
                                return(TRUE)
                              
                              if (private$.phase=="init") {
                                fun<-private$.init_source
                                filled<-FALSE
                              } else {
                                
                                fun<-private$.run_source
                                filled<-(self$table$isFilled() && is.something(self$table$items))
                              }
                              
                              if (is.null(fun)) 
                                return(TRUE)
                              
                              if (is.character(fun))
                                if (!(fun %in% names(private$.estimator))) 
                                  return(TRUE)
                              
                              return(filled)
                              
                            }
                            
                            
                            
                            
                          ) #end of private
) # end of class
