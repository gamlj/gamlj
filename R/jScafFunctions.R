
tinfo <- function(...) {
  if (t_INFO) {
    cat(paste(list(...)))
    cat("\n")
  }
}


ginfo <- function(...) {
  if (j_INFO) {
    cat(paste(list(...)))
    cat("\n")
  }
}

gstart <- function(...) {
  if (j_INFO) {
    cat(paste(paste(list(...)),"..."))
  }
}
gend <- function() {
  if (j_INFO) {
    cat(".......................................done\n")
  }
}


mark <- function(...) {
  if (!j_DEBUG) 
    return()
  
  if (missing(...))
    cat("Mark here\n")
  items<-list(...)
  
  if (length(items)>1)  cat("______begin________\n\n")
  for (a in items)
    if (is.character(a))
      cat(a,"\n")
  else
    print(a)
  if (length(items)>1)  cat("_____end_______\n\n")
  
}

is.something <- function(x, ...) UseMethod(".is.something")

.is.something.default <- function(obj) (!is.null(obj))

.is.something.list <- function(obj) (length(obj) > 0)

.is.something.numeric <- function(obj) (length(obj) > 0)

.is.something.character <- function(obj) (length(obj) > 0)

.is.something.logical <- function(obj) !is.na(obj)

is.there<-function(pattern,string) length(grep(pattern,string,fixed=T))>0

#### This function run an expression and returns any warnings or errors without stopping the execution.
#### It does not reterun the results, so the expr should assign a valut to the results
#### something like try_hard({a<-3^2}) and not a<-try_hard(3^2)

try_hard<-function(exp) {

  .results<-list(error=FALSE,warning=FALSE,message=FALSE,obj=FALSE)
  
  .results$obj <- withCallingHandlers(
    tryCatch(exp, error=function(e) {
       mark("SOURCE:")
       mark(conditionCall(e))
      .results$error<<-conditionMessage(e)
      NULL
    }), warning=function(w) {
      .results$warning<<-conditionMessage(w)
      invokeRestart("muffleWarning")
    }, message = function(m) {
      .results$message<<-conditionMessage(m)
      invokeRestart("muffleMessage")
    })

  
  if (!isFALSE(.results$error)) {
               mark("CALLER:")
               mark(rlang::enquo(exp))
               mark("ERROR:")
               mark(.results$error)
  }
  

  return(.results)
}

sourcifyOption<- function(x,...) UseMethod(".sourcifyOption")

.sourcifyOption.default=function(option,def=NULL) {
  
  if (option$name == 'data')
    return('data = data')
  
  if (startsWith(option$name, 'results/'))
    return('')
  
  value <- option$value
  def <- option$default
  
  if ( ! ((is.numeric(value) && isTRUE(all.equal(value, def))) || base::identical(value, def))) {
    valueAsSource <- option$valueAsSource
    if ( ! identical(valueAsSource, ''))
      return(paste0(option$name, ' = ', valueAsSource))
  }
  ''
}
.sourcifyOption.OptionVariables<-function(option,def=NULL) {
  
  if (is.null(option$value))
     return('')
  
  values<-sourcifyName(option$value)
  
  if (length(values)==1)
     return(paste0(option$name,"=",values))
  else
    return(paste0(option$name,"=c(",paste0(values,collapse = ","),")"))
}
  
.sourcifyOption.OptionTerms<-function(option,def=NULL)
     .sourcifyOption.default(option,def)
  
.sourcifyOption.OptionArray<-function(option,def=NULL) {
  alist<-option$value
  if (length(alist)==0)
      return('')
  if (is.something(def) & option$name %in% names(def)) {
    test<-all(sapply(alist,function(a) a$type)==def[[option$name]])
    if (test)
    return("")
  }
  paste0(option$name,"=c(",paste(sapply(alist,function(a) paste0(sourcifyName(a$var),' = \"',a$type,'\"')),collapse=", "),")")
}


sourcifyName<-function(name) {
  
  what<-which(make.names(name)!=name)
  for (i in what)
    name[[i]]<-paste0('"',name[[i]],'"')
  name
}

sourcifyVars<-function(value) {
  
  paste0(sourcifyName(value),collapse = ",")
  
}
append_list <- function(alist, aelement, name = NULL) {
  alist[[length(alist) + 1]] <- aelement
  if (!is.null(name)) 
    names(alist)[length(alist)] <- name
  alist
}
prepend_list <- function(alist, aelement, name = NULL) {
  alist <- c(0, alist)
  alist[[1]] <- aelement
  if (!is.null(name)) 
    names(alist)[1] <- name
  alist
}

listify <- function(adata) {
  res <- lapply(1:dim(adata)[1], function(a) as.list(adata[a, ]))
  names(res) <- rownames(adata)
  res
}            

smartTableName<-function(root,alist,end=NULL) {
    paste(root,make.names(paste(alist,collapse = ".")),end,sep="_")
}


transnames<-function(original,ref) {
  unlist(lapply(original,function(x) {
    i<-names(ref)[sapply(ref,function(y) any(y %in% x))]
    ifelse(length(i)>0,i,x)
  }))
}
