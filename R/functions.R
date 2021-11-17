

ginfo <- function(...) {
    if (j_INFO) {
        cat(paste(list(...)))
        cat("\n")
    }
}

mark <- function(what = NULL, obj = NULL) {
    if (j_DEBUG) {
        if (!is.null(what)) 
            print(what) else print("you got here")

        if (!is.null(obj)) {
            print(obj)
            print("#### end ###")
        }
    }
}



is.something <- function(x, ...) UseMethod(".is.something")

.is.something.default <- function(obj) (!is.null(obj))

.is.something.list <- function(obj) (length(obj) > 0)

.is.something.numeric <- function(obj) (length(obj) > 0)

.is.something.character <- function(obj) (length(obj) > 0)

.is.something.logical <- function(obj) !is.na(obj)


#### This function run an expression and returns any warnings or errors without stopping the execution.
#### It does not reterun the results, so the expr should assign a valut to the results
#### something like try_hard({a<-3^2}) and not a<-try_hard(3^2)

try_hard<-function(exp) {
    results<-list(error=FALSE,warning=FALSE,message=FALSE,obj=FALSE)
    
    results$obj <- withCallingHandlers(
        tryCatch(exp, error=function(e) {
            results$error<<-conditionMessage(e)
            NULL
        }), warning=function(w) {
            results$warning<<-conditionMessage(w)
            invokeRestart("muffleWarning")
        }, message = function(m) {
            results$message<<-conditionMessage(m)
            invokeRestart("muffleMessage")
        })
    
    return(results)
}


sourcifyList<-function(option,def) {
    alist<-option$value
    test<-all(sapply(alist,function(a) a$type)==def)
    if (test)
        return("")
    paste0(option$name,"=c(",paste(sapply(alist,function(a) paste0(a$var,' = \"',a$type,'\"')),collapse=", "),")")
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


########### names ###########

tob64<- function(x,...) UseMethod(".tob64")

.tob64.default<-function(obj,ref=NULL) {
    
    
    if (is.null(obj))
        return()
    
    
    if (is.null(ref)) {
        obj<-jmvcore::toB64(obj)
    } else {
        for (r in ref) {
            reg<-paste0("(?<=[\\s*~=]|^)",r,"(?=[\\s*~=]|$)")
            obj<-stringr::str_replace_all(obj,reg,jmvcore::toB64(r))
        }
    }
    obj
}

.tob64.list<-function(obj,ref=NULL) {
    lapply(obj,bogustob64,ref)
}

bogustob64<-function(obj,ref) tob64(obj,ref)

fromb64<- function(x,...) UseMethod(".fromb64")


.fromb64.default<-function(obj,ref=NULL) {
    
    if (!is.something(obj))
        return(obj)
    
    if (length(obj)>1)
        return(unlist(sapply(obj, bogusfromb64,ref=ref,USE.NAMES = F)))
    
    int<-strsplit(obj,INTERACTION_SYMBOL,fixed=T)[[1]]
    if (length(int)>1)
        return(paste0(unlist(sapply(int,bogusfromb64,ref=ref)),collapse = ":"))
    
    if (is.null(ref)) {
        fac<-strsplit(obj,FACTOR_SYMBOL,fixed=T)[[1]]
        obj<-fac[[1]]  
        obj<-jmvcore::fromB64(obj)
        if (length(fac)>1) obj<-paste0(obj,fac[[2]])
    } else {
        for (r in ref) 
            obj<-stringr::str_replace_all(obj,paste0("\\b",jmvcore::toB64(r),"\\b"),r)
        
    }
    obj<-gsub(LEVEL_SYMBOL,"",obj,fixed = T)
    obj<-gsub(FACTOR_SYMBOL,"",obj,fixed = T)
    obj<-gsub(INTERACTION_SYMBOL,":",obj,fixed = T)
    
    obj
}

.fromb64.list<-function(obj,ref=NULL) {
    lapply(obj,bogusfromb64,ref=ref)
}

bogusfromb64<-function(obj,ref=NULL) fromb64(obj,ref=ref)

