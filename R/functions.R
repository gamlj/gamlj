

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
    paste0(B64_SYMBOL,obj)
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
    
    if (!is.b64(obj))
          return(obj)
    
    obj<-gsub(B64_SYMBOL,"",obj,fixed=T)
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


is.b64<-function(a) ifelse(length(grep(B64_SYMBOL,a,fixed = TRUE))>0,TRUE,FALSE)

is.listOfList<-function(obj) {

    if (inherits(obj,"list")) {
       child<-obj[[1]]
       return(inherits(obj,"list"))
    }
  return(FALSE)
}
  
