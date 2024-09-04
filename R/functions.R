
########### names ###########

tob64<- function(x,...) UseMethod(".tob64")

.tob64.default<-function(obj,ref=NULL) {
    
    
    if (is.null(obj))
        return()
    if (is.numeric(obj))
       return(obj)
  
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
  
  bregex<-paste0("(?<=",B64_REGEX,")\\w+")
  matches64<-stringr::str_extract_all(obj,bregex)[[1]]
  matches<-jmvcore::fromB64(matches64)
  astring<-obj
  for (i in seq_along(matches64)) {
      if (!is.na(matches64[i]))
         astring<-stringr::str_replace_all(astring,matches64[i],matches[i])
  }
  astring<-stringr::str_replace_all(astring,B64_SYMBOL,"")
  astring<-stringr::str_replace_all(astring,FACTOR_SYMBOL,"")
  astring<-stringr::str_replace_all(astring,LEVEL_SYMBOL,"")
  astring<-stringr::str_replace_all(astring,INTERACTION_SYMBOL,":")
  astring
}

.fromb64.formula<-function(obj)   return(stats::formula(fromb64(deparse(obj))))


.fromb64.list<-function(obj,ref=NULL) lapply(obj,bogusfromb64,ref=ref)


bogusfromb64<-function(obj,ref=NULL) fromb64(obj,ref=ref)


is.b64<-function(a) { 
    test1  <-  is.there(B64_SYMBOL,a)
    test2  <-  is.there(LEVEL_SYMBOL,a)
    test3  <-  is.there(FACTOR_SYMBOL,a)
    test4  <-  is.there(INTERACTION_SYMBOL,a) 
    any(test1,test2,test3,test4)
}

epretty<-function(min,max,steps) {
  i     <- (max-min)/steps
  seq(min,max,by=i)
}


### This is taken from MASS::ginv(). It is reproduced here to avoid linking to MASS
### which creates some issue in jamovi

.ginv<-function (X, tol = sqrt(.Machine$double.eps)) 
{
    if (length(dim(X)) > 2L || !(is.numeric(X) || is.complex(X))) 
        stop("'X' must be a numeric or complex matrix")
    if (!is.matrix(X)) 
        X <- as.matrix(X)
    Xsvd <- svd(X)
    if (is.complex(X)) 
        Xsvd$u <- Conj(Xsvd$u)
    Positive <- Xsvd$d > max(tol * Xsvd$d[1L], 0)
    if (all(Positive)) 
        Xsvd$v %*% (1/Xsvd$d * t(Xsvd$u))
    else if (!any(Positive)) 
        array(0, dim(X)[2L:1L])
    else Xsvd$v[, Positive, drop = FALSE] %*% ((1/Xsvd$d[Positive]) * 
        t(Xsvd$u[, Positive, drop = FALSE]))
}

## dealing with packages

is.package<-function(what) {
  
  if (stringr::str_length(system.file(package=what))==0)
               stop(what," package is required for the required analysis. Please install it.")  
  
}
