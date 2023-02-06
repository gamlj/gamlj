

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

.fromb64.formula<-function(obj)   return(formula(fromb64(deparse(obj))))


.fromb64.list<-function(obj,ref=NULL) lapply(obj,bogusfromb64,ref=ref)


bogusfromb64<-function(obj,ref=NULL) fromb64(obj,ref=ref)


is.b64<-function(a) { 
    test1  <-  is.there(B64_SYMBOL,a)
    test2  <-  is.there(LEVEL_SYMBOL,a)
    test3  <-  is.there(FACTOR_SYMBOL,a)
    test4  <-  is.there(INTERACTION_SYMBOL,a) 
    any(test1,test2,test3,test4)
}

### basic functions ###



