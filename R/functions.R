
mark<-function(what=NULL,obj=NULL) {
  if (GAMLj_DEBUG) {
    if (!is.null(what))
      print(what)
    else print("you got here")
    
    if (!is.null(obj)) {
      print(obj)
      print("#### end ###")
    }
  }
}

c.real<-function(...) {
  obj <- c(...)
  obj[!is.null(obj)]
}

