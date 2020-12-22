

ginfo<-function(what=NULL,obj=NULL) {
  if (GAMLj_INFO) {
    if (!is.null(what))
        print(what)
    if (!is.null(obj)) {
      print(obj)
      cat("------------\n")
    }
  }
}

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

.listdeep<-function(aList,n=0) {
  if (!inherits(aList,"list"))
    return(n)
  max(sapply(aList,.listdeep,n+1))
}

.keepShape<-function(mat) {
  if (is.null(dim(mat)))
    mat<-t(as.matrix(mat))
  mat  
}



is.something<- function(x,...) UseMethod(".is.something")

.is.something.default<-function(obj) (!is.null(obj))

.is.something.list<-function(obj) (length(obj)>0)

.is.something.numeric<-function(obj) (length(obj)>0)

.is.something.character<-function(obj) (length(obj)>0)

.is.something.logical<-function(obj) !is.na(obj)




