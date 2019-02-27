is.something<- function(x,...) UseMethod(".is.something")

.is.something.default<-function(obj) (!is.null(obj))

.is.something.list<-function(obj) (length(obj)>0)

.is.something.numeric<-function(obj) (length(obj)>0)

.is.something.character<-function(obj) isTRUE(length(obj)>0 | any(obj!=''))

.is.something.logical<-function(obj) !is.na(obj)
