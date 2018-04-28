library(jmvcore)
library(R6)

names64 <- R6Class("names64",list(
                         .factors = list(),
                         .covs= list(),
                         .labels= list(),
                         addFactor = function(variable,levels) {
                           var64<-toB64(variable)
                           self$.factors[[var64]]<-variable
                           for(i in levels) {
                             name64<-paste0(var64,i)
                             name<-paste0(variable,i)
                             self$.factors[[name64]]<-name
                           }
                         },
                         addLabel=function(variable,labels) {
                           var64<-toB64(variable)
                           for (i in seq_along(labels)) {
                             name<-paste0(var64,"_._._",i)
                             self$.labels[[name]]<-labels[[i]]
                           }
                         },
                         nicelabels=function(obj) {
                           q<-sapply(obj,function(x) {
                             a<-unlist(strsplit(x,":",fixed = T))
                             b<-sapply(a, function(x) self$.onelabel(x))
                             paste(b,collapse = ":")    
                           })
                           return(q)
                         },
                         
                         addVar=function(variable) {
                           var64<-toB64(variable)
                           self$.covs[[var64]]<-variable
                         },
                         nicenames=function(obj) {
                                 sapply(obj,function(x) {
                                 a<-unlist(strsplit(x,":",fixed = T))
                                 b<-sapply(a, function(x) self$.onename(x))
                                 paste(b,collapse = ":")    
                                 })
                                 },
                         translate=function(astring) {
                           res<-astring
                           for (a in names(self$.factors)) 
                             res<-gsub(a,self$.factors[[a]],res)
                           for (a in names(self$.covs)) 
                             res<-gsub(a,self$.covs[[a]],res)
                           return(res)
                         },
                         .onename=function(var64) {
                                  x<-unlist(strsplit(var64,"_._._",fixed = T))
                                  if (length(x)==2) 
                                         return(paste0(fromB64(x[1]),x[2]))
                                  if (var64 %in% names(self$.factors))   
                                         return(self$.factors[[var64]])
                                  if (var64 %in% names(self$.covs))   
                                         return(self$.covs[[var64]])
                                  return(var64)                         
                         },
                         .onelabel=function(obj) {
                           if (obj %in% names(self$.labels))  
                                return(self$.labels[obj])
                           if (obj %in% names(self$.covs))
                                return(self$.covs[obj])
                           return(self$.onename(obj))
                           },
                         print=function() {
                           print(self$.factors)
                           print(self$.covs)
                           print(self$.labels)
                         }
                       )

)

