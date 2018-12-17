library(R6)

names64 <- R6Class("names64",list(
                         .factors = list(),
                         .covs= list(),
                         .labels= list(),
                         .contrasts=list(),
                         addFactor = function(variable,levels) {
                           var64<-jmvcore::toB64(variable)
                           self$.factors[[var64]]<-variable
                           self$.contrasts[[var64]]<-list()
                           for(i in seq_along(levels[-1])) {
                             name64<-paste0(var64,i)
                             name<-paste0(variable,i)
                             self$.factors[[name64]]<-name
                             name<-paste0(var64,"_._._",i)
                             self$.contrasts[[var64]][i]<-name
                           }
                           ## this is for lme4 random names without intercept
                           for(a in levels) {
                             name64<-paste0(var64,a)
                             name<-paste0(variable,a)
                             self$.factors[[name64]]<-name
                           }
                           
                         },
                         addLabel=function(variable,labels) {
                           var64<-jmvcore::toB64(variable)
                           for (i in seq_along(labels)) {
                             name<-paste0(var64,"_._._",i)
                             self$.labels[[name]]<-labels[[i]]
                           }
                         },
                         nicelabels=function(obj) {
                           q<-sapply(obj,function(x) {
                             a<-unlist(strsplit(x,":",fixed = T))
                             unlist(sapply(a, function(x) self$.onelabel(x)))
                           })
                           return(q)
                         },
                         
                         addVar=function(variable) {
                           var64<-jmvcore::toB64(variable)
                           self$.covs[[var64]]<-variable
                         },
                         nicenames=function(obj) {
                                 sapply(obj,function(x) {
                                 a<-unlist(strsplit(x,":",fixed = T))
                                 unlist(sapply(a, function(x) self$.onename(x)))
#                                 paste(b,collapse = ":")    
                                 })
                                 },
                         contrasts=function(var) {
                           self$.contrasts[[jmvcore::toB64(var)]]
                         },
                         contrastsLabels=function(var) {
                           conts<-self$.contrasts[[jmvcore::toB64(var)]]
                           self$nicelabels(conts)
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
                                   # first we deal with higher oders
                                   tochange<-regmatches(var64, gregexpr("I\\(.*?\\^.*\\)", var64))
                                   highertest<-any(sapply(tochange, length)) 
                                   if (highertest) {
                                       var64<-gsub("^I\\(", "", gsub("\\^.*", "", tochange))
                                       apex<-gsub("\\^","",regmatches(tochange,gregexpr("\\^[[:digit:]]",tochange)))
                                       return(rep(self$.covs[[var64]],apex))
                                   }
                                  # then we deal with factor contrasts
                                  x<-unlist(strsplit(var64,"_._._",fixed = T))
                                  if (length(x)==2) 
                                         return(paste0(jmvcore::fromB64(x[1]),x[2]))
                                  ### then we search for names
                                  if (var64 %in% names(self$.factors))   
                                         return(self$.factors[[var64]])
                                  if (var64 %in% names(self$.covs))   
                                         return(self$.covs[[var64]])
                                  return(var64)                         
                         },
                         .onelabel=function(obj) {
                           
                           if (obj %in% names(self$.labels))  
                                return(self$.labels[[obj]])
                           if (obj %in% names(self$.covs))
                                return(self$.covs[obj])
                           return(self$.onename(obj))
                           },
                         print=function() {
                           print(self$.factors)
                           print(self$.covs)
                           print(self$.labels)
                           print(self$.contrasts)
                           
                         }
                       )

)

