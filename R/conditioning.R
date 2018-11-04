library(R6)
# this class will take care of conditioning of covariates. Because is jamovi when you need the labels
# you might not have the data (as in .init()), the class would compute the labels even without data.
# when the data are available, it computes the actual conditioning values and, if requested, labels=values

# Conditioning of covariates can come in different forms:
# 1. can be a list of numbers, thus are passed as they are as the "at" levels. Each covariate is conditioned to 
#    those values.
# 2. a list variable, each with labels and values: such as list(x=list("mean-sd"=-10,"mean"=0,"mean+sd"=+10))
#    the list can be partial, the missing labels are substituted with the values passed.
# 4. a list variable, each with labels and values: such as list(x=list("mean-sd"=-10,"mean"=0,"mean+sd"=+10)) 
# 3. one of standard conditioning: mean_sd, percent
# method "mean_sd" and "percent" can be customized with "span" option: for mean_sd span indicates
# how many sd below and above the average should be used. For "percent", the increase and descrease of
# percentile around the median. The option "span" can be a number which applies to all variables, or vector
# or number that applies in order to corresponding variables. Partial vector is fine, the remaining is set to 1

# In jamovi span and method are applied to all covariates. In R can be 

conditioning <- R6Class("conditioning",
        public=list(
        labels_type="labels",
        initialize = function(vars=NULL,method= "mean_sd", span = 1) {
            if(is.null(vars))
                return(self)
            if (is.list(method) & !is.null(names(method))) 
                if (!all(names(method) %in% vars))
                  stop("Conditioning values are assigned to undefined variables")
            
            self$vars<-vars
            private$init(method,span)
        },
        storeValues=function(vardata,varname=NULL,decode=FALSE) {
          
          .storeValues<-function(vardata,varname) {
                 obj<-private$cond_specs[[varname]]
                 if (obj$method=="custom")
                    return()
                 if (obj$method=="mean_sd") {
                    .mean<-mean(vardata)
                    .sd<-sd(vardata)
                    .span<-obj$span
                    private$cond_specs[[varname]]$values=c(.mean-(.span*.sd),.mean,.mean+(.span*.sd))
                 }
                if (obj$method=="percent") {
                    .span<-obj$span/100
                    private$cond_specs[[varname]]$values<-(round(quantile(vardata,c(.5-.span,.5,.5+.span)),digits=3))
                }
          }
          if (decode) {
            if (!is.null(varname))
                varname<-jmvcore::fromB64(varname)
            if (is.data.frame(vardata)) 
                names(vardata)<-jmvcore::fromB64(names(vardata))
          }

          if (is.data.frame(vardata)) {
            for (name in self$vars)
              if (name %in% names(vardata))
                .storeValues(vardata[,name],name)
          } else
                .storeValues(vardata,varname)
        },
        print=function() {
          print(private$cond_specs)
        },
        updateValues=function(var,values,decode=F) {

          if (decode)
             var<-jmvcore::fromB64(var)

          obj<-private$cond_specs[var]
          obj$values<-values
          obj$method<-"custom"
          obj$labels<-round(values,digits=3)
          private$cond_specs[[var]]<-obj
        },
        updateLabels=function(var,values,decode=F) {
          
          if (decode)
            var<-jmvcore::fromB64(var)
          
          obj<-private$cond_specs[var]
          obj$labels<-values
          if (length(obj$labels)!=length(obj$values))
            warning("Updated labels have different dimension than their values")
          private$cond_specs[[var]]<-obj
        },
        
        vars=NULL,
        values=function(var,decode=FALSE) {
          res<-list()
          if (decode)
             var<-jmvcore::fromB64(var)
          
          specs<-private$cond_specs[var]
          for (one in names(specs)) {
            res[[one]]<-specs[[one]]$values
          }
          if (decode)
            names(res)<-jmvcore::toB64(names(res))
          
          res
        },
        labels=function(var,decode=F) {
          type=self$labels_type
          res<-list()
          if (decode)
             var<-jmvcore::fromB64(var)
          
          specs<-private$cond_specs[var]
          if (type=="values")
            for (one in names(specs)) {
              res[[one]]<-round(specs[[one]]$values,digits = 3)
            }
          if (type=="labels")
            for (one in names(specs)) {
              res[[one]]<-specs[[one]]$labels
            }
          if (type=="values_labels")
            for (one in names(specs)) {
              if (all(specs[[one]]$labels==round(specs[[one]]$values,digits = 3)))
                  res[[one]]<-specs[[one]]$labels
              else                  
                  res[[one]]<-paste(specs[[one]]$labels,round(specs[[one]]$values,digits = 3),sep = "=")
            }
          if (decode)
            names(res)<-jmvcore::toB64(names(res))
          
          if (length(res)==1)
            res<-res[[1]]
          res
        }
        ),
        private=list(
          cond_specs=list(),
          init=function(method,span) {
            ### expands spans to all variables
            spans<-c(span,rep(1,(length(self$vars)-length(span))))
            names(spans)<-self$vars
            if (is.list(method)) {
                for (name in names(method)) 
                    private$cond_specs[[name]]<-private$makeLabels(method[[name]],spans[[name]])
                empty<-which(!(self$vars %in% names(private$cond_specs)))
                if (length(empty)>0) {
                    for (e in empty)
                        private$cond_specs[[self$vars[e]]]<-private$makeLabels("mean_sd",spans[[self$vars[e]]])
                }
            }
            else
              for (name in self$vars) 
                private$cond_specs[[name]]<-private$makeLabels(method,spans[[name]])
            

          },
          makeLabels=function(obj,span) {
            if (length(obj)==1) {
              if (obj=="mean_sd") {
                res<-list(method="mean_sd",
                          span=span,
                          labels=(c(paste0("Mean-",span,"\U22C5","SD"),"Mean",paste0("Mean+",span,"\U22C5","SD"))),
                          values=NULL)
                return(res)
              }
              if (obj=="percent") {
                if (span==1)
                      span<-25
                res<-list(method="percent",
                          span=span,
                          labels=c(paste0(50-span,"%"),"50%",paste0(50+span,"%")),
                          values=NULL)
                return(res)                
              }
            }
            private$makeNames(obj)
          },
          makeNames=function(obj) {
            .names<-names(obj)
            if (is.null(.names)) {
              .labs=obj
            }
            else {
              empty<-.names==""
              .labs<-.names
              .labs[empty]<-obj[empty]
            }
            names(obj)<-NULL
            res<-list(method="custom",
                      labels=.labs,
                      values=obj)
            return(res)                
            
          }
          
        ) # end of private
)
