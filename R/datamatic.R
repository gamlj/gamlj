Variable <- R6::R6Class(
  "Variable",
  class=TRUE, ## this and the next 
  cloneable=FALSE, ## should improve performance https://r6.r-lib.org/articles/Performance.html ###
  inherit = Dispatch,
  public=list(
    name=NULL,
    paramsnames=NULL,
    type=NULL,
    levels=NULL,
    nlevels=NULL,
    contrast_values=NULL,
    contrast_labels=NULL,
    method=NULL,
    initialize=function(var,data,options) {
      self$name<-var
      private$.data<-data
      self$options<-options
      self$checkVariable()
    },
    checkVariable=function() { 
      if (private$.set==TRUE)
        return()
      var<-self$name
      if (var %in% self$options$factors) {
        self$type="factor"
        vardata<-private$.data[[var]]
        if (!is.factor(vardata)) {
          self$warnings<-list(topic="data",message=paste("Variable",var,"has been coerced to factor"))
          vardata<-factor(vardata)
        }
        self$levels<-levels(vardata)
        self$nlevels<-length(self$levels)
        self$paramsnames<-paste0(var,1:(self$nlevels-1))
        private$.paramsnames<-paste0(tob64(var),FACTOR_SYMBOL,1:self$nlevels)
        private$.name<-tob64(var)
        
        cont<-lapply(options$contrasts,function(a) a$type)
        names(cont)<-sapply(options$contrasts,function(a) a$var)
        ctype<-ifelse(var %in% names(cont),cont[[var]],"simple") 
        self$contrast_values<-private$.contrast_values(self$levels, ctype)
        self$contrast_labels<-private$.contrast_labels(self$levels, ctype)
        self$method=ctype
        private$.set=TRUE
      }
      
      if (var %in% self$options$covs) {
        self$type="numeric"
        vardata<-private$.data[[var]]
        if (is.factor(vardata)) {
          self$warnings<-list(topic="data",message=paste("Variable",var,"has been coerced numeric"))
          vardata<-jmvcore::toNumeric(vardata)
        }
        self$paramsnames<-var
        private$.name<-tob64(var)
        private$.paramsnames<-tob64(var)
        
        if (length(vardata)==0)
          return()
        
        if (self$options$simpleScale=="mean_sd")  {
              .span<-1
              .mean <- mean(vardata)
              .sd <- sd(vardata)
               self$levels=c(.mean - (.span * .sd), .mean, .mean + (.span * .sd))
               self$method="mean_sd"
        }
        if (self$options$simpleScale=="percent") {
              .span=.25
              self$levels<-(round(quantile(vardata, c(0.5 - .span, 0.5, 0.5 + .span)), digits = 3)) 
              self$method="percent"
        }
        
                  
        
        
        self$nlevels=3
        private$.set=TRUE
        
        
      }
    }
  ), # end of public
  private=list(
    .name=NULL,
    .paramsnames=NULL,
    .data=NULL,
    .set=FALSE,
    .contrast_values=function(levels, type) {
      
      nLevels <- length(levels)
      
      if (is.null(type))
        type<-"simple"
      
      
      if (type == 'simple') {
        dummy <- stats::contr.treatment(levels)
        dimnames(dummy) <- NULL
        coding <- matrix(rep(1/nLevels, prod(dim(dummy))), ncol=nLevels-1)
        contrast <- (dummy - coding)
        
      } else if (type == 'deviation') {
        contrast <- matrix(0, nrow=nLevels, ncol=nLevels-1)
        for (i in seq_len(nLevels-1)) {
          contrast[i+1, i] <- 1
          contrast[1, i] <- -1
        }
        
      } else if (type == 'difference') {
        
        contrast <- stats::contr.helmert(levels)
        for (i in 1:ncol(contrast))
          contrast[,i] <- contrast[,i] / (i + 1)
        
        dimnames(contrast) <- NULL
        
      } else if (type == 'helmert') {
        
        contrast <- matrix(0, nrow=nLevels, ncol=nLevels-1)
        
        for (i in seq_len(nLevels-1)) {
          p <- (1 / (nLevels - i + 1))
          contrast[i,i] <- p * (nLevels - i)
          contrast[(i+1):nLevels,i] <- -p
        }
        
      } else if (type == 'polynomial') {
        
        contrast <- stats::contr.poly(levels)
        dimnames(contrast) <- NULL
        
      } else if (type == 'repeated') {
        
        contrast <- matrix(0, nrow=nLevels, ncol=nLevels-1)
        for (i in seq_len(nLevels-1)) {
          contrast[1:i,i] <- (nLevels-i) / nLevels
          contrast[(i+1):nLevels,i] <- -i / nLevels
        }
        
      } else if (type == 'dummy') {
        contrast <- stats::contr.treatment(levels,base=1)
        dimnames(contrast) <- NULL
      } else {
        contrast <- matrix(0, nrow=nLevels, ncol=nLevels-1)
        for (i in seq_len(nLevels-1)) {
          contrast[i+1, i] <- 1
          contrast[1, i] <- -1
        }
      }
      dimnames(contrast)<-list(NULL,1:(nLevels-1))
      contrast
    },
    .contrast_labels=function(levels, type) {
      
      nLevels <- length(levels)
      labels <- list()
      
      if (is.null(type))
        type<-"simple"
      
      if (type == 'simple') {
        for (i in seq_len(nLevels-1))
          labels[[i]] <- paste(levels[i+1], '-', levels[1])
        return(labels)
      } 
      
      if (type == 'dummy') {
        for (i in seq_len(nLevels-1))
          labels[[i]] <- paste(levels[i+1], '-', levels[1])
        return(labels)
      } 
      
      if (type == 'deviation') {
        all <- paste(levels, collapse=', ')
        for (i in seq_len(nLevels-1))
          labels[[i]] <- paste(levels[i+1], '- (', all,")")
        return(labels)
        
      } 
      
      if (type == 'difference') {
        
        for (i in seq_len(nLevels-1)) {
          rhs <- paste0(levels[1:i], collapse=', ')
          if (nchar(rhs)>1) rhs<-paste0(" (",rhs,")")
          labels[[i]] <- paste(levels[i + 1], '-', rhs)
        }
        return(labels)
      }
      
      if (type == 'helmert') {
        
        for (i in seq_len(nLevels-1)) {
          rhs <- paste(levels[(i+1):nLevels], collapse=', ')
          if (nchar(rhs)>1) rhs<-paste0(" (",rhs,")")
          labels[[i]] <- paste(levels[i], '-', rhs)
        }
        return(labels)
      }
      
      
      
      if (type == 'repeated') {
        
        for (i in seq_len(nLevels-1))
          labels[[i]] <- paste(levels[i], '-', levels[i+1])
        return(labels)
        
      } 
      if (type == 'polynomial') {
        names <- c('linear', 'quadratic', 'cubic', 'quartic', 'quintic', 'sextic', 'septic', 'octic')
        for (i in seq_len(nLevels-1)) {
          if (i <= length(names)) {
            labels[[i]] <- names[i]
          } else {
            labels[[i]] <- paste('degree', i, 'polynomial')
          }
        }
        return(labels)
      }
      mark("no contrast definition met")
      
      all <- paste(levels, collapse=', ')
      for (i in seq_len(nLevels-1))
        labels[[i]] <- paste(levels[i+1], '- (', all,")")
      return(labels)
    },
    .contrast_label=function(levels, type) {
      
      nLevels <- length(levels)
      labels <- list()
      
      if (is.null(type))
        type<-"simple"
      
      if (type == 'simple') {
        for (i in seq_len(nLevels-1))
          labels[[i]] <- paste(levels[i+1], '-', levels[1])
        return(labels)
      } 
      
      if (type == 'dummy') {
        for (i in seq_len(nLevels-1))
          labels[[i]] <- paste(levels[i+1], '-', levels[1])
        return(labels)
      } 
      
      if (type == 'deviation') {
        all <- paste(levels, collapse=', ')
        for (i in seq_len(nLevels-1))
          labels[[i]] <- paste(levels[i+1], '- (', all,")")
        return(labels)
        
      } 
      
      if (type == 'difference') {
        
        for (i in seq_len(nLevels-1)) {
          rhs <- paste0(levels[1:i], collapse=', ')
          if (nchar(rhs)>1) rhs<-paste0(" (",rhs,")")
          labels[[i]] <- paste(levels[i + 1], '-', rhs)
        }
        return(labels)
      }
      
      if (type == 'helmert') {
        
        for (i in seq_len(nLevels-1)) {
          rhs <- paste(levels[(i+1):nLevels], collapse=', ')
          if (nchar(rhs)>1) rhs<-paste0(" (",rhs,")")
          labels[[i]] <- paste(levels[i], '-', rhs)
        }
        return(labels)
      }
      
      
      
      if (type == 'repeated') {
        
        for (i in seq_len(nLevels-1))
          labels[[i]] <- paste(levels[i], '-', levels[i+1])
        return(labels)
        
      } 
      if (type == 'polynomial') {
        names <- c('linear', 'quadratic', 'cubic', 'quartic', 'quintic', 'sextic', 'septic', 'octic')
        for (i in seq_len(nLevels-1)) {
          if (i <= length(names)) {
            labels[[i]] <- names[i]
          } else {
            labels[[i]] <- paste('degree', i, 'polynomial')
          }
        }
        return(labels)
      }
      mark("no contrast definition met")
      
      all <- paste(levels, collapse=', ')
      for (i in seq_len(nLevels-1))
        labels[[i]] <- paste(levels[i+1], '- (', all,")")
      return(labels)
    }
    
    
    
  ) #end of private
) # end of class




Datamatic <- R6::R6Class(
  "Datamatic",
  cloneable=FALSE,
  class=TRUE,
  inherit = Dispatch,
  public=list(
    factors=NULL,
    factors_scale=NULL,
    factors_levels=list(),
    contrasts_labels=NULL,
    contrasts_values=NULL,
    contrasts_names=NULL,
    continuous=NULL,
    continuous_scale=NULL,
    initialize=function(options,data) {
      super$initialize(options=options,vars=unlist(c(options$dep,options$factors,options$covs)))
      self$factors<-options$factors
      self$factors_scale<-sapply(options$contrasts, function(a) a$type)
      names(self$factors_scale)<-sapply(options$contrasts, function(a) a$var)
      self$continuous<-unlist(c(options$dep,options$covs))
      self$continuous_scale<-sapply(options$scaling, function(a) a$type)
      names(self$continuous_scale)<-sapply(options$scaling, function(a) a$var)
      private$.inspect_data(data)

    },
    cleandata=function(data,interactions=NULL) {

      for (factor in factors) {
        ### we need this for Rinterface ####
        if (!("factor" %in% class(dataRaw[[factor]]))) {
          self$warnings<-list(topic="data",message=paste("Warning, variable",factor," has been coerced to factor"))
          dataRaw[[factor]]<-factor(dataRaw[[factor]])
        }
        data[[tob64(factor)]] <- dataRaw[[factor]]
        levels <- base::levels(data[[tob64(factor)]])
        stats::contrasts(data[[tob64(factor)]]) <- lf.createContrasts(levels,"simple")
        attr(data[[jmvcore::toB64(factor)]],"jcontrast")<-"simple"
      }
      
      for (contrast in self$options$contrasts) {
        levels <- base::levels(data[[jmvcore::toB64(contrast$var)]])
        stats::contrasts(data[[jmvcore::toB64(contrast$var)]]) <- lf.createContrasts(levels, contrast$type)
        n64$addLabel(contrast$var,lf.contrastLabels(levels, contrast$type)) 
        attr(data[[jmvcore::toB64(contrast$var)]],"jcontrast")<-contrast$type
      }
      
      if ( ! is.null(dep)) {
        data[[jmvcore::toB64(dep)]] <- jmvcore::toNumeric(dataRaw[[dep]])
        n64$addVar(dep)
      }
      
      for (covariate in covs) {
        data[[jmvcore::toB64(covariate)]] <- jmvcore::toNumeric(dataRaw[[covariate]])
        n64$addVar(covariate)
      }
      attr(data, 'row.names') <- seq_len(length(data[[1]]))
      attr(data, 'class') <- 'data.frame'      
      data <- jmvcore::naOmit(data)
      return(data)
      
      
      
        
    }
    ), ### end of public
   private=list(
     .inspect_data=function(data) {
            names(data)<-jmvcore::toB64(names(data))
            for (factor in self$factors) {
                    self$factors_levels[[factor]]<-levels(data[,tob64(factor)])
                    for (i in seq_along(levels(data[,tob64(factor)])))
                          self$contrasts_names[[paste0(factor,i)]]<-paste0(tob64(factor),FACTOR_SYMBOL,i)
            }
     
            private$.create_constrasts()
            if (is.something(self$multigroup)) {
              var64<-tob64(self$multigroup)
              levels<-levels(data[,var64])
              self$multigroup<-list(var=self$multigroup,var64=var64,levels=levels,nlevels=length(levels))
            }
            
       },
     .create_constrasts=function() {
       
       self$contrasts_labels<-sapply(self$factors,function(factor) 
                         private$.contrast_label(self$factors_levels[[factor]],self$factors_scale[[factor]]),simplify = FALSE)  
       
       self$contrasts_values<-sapply(self$factors,function(factor) 
                         private$.contrast_value(self$factors_levels[[factor]],self$factors_scale[[factor]]),simplify = FALSE)  
     },
     .contrast_value=function(levels, type) {
       
       nLevels <- length(levels)
       
       if (is.null(type))
         type<-"simple"
       

       if (type == 'simple') {
         dummy <- stats::contr.treatment(levels)
         dimnames(dummy) <- NULL
         coding <- matrix(rep(1/nLevels, prod(dim(dummy))), ncol=nLevels-1)
         contrast <- (dummy - coding)
         
       } else if (type == 'deviation') {
         contrast <- matrix(0, nrow=nLevels, ncol=nLevels-1)
         for (i in seq_len(nLevels-1)) {
           contrast[i+1, i] <- 1
           contrast[1, i] <- -1
         }
         
       } else if (type == 'difference') {
         
         contrast <- stats::contr.helmert(levels)
         for (i in 1:ncol(contrast))
           contrast[,i] <- contrast[,i] / (i + 1)
         
         dimnames(contrast) <- NULL
         
       } else if (type == 'helmert') {
         
         contrast <- matrix(0, nrow=nLevels, ncol=nLevels-1)
         
         for (i in seq_len(nLevels-1)) {
           p <- (1 / (nLevels - i + 1))
           contrast[i,i] <- p * (nLevels - i)
           contrast[(i+1):nLevels,i] <- -p
         }
         
       } else if (type == 'polynomial') {
         
         contrast <- stats::contr.poly(levels)
         dimnames(contrast) <- NULL
         
       } else if (type == 'repeated') {
         
         contrast <- matrix(0, nrow=nLevels, ncol=nLevels-1)
         for (i in seq_len(nLevels-1)) {
           contrast[1:i,i] <- (nLevels-i) / nLevels
           contrast[(i+1):nLevels,i] <- -i / nLevels
         }
         
       } else if (type == 'dummy') {
         contrast <- stats::contr.treatment(levels,base=1)
         dimnames(contrast) <- NULL
       } else {
         contrast <- matrix(0, nrow=nLevels, ncol=nLevels-1)
         for (i in seq_len(nLevels-1)) {
           contrast[i+1, i] <- 1
           contrast[1, i] <- -1
         }
       }
       dimnames(contrast)<-list(NULL,1:(nLevels-1))
       contrast
     },
     .contrast_label=function(levels, type) {

         nLevels <- length(levels)
         labels <- list()
         
         if (is.null(type))
             type<-"simple"
         
         if (type == 'simple') {
           for (i in seq_len(nLevels-1))
             labels[[i]] <- paste(levels[i+1], '-', levels[1])
           return(labels)
         } 
         
         if (type == 'dummy') {
           for (i in seq_len(nLevels-1))
             labels[[i]] <- paste(levels[i+1], '-', levels[1])
           return(labels)
         } 
         
         if (type == 'deviation') {
           all <- paste(levels, collapse=', ')
           for (i in seq_len(nLevels-1))
             labels[[i]] <- paste(levels[i+1], '- (', all,")")
           return(labels)
           
         } 
         
         if (type == 'difference') {
           
           for (i in seq_len(nLevels-1)) {
             rhs <- paste0(levels[1:i], collapse=', ')
             if (nchar(rhs)>1) rhs<-paste0(" (",rhs,")")
             labels[[i]] <- paste(levels[i + 1], '-', rhs)
           }
           return(labels)
         }
         
         if (type == 'helmert') {
           
           for (i in seq_len(nLevels-1)) {
             rhs <- paste(levels[(i+1):nLevels], collapse=', ')
             if (nchar(rhs)>1) rhs<-paste0(" (",rhs,")")
             labels[[i]] <- paste(levels[i], '-', rhs)
           }
           return(labels)
         }
         
         
         
         if (type == 'repeated') {
           
           for (i in seq_len(nLevels-1))
             labels[[i]] <- paste(levels[i], '-', levels[i+1])
           return(labels)
           
         } 
         if (type == 'polynomial') {
           names <- c('linear', 'quadratic', 'cubic', 'quartic', 'quintic', 'sextic', 'septic', 'octic')
           for (i in seq_len(nLevels-1)) {
             if (i <= length(names)) {
               labels[[i]] <- names[i]
             } else {
               labels[[i]] <- paste('degree', i, 'polynomial')
             }
           }
           return(labels)
         }
         mark("no contrast definition met")
         
         all <- paste(levels, collapse=', ')
         for (i in seq_len(nLevels-1))
           labels[[i]] <- paste(levels[i+1], '- (', all,")")
         return(labels)
       },

 .continuous_value=function(var,method,by=NULL) {

        if (is.null(method))
           return(as.numeric(var))
   

       if (method=="centered") 
         var<-scale(var,scale = F)  
       if (method=="cluster-based centered") {    
         var<-unlist(tapply(var,by,scale,scale=F))
       }
       if (method=="standardized") 
         var<-scale(var,scale = T)  
       if (method=="cluster-based standardized")     
         var<-unlist(tapply(var,by,scale,scale=T))

       as.numeric(var)
       
     }
     
     
     
   ) #end of private
)