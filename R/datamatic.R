Variable <- R6::R6Class(
  "Variable",
  class=TRUE, ## this and the next 
  cloneable=FALSE, ## should improve performance https://r6.r-lib.org/articles/Performance.html ###
  inherit = Dispatch,
  public=list(
    name=NULL,
    paramsnames=NULL,
    name64=NULL,
    paramsnames64=NULL,
    type=NULL,
    levels=NULL,
    original_levels=NULL,
    nlevels=NULL,
    contrast_values=NULL,
    contrast_labels=NULL,
    levels_labels=NULL,
    method=NULL,
    scaling=NULL,
    hasCluster=NULL,
    initialize=function(var,options) {
      self$name<-var
      self$options<-options
      self$vars=var
      self$name64<-tob64(var)
      
      if (hasName(options,"cluster")) {
         self$hasCluster<-options$cluster[[1]]
      }

    },
    checkVariable=function(data) { 

      var<-self$name
      
      if (inherits(data,"data.frame"))
             vardata<-data[[var]]
      else 
        vardata<-data
           
      if (is.null(vardata)) {
          self$errors<-paste("Variable",var,"not in the data")
          return(self)
      }

      if (var %in% self$options$factors) {
        self$type="factor"
        self$levels<-levels(vardata)
        self$levels_labels<-levels(vardata)
        
        self$nlevels<-length(self$levels)
        self$paramsnames<-paste0(var,1:(self$nlevels-1))
        self$paramsnames64<-paste0(tob64(var),FACTOR_SYMBOL,1:(self$nlevels-1))

        cont<-lapply(self$options$contrasts,function(a) a$type)
        names(cont)<-sapply(self$options$contrasts,function(a) a$var)
        ctype<-ifelse(var %in% names(cont),cont[[var]],"simple") 
        self$contrast_values<-private$.contrast_values(self$levels, ctype)
        self$contrast_labels<-private$.contrast_labels(self$levels, ctype)
        self$method=ctype
      }

      if (var %in% self$options$dep) {
        self$type<-"numeric"
        self$scaling<-self$options$dep_scale
      }
        
            
      if (var %in% self$options$covs) {
        self$type="numeric"
        scaling<-sapply(self$options$scaling,function(a) a$type)
        names(scaling)<-unlist(sapply(self$options$scaling,function(a) a$var))

        if (var %in% names(scaling))
             self$scaling<-scaling[[var]]
        else
             self$scaling<-"centered"
        
        if (is.factor(vardata)) {
          self$warnings<-list(topic="data",message=paste("Variable",var,"has been coerced numeric"))
          vardata<-jmvcore::toNumeric(vardata)
        }
        self$paramsnames<-var
        self$paramsnames64<-tob64(var)
        self$nlevels=3
      }
      return(self)  
      
    },
    get_values=function(data) {
      
      vardata<-data[[self$name]]
      
      if (self$type=="factor") {
        if (!is.factor(vardata)) {
          vardata<-factor(vardata)
          self$warnings<-list(topic="data",message=paste("Variable",var,"has been coerced to factor"))
        }
        contrasts(vardata)<-self$contrast_values
        return(vardata)

      }
      else {
           return(private$.continuous_values(vardata))
      }
      
      
    }
  ), # end of public
  private=list(
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
      dimnames(contrast)<-list(NULL,paste0(FACTOR_SYMBOL,1:(nLevels-1)))
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
    },
    
    .continuous_values=function(vardata) {
      

      if (is.null(vardata))
           return(NULL)
      
      private$.update_levels(vardata)
      
      
      method<-self$scaling
      
      by<-self$hasCluster
      
      if (method=="centered") 
        vardata<-scale(vardata,scale = F)  
      if (method=="cluster-based centered") {    
        vardata<-unlist(tapply(vardata,by,scale,scale=F))
      }
      if (method=="standardized") 
        vardata<-scale(vardata,scale = T)  
      if (method=="cluster-based standardized")     
        vardata<-unlist(tapply(vardata,by,scale,scale=T))
      
      private$.update_levels(vardata)
      
      as.numeric(vardata)
      
    },
    
    .update_levels=function(vardata) {
      
      
      self$original_levels<-self$levels
      
      labels_type<-ifelse(is.null(self$options$simpleScaleLabels),"values",self$options$simpleScaleLabels)
      ### when called by init, force labels because we cannot compute the values
      if (length(vardata)==0)
           labels_type="labels"
      
      if (self$options$simpleScale=="mean_sd")  {
        .span<-ifelse(is.null(self$options$cvalue),1,self$options$cvalue)
        .labs<-c(paste0("Mean-", .span, "\u00B7", "SD"), "Mean", paste0("Mean+", .span, "\u00B7","SD"))
        .mean <- mean(vardata)
        .sd <- sd(vardata)
        self$levels=round(c(.mean - (.span * .sd), .mean, .mean + (.span * .sd)),digits = 3)
        self$method="mean_sd"
        if (labels_type == "values") 
          self$levels_labels<-self$levels
        if (labels_type == "labels") 
          self$levels_labels<-.labs
        if (labels_type == "values_labels") { 
          self$levels_labels<-paste(.labs,self$levels,sep="=")
        }
      }
      if (self$options$simpleScale=="percent") {
        
        .lspan<-ifelse(is.null(self$options$percvalue),25,self$options$percvalue)
        .span<-.lspan/100
        
        .labs<-c(paste0("50-", .lspan,"\u0025"), "50\u0025", paste0("50+", .lspan,"\u0025"))
        
        self$levels<-round(quantile(vardata, c(0.5 - .span, 0.5, 0.5 + .span)), digits = 3) 
        self$method="percent"
        if (labels_type == "values") 
          self$levels_labels<-self$levels
        
        if (labels_type == "labels") 
          self$levels_labels<-.labs
        
        if (labels_type == "values_labels") { 
          self$levels_labels<-paste(.labs,self$levels,sep="=")
        }
      }
      
      
    }
    

  ) #end of private
) # end of class




Datamatic <- R6::R6Class(
  "Datamatic",
  cloneable=FALSE,
  class=TRUE,
  inherit = Dispatch,
  public=list(
    variables=NULL,
    data_structure64=NULL,
    labels=NULL,
    initialize=function(options,data) {
      super$initialize(options=options,vars=unlist(c(options$dep,options$factors,options$covs)))
      private$.inspect_data(data)

    },
    cleandata=function(data) {
      
      data64<-data
      names(data64)<-tob64(names(data))
      
      for (var in self$variables) {
            data64[[var$name64]]<-var$get_values(data)
      }
      attr(data64, 'row.names') <- seq_len(dim(data64)[1])
      data64 <- jmvcore::naOmit(data64)
      return(data64)

    },
    get_params_labels=function(terms) {
      
      labs<-lapply(terms, function(term) {
        for (i in seq_along(term)) {
          alabel<-self$labels[[ term[[i]] ]]
          if (is.something(alabel)) {
            if (length(term)>1) 
              alabel<-paste0("(",alabel,")")
            term[[i]]<-alabel
          }
        }
        return(term)
      })
      return(fromb64(labs,self$vars))
      
      
    }
    
    ), ### end of public
   private=list(
     .inspect_data=function(data) {
       
       self$variables<-lapply(self$vars,function(var) Variable$new(var,self$options)$checkVariable(data))
       names(self$variables)<-unlist(lapply(self$variables,function(var) var$name64))
       labels<-list()
       for (var in self$variables) 
           for (i in seq_along(var$paramsnames64)) {
             par64<-var$paramsnames64[[i]]
             lab<-var$contrast_labels[[i]]
             labels[[par64]]<-lab
           }
       self$labels<-labels
       self$data_structure64<-self$cleandata(data)
       }


     
     
   ) #end of private
)