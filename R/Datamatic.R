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
    original_descriptive=NULL,
    descriptive=NULL,
    nlevels=NULL,
    neffects=NULL,
    contrast_values=NULL,
    contrast_labels=NULL,
    levels_labels=NULL,
    method=NULL,
    scaling="none",
    hasCluster=NULL,
    nClusters=0,
    isBetween=FALSE,
    initialize=function(var,options) {
      self$name<-var
      self$options<-options
      self$vars=var
      self$name64<-tob64(var)
      

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
        self$neffects<-self$nlevels-1
        self$paramsnames<-paste0(var,1:(self$neffects))
        self$paramsnames64<-paste0(tob64(var),FACTOR_SYMBOL,1:(self$neffects))

        self$descriptive=list(min=0,max=1)
        
        cont<-lapply(self$options$contrasts,function(a) a$type)
        names(cont)<-sapply(self$options$contrasts,function(a) a$var)
        ctype<-ifelse(var %in% names(cont),cont[[var]],"simple") 
        self$contrast_values<-private$.contrast_values(self$levels, ctype)
        self$contrast_labels<-private$.contrast_labels(self$levels, ctype)
        self$method=ctype
      }

      ### check dependent variables ###
      if (var %in% self$options$dep) {
           self$type=class(vardata)
           if (self$type=="factor") {
                 self$descriptive=list(min=0,max=1)
                 self$levels<-levels(vardata)
                 self$levels_labels<-levels(vardata)
                 self$nlevels<-length(self$levels)
                 self$neffects<-self$nlevels-1
                 self$contrast_values<-private$.contrast_values(self$levels, "dummy")
                 self$contrast_labels<-private$.contrast_labels(self$levels,  "dummy")
                 self$method="dummy"
                 
           } else {
             
             if (self$option("dep_scale")) 
                 self$scaling<-self$options$dep_scale
                 self$contrast_labels<-self$name

           }


      }
        
            
      if (var %in% self$options$covs) {
        self$type="numeric"
        scaling<-sapply(self$options$scaling,function(a) a$type)
        names(scaling)<-unlist(sapply(self$options$scaling,function(a) a$var))
        self$scaling<-ifelse(var %in% names(scaling),scaling[[var]],"centered")
        
        if (is.factor(vardata)) {
          self$warnings<-list(topic="data",message=paste("Variable",var,"has been coerced to numeric"))
        }
        self$contrast_labels<-self$name
        self$paramsnames<-var
        self$paramsnames64<-tob64(var)
        self$nlevels=3
        self$neffects=1
        
      }
      ### end covs ####
      
      if (self$option("cluster")) {
        if (self$name %in% self$options$cluster) {
            self$type="cluster"
            self$levels<-levels(vardata)
            self$levels_labels<-levels(vardata)
            self$nlevels<-length(self$levels)
            self$neffects<-self$nlevels-1
        } else {
           self$hasCluster<-self$options$cluster
           self$nClusters<-length(self$options$cluster)
        }
      }
        
      return(self)  
      
    },
    get_values=function(data) {

      
       vardata<-data[[self$name64]]

       if (self$type=="cluster") {
         if (!is.factor(vardata)) {
           vardata<-factor(vardata)
           self$warnings<-list(topic="data",message=paste("Variable",self$name,"has been coerced to factor"))
         }
         return(vardata)
         
       }
       
       
       
       if (self$type=="factor") {
          if (!is.factor(vardata)) {
              self$errors<-list(topic="data",message=paste("Variable",self$name,"is not a factor"))
              return()
          }
       
                
        ## check if is within or between in case of repeated measures
        ## because tables can be very long to build, we examine only the first and the last
        ## cluster. In the majority of the case the guess is good
        if (is.something(self$hasCluster) && length(vardata)>0) {
          cluster64<-tob64(self$hasCluster[[1]])
          levs<-unique(data[[cluster64]])
          testdata<-data[data[[cluster64]]==levs[1],self$name64]
          tt<-table(testdata)
          test1=(max(tt)!=sum(tt))
          testdata<-data[data[[cluster64]]==levs[length(levs)],self$name64]
          tt<-table(testdata)
          test2=(max(tt)!=sum(tt))
          if (!all(c(test1,test2)))
               self$isBetween<-TRUE
        }
         
         
        contrasts(vardata)<-self$contrast_values
        ### fix levels ####
        levels(vardata)<-paste0(LEVEL_SYMBOL,levels(vardata))
        
        return(vardata)

      }
    

        ### if we are here, it means this is a continuos variable
        ### we need to pass the data for when clustering is needed
         return(private$.continuous_values(data))
      },
      
      contrast_codes=function(type) {
        
        private$.contrast_values(self$levels,type)
        
      }
      
  
  ), # end of public
  private=list(
    .data=NULL,
    .set=FALSE,
    .contrast_values=function(levels, type) {
      
      nLevels <- length(levels)
      
      if (is.null(type))
        type<-"simple"
      
      switch(type,
            "simple"={
              dummy <- stats::contr.treatment(levels)
              dimnames(dummy) <- NULL
              coding <- matrix(rep(1/nLevels, prod(dim(dummy))), ncol=nLevels-1)
              contrast <- (dummy - coding)
            },
            "deviation"={
              contrast <- matrix(0, nrow=nLevels, ncol=nLevels-1)
              for (i in seq_len(nLevels-1)) {
                    contrast[i+1, i] <- 1
                    contrast[1, i] <- -1
              }
              },
            'difference'={
                  contrast <- stats::contr.helmert(levels)
                  for (i in 1:ncol(contrast))
                  contrast[,i] <- contrast[,i] / (i + 1)
              },
            'helmert'={
                  contrast <- matrix(0, nrow=nLevels, ncol=nLevels-1)
        
                  for (i in seq_len(nLevels-1)) {
                        p <- (1 / (nLevels - i + 1))
                        contrast[i,i] <- p * (nLevels - i)
                        contrast[(i+1):nLevels,i] <- -p
                    }
                  },
             'polynomial'={
                  contrast <- stats::contr.poly(levels)
              },
            'repeated'={
        
                  contrast <- matrix(0, nrow=nLevels, ncol=nLevels-1)
                  for (i in seq_len(nLevels-1)) {
                      contrast[1:i,i] <- (nLevels-i) / nLevels
                      contrast[(i+1):nLevels,i] <- -i / nLevels
                  }
              },
            'dummy'={
                    contrast <- stats::contr.treatment(levels,base=1)
      } 
      ) # end of switch
      
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
      ginfo("no contrast definition met")
      
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
      ginfo("no contrast definition met")
      
      all <- paste(levels, collapse=', ')
      for (i in seq_len(nLevels-1))
        labels[[i]] <- paste(levels[i+1], '- (', all,")")
      return(labels)
    },
    
    .continuous_values=function(data) {
      

      if (nrow(data)==0)
           return(jmvcore::toNumeric(data[[self$name64]]))

      vardata<-data[[self$name64]]

      if (is.factor(vardata)) 
           vardata<-jmvcore::toNumeric(vardata)
           
      ## we first update levels to same the old levels
      private$.update_levels(vardata)
      
      
      method<-self$scaling
      
    

      if (method=="centered") 
        vardata<-scale(vardata,scale = F)  

      if (method=="standardized") 
        vardata<-scale(vardata,scale = T)  

      if (method=="log") {
        vardata<-log(vardata)  
        if (any(is.nan(vardata)))
          self$errors<-list(topic="info",message=paste("Negative values found in variable",self$name,". Log transform not applicable."))
      }
      
      
      if (method=="clusterbasedcentered") {    
        cluster64<-tob64(self$hasCluster[1])
        sdata<-data[,c(cluster64,self$name64)]
        mdata<-aggregate(sdata[,self$name64],list(sdata[[cluster64]]),mean,na.rm=TRUE)
        names(mdata)<-c(cluster64,"mean")
        sdata<-merge(sdata,mdata,by=cluster64)
        sdata[[self$name64]]<-sdata[[self$name64]]-sdata[["mean"]]
        vardata<-sdata[[self$name64]]
        self$warnings<-list(topic="data",message=paste("Variable",self$name,"has been centered within clusters defined by",self$hasCluster[[1]]))
      }
      if (method=="clusterbasedstandardized") {    
        cluster64<-tob64(self$hasCluster[1])
        sdata<-data[,c(cluster64,self$name64)]
        mdata<-aggregate(sdata[,self$name64],list(sdata[[cluster64]]),mean,na.rm=TRUE)
        names(mdata)<-c(cluster64,"mean")
        sdata<-merge(sdata,mdata,by=cluster64)
        ddata<-aggregate(sdata[,self$name64],list(sdata[[cluster64]]),sd,na.rm=TRUE)
        names(ddata)<-c(cluster64,"sd")
        if (any(ddata[["sd"]]<.0000001))
            stop("Variable ",self$name," has zero variance in at least one cluster defined by",self$hasCluster[1])
          
        sdata<-merge(sdata,ddata,by=cluster64)
        sdata[[self$name64]]<-(sdata[[self$name64]]-sdata[["mean"]])/sdata[["sd"]]
        vardata<-sdata[[self$name64]]
        self$warnings<-list(topic="data",message=paste("Variable",self$name,"has been standardized within clusters defined by",self$hasCluster[[1]]))
        
      }

      if (method=="clustermeans") {    
        cluster64<-tob64(self$hasCluster[1])
        sdata<-data[,c(cluster64,self$name64)]
        mdata<-aggregate(sdata[,self$name64],list(sdata[[cluster64]]),mean,na.rm=TRUE)
        names(mdata)<-c(cluster64,"mean")
        sdata<-merge(sdata,mdata,by=cluster64)
        vardata<-sdata[["mean"]]
        self$warnings<-list(topic="data",message=paste("Variable",self$name,"represents means of clusters in",self$hasCluster[[1]]))
        
      }
      
      ## we then update levels to same the new levels (mean, sd etc)
      private$.update_levels(vardata)
      
      as.numeric(vardata)
      
    },
    
    .update_levels=function(vardata) {
      
      self$original_levels<-self$levels
      self$original_descriptive<-self$descriptive
      labels_type<-ifelse(is.null(self$options$simpleScaleLabels),"values",self$options$simpleScaleLabels)
      ### when called by init, force labels because we cannot compute the values
      ### if not, we can compute the descriptive
      if (length(vardata)==0)
           labels_type="labels"
      else
          self$descriptive<-list(min=min(vardata,na.rm = TRUE),
                              max=max(vardata,na.rm = TRUE),
                              mean=mean(vardata,na.rm = TRUE),
                              sd=sd(vardata,na.rm = TRUE))
      
      
      if (self$options$simpleScale=="mean_sd")  {
        
        .span<-ifelse(is.null(self$options$cvalue),1,self$options$cvalue)
        .labs<-c(paste0("Mean-", .span, "\u00B7", "SD"), "Mean", paste0("Mean+", .span, "\u00B7","SD"))
        .mean <- mean(vardata)
        .sd <- sd(vardata)
        self$levels=round(c(.mean - (.span * .sd), .mean, .mean + (.span * .sd)),digits = 3)
        self$method="mean_sd"

      }
      if (self$options$simpleScale=="percent") {
        
        .lspan<-ifelse(is.null(self$options$percvalue),25,self$options$percvalue)
        .span<-.lspan/100
        
        .labs<-c(paste0("50-", .lspan,"\u0025"), "50\u0025", paste0("50+", .lspan,"\u0025"))
        
        self$levels<-round(quantile(vardata, c(0.5 - .span, 0.5, 0.5 + .span),na.rm=TRUE), digits = 3) 
          
        self$method="percent"
      }
      
      if (labels_type == "labels") 
        self$levels_labels<-.labs
      
      if (labels_type == "values") 
        self$levels_labels<-self$levels
      
      if (labels_type == "uvalues") 
        self$levels_labels<-self$original_levels
      
      
      if (labels_type == "values_labels") 
        self$levels_labels<-paste(.labs,self$levels,sep="=")
      
      if (labels_type == "uvalues_labels") 
        self$levels_labels<-paste(.labs,self$original_levels,sep="=")
  
      if (all(!is.nan(self$levels)) &  all(!is.na(self$levels)))
            if(any(duplicated(self$levels))) {
               self$levels<-unique(self$levels)
               self$warnings<-list(topic="simpleEffects_anova",message=paste0("Problems in covariates conditioning for variable ",self$name,". Values are not differentiable, results may be misleading. Please enlarge the offset or change the conditioning method."))
               self$warnings<-list(topic="simpleEffects_coefficients",message=paste0("Problems in covariates conditioning for variable ",self$name,". Values are not differentiable, results may be misleading. Please enlarge the offset or change the conditioning method."))
               
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
    dep=NULL,
    labels=NULL,
    N=NULL,
    initialize=function(options,data) {
      vars<-unlist(c(options$dep,options$factors,options$covs))
      
      if (hasName(options,"cluster"))
               vars<-c(options$cluster,vars)
      
      super$initialize(options=options,vars=vars)
      private$.inspect_data(data)

    },

     cleandata=function(data) {
      
      data64          <-   data
      names(data64)   <-   tob64(names(data))

      for (var in self$variables) {
                      data64[[var$name64]]   <-  var$get_values(data64)
      }
        
      data64 <- jmvcore::naOmit(data64)
      attr(data64, 'row.names') <- seq_len(dim(data64)[1])
      self$N<-dim(data64)[1]
      self$absorbe_issues(self$variables)
      return(data64)

    },
    get_params_labels=function(terms) {
      
      ### here we want to gather the labels of the effects. If the variable is continuous, its name is passed on
      ### if the variable is categorical (a contrast is required), it is passed. 
      ### however, we need to change the formatting depending on the type of label
      
      labs<-lapply(terms, function(term) {
        for (i in seq_along(term)) {
          alabel<-self$labels[[ term[[i]] ]]
          if (is.something(alabel)) {
            ## if it is a contrast and its part of an interaction, we put paranthesis around
            if (length(term)>1 & length(grep(FACTOR_SYMBOL,term[[i]],fixed = T))>0) {
                 alabel<-paste0("(",alabel,")")
                 ### we want to avoid that an interaction (1-0)*(1-0) becomes (1-0)^2, so we trick 
                 ### jmvcore::stringifyTerm by adding a different string to each label
                 term[[i]]<-paste0(alabel,paste0(rep(IMPROBABLE_SEQ,i),collapse = ""))
            } else
                 term[[i]]<-alabel
          }
        }
        term<-gsub(IMPROBABLE_SEQ,"",jmvcore::stringifyTerm(term,raise = T),fixed = T)

        return(term)
      })
      return(unlist(fromb64(labs,self$vars)))
      
      
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
       self$dep<-self$variables[[tob64(self$options$dep)]]
       }
     
   ) #end of private
)