Datamatic <- R6::R6Class(
  "Datamatic",
  cloneable=FALSE,
  class=TRUE,
  inherit = Scaffold,
  public=list(
    vars=NULL,
    variables=NULL,
    data_structure64=NULL,
    dep=NULL,
    labels=NULL,
    N=NULL,
    wN=NULL,
    has_weights=FALSE,
    initialize=function(jmvobj) {

      super$initialize(jmvobj)
      
      self$vars<-unlist(c(self$options$dep,self$options$factors,self$options$covs))
      if (is.joption(self$options,"cluster")) {
         if (is.joption(self$options,"re_nestedclusters") && self$options$re_nestedclusters)
             lapply(self$options$cluster, function(x) 
                   if (length(grep("\\/",x))>0) stop("Cluster variables names cannot contain `/` when nesting by formula is used."))
         self$vars<-c(self$options$cluster,self$vars)
      }

      if (is.joption(self$options,"offset"))
        self$vars<-c(self$options$offset,self$vars)
      private$.inspect_data(self$analysis$data)
      
    },
    
    cleandata=function(data) {

      data64          <-   data
      names(data64)   <-   tob64(names(data))
      
      if (self$option("scale_missing","complete"))
                 data64 <- jmvcore::naOmit(data64)
      for (var in self$variables) {
        data64[[var$name64]]   <-  var$get_values(data64)
      }
      
      data64 <- jmvcore::naOmit(data64)
#      attr(data64, 'row.names') <- seq_len(dim(data64)[1])
      self$N<-dim(data64)[1]
     
      if (is.something(attr(data, "jmv-weights-name"))) {
                   self$has_weights<-TRUE
                   wname<-tob64(attr(data, "jmv-weights-name"))
                   attr(data64, "jmv.weights") <- jmvcore::naOmit(attr(data,"jmv-weights"))
      }
    
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
      return(unlist(fromb64(labs)))
      
      
    }
    
    

  ), ### end of public
  private=list(
    .inspect_data=function(data) {
      
      self$variables<-lapply(self$vars,function(var) Variable$new(var,self)$checkVariable(data))
      names(self$variables)<-unlist(lapply(self$variables,function(var) var$name64))

      labels<-list()
      for (var in self$variables)  {
        for (i in seq_along(var$paramsnames64)) {
          par64<-var$paramsnames64[[i]]
          lab<-var$contrast_labels[[i]]
          labels[[par64]]<-lab
        }
      }
      self$labels<-labels
      self$data_structure64<-self$cleandata(data)

      self$dep<-self$variables[[tob64(self$options$dep)]]
    }

  ) #end of private
)

Variable <- R6::R6Class(
  "Variable",
  class=TRUE, ## this and the next 
  cloneable=FALSE, ## should improve performance https://r6.r-lib.org/articles/Performance.html ###
  public=list(
    name=NULL,
    datamatic=NULL,
    paramsnames=NULL,
    name64=NULL,
    paramsnames64=NULL,
    type=NULL,
    levels               = NULL,
    original_levels      = NULL,
    original_descriptive = NULL,
    descriptive          = NULL,
    nlevels              = NULL,
    neffects=NULL,
    contrast_values=NULL,
    contrast_labels=NULL,
    levels_labels=NULL,
    method="numeric",
    covs_scale="none",
    hasCluster=NULL,
    nClusters=0,
    isBetween            = FALSE,
    isDependent          = FALSE,
    isFocal              = FALSE,

    initialize=function(var,datamatic) {
      self$name<-var
      self$datamatic<-datamatic
      self$name64<-tob64(var)
    },
    checkVariable=function(data) { 
      var<-self$name
      if (inherits(data,"data.frame"))
             vardata<-data[[var]]
      else 
        vardata<-data
      
      if (is.null(vardata)) {
          self$datamatic$error<-list(topic="info",message=paste("Variable",var,"not in the data"))
          return(self)
      }

      if (var %in% self$datamatic$options$factors) {
        self$type="factor"
        self$levels<-levels(vardata)
        self$levels_labels<-levels(vardata)
        self$nlevels<-length(self$levels)
        if (self$nlevels==0)
            stop("Variable ", var," has no valid level.")
        self$neffects<-self$nlevels-1
        self$paramsnames<-paste0(var,1:(self$neffects))
        self$paramsnames64<-paste0(tob64(var),FACTOR_SYMBOL,1:(self$neffects))

        self$descriptive=list(min=0,max=1)
        
        cont<-lapply(self$datamatic$options$contrasts,function(a) a$type)
        names(cont)<-sapply(self$datamatic$options$contrasts,function(a) a$var)
        ctype<-ifelse(var %in% names(cont),cont[[var]],"simple") 
        self$contrast_values<-private$.contrast_values(self$levels, ctype)
        self$contrast_labels<-private$.contrast_labels(self$levels, ctype)
        self$method=ctype
      }

      ### check dependent variables ###
      if (var %in% self$datamatic$options$dep) {
        
           self$isDependent<-TRUE
           self$type=class(vardata)
           if ("character" %in% self$type) stop("Character type not allowed. Please set variable ",self$name," as numeric or factor")


           if ("ordered" %in% self$type) {
                 self$type="factor"
           }
           

           if (self$type=="factor") {
                 self$descriptive=list(min=0,max=1)
                 self$levels<-levels(vardata)
                 self$levels_labels<-levels(vardata)
                 self$nlevels<-length(self$levels)
                 self$neffects<-self$nlevels-1
                 self$contrast_values<-private$.contrast_values(self$levels, "dummy")
                 self$contrast_labels<-private$.contrast_labels(self$levels,  "dummy")
                 self$method="dummy"
                 
           }
           if (self$type %in% c("numeric","integer")) {
               if (self$datamatic$option("dep_scale")) 
                 self$covs_scale<-self$datamatic$options$dep_scale
                 self$contrast_labels<-self$name
             }
            }

            
      if (var %in% self$datamatic$options$covs) {
        self$type="numeric"
        covs_scale<-sapply(self$datamatic$options$covs_scale,function(a) a$type)
        names(covs_scale)<-unlist(sapply(self$datamatic$options$covs_scale,function(a) a$var))
        self$covs_scale<-ifelse(var %in% names(covs_scale),covs_scale[[var]],"centered")
        
        if (is.factor(vardata)) {
          self$datamatic$warning<-list(topic="info",message=paste("Variable",var,"has been coerced to numeric"))
        }
        self$contrast_labels <- self$name
        self$paramsnames     <- var
        self$paramsnames64   <- tob64(var)
        self$nlevels         <- 3
        self$neffects        <- 1
        if (self$datamatic$options$covs_conditioning == "range")
                self$nlevels <- as.numeric(self$datamatic$options$ccra_steps)+1
        
      }
      ### end covs ####
      
      if (self$datamatic$option("cluster")) {
        if (self$name %in% self$datamatic$options$cluster) {
            if (!is.factor(vardata)) stop("Cluster variable ",self$name," should be a nominal variable")
            self$type="cluster"
            self$levels<-levels(vardata)
            self$levels_labels<-levels(vardata)
            self$nlevels<-length(self$levels)
            self$neffects<-self$nlevels-1
        } else {
           self$hasCluster<-self$datamatic$options$cluster
           self$nClusters<-length(self$datamatic$options$cluster)
        }
      }
      
      if (self$datamatic$option("offset")) {
        if (var %in% self$datamatic$options$offset) {
            if (is.factor(vardata))
               stop("Offset variable should be numeric")
            self$type="numeric"
        }
        }
      
      ### end offset ####
      
      ## this is for special treatments of variables
      

      return(self)  
      
    },
    get_values=function(data) {


       vardata<-data[[self$name64]]
       if (nrow(data)>0 && all(is.na(vardata)))
           stop("Variable ",self$name," has no valid case.")
       
       if (self$type=="numeric" || self$type=="integer") {
          if (is.factor(vardata)) {
           vardata<-as.numeric(vardata)
         }
         return(private$.continuous_values(data))
       }
         

       if (self$type=="cluster") {
         if (!is.factor(vardata)) {
           vardata<-factor(vardata)
           self$datamatic$warning<-list(topic="info",message=paste("Variable",self$name,"has been coerced to nominal"))
         }
         return(vardata)
       }
       
       
       if (self$type=="factor") {
          if (!is.factor(vardata)) {
            self$datamatic$error<-list(topic="info",message=paste("Variable",self$name,"is not a factor"))
            return()
          }
       }

        contrasts(vardata)<-self$contrast_values
        ### fix levels ####
        levels(vardata)<-paste0(LEVEL_SYMBOL,tob64(levels(vardata)))
        return(vardata)
    
      },
      requireFocus = function() {
        
        if (self$method=="custom" && self$datamatic$options$contrast_custom_focus && self$isFocal)
           return(TRUE)
        else
           return(FALSE)
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
             },
            'custom'={
                     custom<-NULL
                     customs=self$datamatic$options$contrast_custom_values
                     for (cus in customs) if (cus$var==self$name) custom<-cus$codes
                     if (is.null(custom)) stop("Contrast weights are required for variable",self$name," defined as custom")
                     custom<-as.numeric(strsplit(custom,split="[,;]")[[1]])
                     custom<-custom[!is.na(custom)]
                     if (length(custom)!=nLevels)
                         stop("Custom codes for variable ",self$name," are not correct: ",nLevels," codes are required.")
                     if (sum(custom)!=0)
                           self$datamatic$warning<-list(topic="info",message=paste("Custom codes for variable ",self$name," do not sum up to zero. Results may be uninterpretable"))
                      x<-factor(1:nLevels)
                      contrasts(x)<-custom
                      contrast <- contrasts(x)
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
      if (type == 'custom') {

        custom<-self$contrast_values[,1]
        lab <- paste("{",paste(custom,levels,sep="*",collapse=", "),"}")
        labels[[1]]<-lab
        if (nLevels>2)
           for (i in 2:(nLevels-1)) labels[[i]]<-paste0("tech",i)
        self$datamatic$warning<-list(topic="info",message=paste("The user defined contrast for variable",self$name," is named ",paste0(self$name,"1"),"and indicated as <b>",lab, "</b> in the effect labels."))
        return(labels)
        
      }
      
      jinfo("no contrast definition met")
      
      for (i in seq_len(nLevels-1)) labels[[i]]<-paste0("User",i)
      return(labels)
    },
    # .contrast_label=function(levels, type) {
    #   
    #   nLevels <- length(levels)
    #   labels <- list()
    #   
    #   if (is.null(type))
    #     type<-"simple"
    #   
    #   if (type == 'simple') {
    #     for (i in seq_len(nLevels-1))
    #       labels[[i]] <- paste(levels[i+1], '-', levels[1])
    #     return(labels)
    #   } 
    #   
    #   if (type == 'dummy') {
    #     for (i in seq_len(nLevels-1))
    #       labels[[i]] <- paste(levels[i+1], '-', levels[1])
    #     return(labels)
    #   } 
    #   
    #   if (type == 'deviation') {
    #     all <- paste(levels, collapse=', ')
    #     for (i in seq_len(nLevels-1))
    #       labels[[i]] <- paste(levels[i+1], '- (', all,")")
    #     return(labels)
    #     
    #   } 
    #   
    #   if (type == 'difference') {
    #     
    #     for (i in seq_len(nLevels-1)) {
    #       rhs <- paste0(levels[1:i], collapse=', ')
    #       if (nchar(rhs)>1) rhs<-paste0(" (",rhs,")")
    #       labels[[i]] <- paste(levels[i + 1], '-', rhs)
    #     }
    #     return(labels)
    #   }
    #   
    #   if (type == 'helmert') {
    #     
    #     for (i in seq_len(nLevels-1)) {
    #       rhs <- paste(levels[(i+1):nLevels], collapse=', ')
    #       if (nchar(rhs)>1) rhs<-paste0(" (",rhs,")")
    #       labels[[i]] <- paste(levels[i], '-', rhs)
    #     }
    #     return(labels)
    #   }
    #   
    #   
    #   
    #   if (type == 'repeated') {
    #     
    #     for (i in seq_len(nLevels-1))
    #       labels[[i]] <- paste(levels[i], '-', levels[i+1])
    #     return(labels)
    #     
    #   } 
    #   if (type == 'polynomial') {
    #     names <- c('linear', 'quadratic', 'cubic', 'quartic', 'quintic', 'sextic', 'septic', 'octic')
    #     for (i in seq_len(nLevels-1)) {
    #       if (i <= length(names)) {
    #         labels[[i]] <- names[i]
    #       } else {
    #         labels[[i]] <- paste('degree', i, 'polynomial')
    #       }
    #     }
    #     return(labels)
    #   }
    #   
    #   if (type == 'custom') {
    #         labels[[1]] <- "Custom" 
    #         for (i in 2:(nLevels-1)) labels[[i]]<-"Orthogonal"
    #         return(labels)
    #     }
    # 
    #   
    #   jinfo("no contrast definition met")
    #   
    #   all <- paste(levels, collapse=', ')
    #   for (i in seq_len(nLevels-1))
    #     labels[[i]] <- paste(levels[i+1], '- (', all,")")
    #   return(labels)
    # },
    
    .continuous_values=function(data) {

      if (nrow(data)==0)
           return(jmvcore::toNumeric(data[[self$name64]]))

      vardata<-data[[self$name64]]

      if (is.factor(vardata)) 
           vardata<-jmvcore::toNumeric(vardata)
           
      ## we first update levels to save the old levels
      private$.update_levels(vardata)
      
      
      method<-self$covs_scale
      
    

      if (method=="centered") 
        vardata<-scale(vardata,scale = F)  

      if (method=="standardized") 
        vardata<-scale(vardata,scale = T)  

      if (method=="log") {
        vardata<-log(vardata)  
        if (any(is.nan(vardata)))
          self$datamatic$error<-list(topic="info",message=paste("Negative values found in variable",self$name,". Log transform not applicable."))
      }
      
      if (method=="clusterbasedcentered") {   
        
        cluster64<-tob64(self$hasCluster[1])
        sdata<-data[,c(cluster64,self$name64)]
        sdata$..id..<-1:nrow(sdata)
        mdata<-aggregate(sdata[,self$name64],list(sdata[[cluster64]]),mean,na.rm=TRUE)
        names(mdata)<-c(cluster64,"mean")
        sdata<-merge(sdata,mdata,by=cluster64)
        sdata[[self$name64]]<-sdata[[self$name64]]-sdata[["mean"]]
        sdata<-sdata[order(sdata$..id..),]
        vardata<-sdata[[self$name64]]
        self$datamatic$warning<-list(topic="info",message=paste("Variable",self$name,"has been centered within clusters defined by",self$hasCluster[[1]]))

      }
      if (method=="clusterbasedstandardized") {    
        cluster64<-tob64(self$hasCluster[1])
        sdata<-data[,c(cluster64,self$name64)]
        sdata$..id..<-1:nrow(sdata)
        mdata<-aggregate(sdata[,self$name64],list(sdata[[cluster64]]),mean,na.rm=TRUE)
        names(mdata)<-c(cluster64,"mean")
        sdata<-merge(sdata,mdata,by=cluster64)
        sdata<-sdata[order(sdata$..id..),]
        ddata<-aggregate(sdata[,self$name64],list(sdata[[cluster64]]),sd,na.rm=TRUE)
        names(ddata)<-c(cluster64,"sd")
        ddata$sd[is.na(ddata$sd)]<-0
        if (any(ddata[["sd"]]<.0000001)) {
            self$datamatic$warning<-list(topic="info",message=paste("Variable",self$name,"has zero variance in at least one cluster defined by ",self$hasCluster[[1]]))
            self$datamatic$warning<-list(topic="info",message=paste("Cluster-standardized value with zero variance have been set to zero"))
        }
        ddata$sd[ddata$sd==0]<-1
        sdata<-merge(sdata,ddata,by=cluster64)
        sdata<-sdata[order(sdata$..id..),]
        sdata[[self$name64]]<-(sdata[[self$name64]]-sdata[["mean"]])/sdata[["sd"]]
        ## this is the beautiful clusterwise standardization for dep vars
        if (self$isDependent) {
          sdata$mean<-as.numeric(scale(sdata$mean))
          sdata[[self$name64]] <- sdata[[self$name64]]+sdata$mean 
        }
            
        vardata<-sdata[[self$name64]]
        self$datamatic$warning<-list(topic="info",message=paste("Variable",self$name,"has been standardized within clusters defined by",self$hasCluster[[1]]))
      }

      if (method=="clustermeans") {    

        cluster64<-tob64(self$hasCluster[1])
        sdata<-data[,c(cluster64,self$name64)]
        sdata$..id..<-1:nrow(sdata)
        mdata<-aggregate(sdata[,self$name64],list(sdata[[cluster64]]),mean,na.rm=TRUE)
        names(mdata)<-c(cluster64,"mean")
        sdata<-merge(sdata,mdata,by=cluster64)
        sdata<-sdata[order(sdata$..id..),]
        vardata<-sdata[["mean"]]
        self$datamatic$warning<-list(topic="info",message=paste("Variable",self$name,"represents means of clusters in",self$hasCluster[[1]]))
        
      }
      cluster64<-tob64(self$hasCluster[1])
      

      
      ## we then update levels the new levels (mean, sd etc)
      private$.update_levels(vardata)
      as.numeric(vardata)
      
    },
    
    .update_levels=function(vardata) {
      
      self$original_levels<-self$levels
      self$original_descriptive<-self$descriptive
      labels_type<-ifelse(is.null(self$datamatic$options$covs_scale_labels),"values",self$datamatic$options$covs_scale_labels)
      ### when called by init, force labels because we cannot compute the values
      ### if not, we can compute the descriptive
      if (length(vardata)==0)
           labels_type="labels"
      else
          self$descriptive<-list(min=min(vardata,na.rm = TRUE),
                              max=max(vardata,na.rm = TRUE),
                              mean=mean(vardata,na.rm = TRUE),
                              sd=sd(vardata,na.rm = TRUE))

    
      if (self$datamatic$options$covs_conditioning=="mean_sd")  {
        
        .span<-ifelse(is.null(self$datamatic$options$ccm_value),1,self$datamatic$options$ccm_value)
        .labs<-c(paste0("Mean-", .span, "\u00B7", "SD"), "Mean", paste0("Mean+", .span, "\u00B7","SD"))
        .mean <- mean(vardata,na.rm=T)
        .sd <- sd(vardata,na.rm=T)
        self$levels=round(c(.mean - (.span * .sd), .mean, .mean + (.span * .sd)),digits = 3)
        self$method="mean_sd"

      }
      if (self$datamatic$options$covs_conditioning=="percent") {
        
        .lspan<-ifelse(is.null(self$datamatic$options$ccp_value),25,self$datamatic$options$ccp_value)
        .span<-.lspan/100
        
        .labs<-c(paste0("50-", .lspan,"\u0025"), "50\u0025", paste0("50+", .lspan,"\u0025"))
        
        self$levels<-round(quantile(vardata, c(0.5 - .span, 0.5, 0.5 + .span),na.rm=TRUE), digits = 3) 
          
        self$method="percent"
      }

      if (self$datamatic$options$covs_conditioning=="range") {
        
         steps         <- ifelse(is.null(self$datamatic$options$ccra_steps),1,self$datamatic$options$ccra_steps)
         min           <- min(vardata,na.rm = TRUE)
         max           <- max(vardata,na.rm = TRUE)
         self$levels   <- round(epretty(min,max,steps),digits=3)
        .labs          <- rep("",length(self$levels))
        .labs[1]       <- "Min"
        .labs[length(.labs)]  <- "Max"
         self$method="range"
      }

      if (self$method == "range") {
        if ( labels_type == "labels")
                    labels_type <- "values_labels"
      }
      
      if (labels_type == "labels") 
        self$levels_labels<-.labs
      
      if (labels_type == "values") 
        self$levels_labels<-self$levels
      
      if (labels_type == "uvalues") 
        self$levels_labels<-self$original_levels
      
      
      if (labels_type == "values_labels") {
        self$levels_labels<-paste(.labs,self$levels,sep="=")
        self$levels_labels<-gsub("^\\=","",self$levels_labels)
      }
      
      if (labels_type == "uvalues_labels") {
        self$levels_labels<-paste(.labs,self$original_levels,sep="=")
        self$levels_labels<-gsub("^\\=","",self$levels_labels)
      }
      if (all(!is.nan(self$levels)) &  all(!is.na(self$levels)))
            if(any(duplicated(self$levels))) {
               self$levels<-unique(self$levels)
               self$datamatic$warning<-list(topic="simpleEffects_anova",message=paste0("Problems in covariates conditioning for variable ",self$name,". Values are not differentiable, results may be misleading. Please enlarge the offset or change the conditioning method."))
               self$datamatic$warning<-list(topic="simpleEffects_coefficients",message=paste0("Problems in covariates conditioning for variable ",self$name,". Values are not differentiable, results may be misleading. Please enlarge the offset or change the conditioning method."))
               
            }

      
    }
    

  ) #end of private
) # end of class




