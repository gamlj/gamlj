

.scaleVariables=function(factors,covariates,data) {
  for(factor in factors)
     data[[factor]]<-factor(data[[factor]])
  for(covariate in covariates)
     data[[covariate]] <- jmvcore::toNumeric(data[[covariate]])
  data
}

lf.createContrasts=function(levels, type) {
  
  nLevels <- length(levels)

  if (type == 'simple') {
    dummy <- contr.treatment(levels)
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

    contrast <- stats::contr.treatment(levels)
    dimnames(contrast) <- NULL
    
  } else {
      contrast <- matrix(0, nrow=nLevels, ncol=nLevels-1)
      for (i in seq_len(nLevels-1)) {
        contrast[i+1, i] <- 1
        contrast[1, i] <- -1
      }
  }
  dimnames(contrast)<-list(NULL,paste0("_._._",1:(nLevels-1)))
  contrast
}

lf.contrastLabels=function(levels, type) {
  nLevels <- length(levels)
  labels <- list()

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

lf.scaleContinuous<-function(var,method,by=NULL) {

  if (method=="centered") 
          var<-scale(var,scale = F)  
  if (method=="cluster-based centered") {    
          var<-unlist(tapply(var,by,scale,scale=F))
          print(tapply(var,by,mean))
  }
  if (method=="standardized") 
          var<-scale(var,scale = T)  
  if (method=="cluster-based standardized")     
          var<-unlist(tapply(var,by,scale,scale=T))
  as.numeric(var)
}





