.nicifyTerms<-function(term) {
  term <- jmvcore::decomposeTerm(term)
  term <- jmvcore::stringifyTerm(term)
  term
}



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
  if (method=="cluster-based centered")    
          var<-unlist(tapply(var,by,scale,scale=F))
  if (method=="standardized") 
          var<-scale(var,scale = T)  
  if (method=="cluster-based standardized")     
          var<-unlist(tapply(var,by,scale,scale=T))
  as.numeric(var)
}



# This is a crazy piece of software to extract labels of contrasts and
# merge them to the names of the terms of a summary.lm(). Before thinking
# that it is too elaborated, please consider that this would attach the right
# labels even in cases where two different model terms have the same name (it may happen) 
# or one dummy gets the same name of a continuous variable (this may happens too).


.getFormulaContrastsLabels<-function(contrasts,formula,data) {

  contrasts<-as.data.frame(do.call(rbind,contrasts))
  modelmatrix<-model.matrix(formula,data)
  facts<-names(attr(modelmatrix,"contrasts"))

  labels<-sapply(facts, function(varname) {
    if (varname %in% contrasts$var) {
      contrasttype<-contrasts[contrasts$var==varname,"type"]
    } else {
      contrasttype<-"deviation"
    }
    
    var<-data[,varname]
    levels <- base::levels(var)
    .contrastLabels(levels, contrasttype)
  },simplify = F)
  
  layout<-attr(terms.formula(formula),"term.labels")
  for (i in seq_along(layout))
     layout[i]<-gsub("`","",layout[i],fixed=T)
  laylist<-sapply(layout,function(a) jmvcore::decomposeTerm(a))
  vars<-as.character(attr(terms.formula(formula),"variables"))
  vars<-vars[c(-1,-2)]
#  laylist<-sapply(layout,function(a) strsplit(a,":",fixed = T))

    for (i in seq_along(vars)) {
      var<-jmvcore::decomposeTerm(vars[i])
    if (var %in% names(labels)) {
      laylist<-sapply(laylist, function(a) {
        a[which(a == var)]<-paste(unlist(labels[var]),collapse = "#")
        a
      })
    }
  }
  laylist<-sapply(laylist,function(a) strsplit(a,"#",fixed = T))
  final<-list()
  nr<-1
  for (j in seq_along(laylist)) {
    lay<-laylist[j]
    records<-expand.grid(lay[[1]],stringsAsFactors = F)
    ready<-apply(records,1,paste,collapse=":")
    for (i in seq_along(ready)) {
      final[nr]<-ready[i]
      nr<-nr+1
    }
  } 
  ### add an empty contrast for the intercept ####
  c("Intercept",unlist(final))
}


### this tells if a model term is dependent on the interaction
.is.scaleDependent<-function(model,term) {
    if (is.null(term))
      return(FALSE)
    try({
    modelterms<-terms(model)
    modelterms<-attr(modelterms,"term.labels")
    nterm<-paste(term,collapse = ":")
    count<-length(grep(nterm,modelterms,fixed=T))
    if (count>1)
      return(TRUE)
    })
    FALSE
}


.term.develop<-function(term){
     n<-.term.order(term)
     (2^n)-1
}

.term.order<-function(term) {
  
    length(unlist(strsplit(term,":",fixed=T)))
  
}

.interaction.term<-function(model,aList) {
  
    ff<-colnames(attr(terms(model),"factors"))
    ff<-jmvcore::decomposeTerms(ff)
    for(f in ff)
        if(all(f %in% aList) & all(aList %in% f) )
           return(paste(f,collapse = ":"))
}





lf.dependencies<-function(model,term,what) {
  if (.is.scaleDependent(model,term))
    return(paste(what,"interactions",sep="."))
   else {
     modelterms<-terms(model)
     modelterms<-attr(modelterms,"term.labels")
     if (.term.develop(term)<length(modelterms))
       return(paste(what,"covariates",sep="."))
   }
  FALSE
}




