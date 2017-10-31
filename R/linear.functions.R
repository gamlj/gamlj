.nicifyTerms<-function(term) {
  term<-gsub("`","",term,fixed = T)
  gsub(":"," ✻ ",term)
}

.getDummiesNames<-function(varname,data) {
  if (!(varname %in% names(data)))
    return(varname)
  if (is.factor(data[,varname]))
  {
    paste(varname,1:length(levels(data[,varname])[-1]),sep="")
  } else 
    varname
}


.scaleVariables=function(factors,covariates,data) {
  for(factor in factors)
     data[[factor]]<-factor(data[[factor]])
  for(covariate in covariates)
     data[[covariate]] <- jmvcore::toNumeric(data[[covariate]])
  data
}

.createContrasts=function(levels, type) {
  
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
      contrast[i,  i] <- 1
      contrast[i+1,i] <- -1
    }
    
  } else {
      contrast <- matrix(0, nrow=nLevels, ncol=nLevels-1)
      for (i in seq_len(nLevels-1)) {
        contrast[i+1, i] <- 1
        contrast[1, i] <- -1
      }
  }
  
  contrast
}

.contrastLabels=function(levels, type) {
  
  nLevels <- length(levels)
  labels <- list()
  
  if (length(levels) <= 1) {
    
    # do nothing
  } else if (type == 'simple') {
    
    for (i in seq_len(nLevels-1))
      labels[[i]] <- paste(levels[i+1], '-', levels[1])
    
  } else if (type == 'dummy') {

    for (i in seq_len(nLevels-1))
      labels[[i]] <- paste(levels[i+1], '-', levels[1])
    
  } else if (type == 'simple') {
    
    for (i in seq_len(nLevels-1))
      labels[[i]] <- paste(levels[i+1], '-', levels[1])
    
  } else if (type == 'deviation') {
    
    all <- paste(levels, collapse=', ')
    for (i in seq_len(nLevels-1))
      labels[[i]] <- paste(levels[i+1], '- (', all,")")
    
  } else if (type == 'difference') {
    
    for (i in seq_len(nLevels-1)) {
      rhs <- paste0(levels[1:i], collapse=', ')
      if (nchar(rhs)>1) rhs<-paste0(" (",rhs,")")
      labels[[i]] <- paste(levels[i + 1], '-', rhs)
    }
    
  } else if (type == 'helmert') {
    
    for (i in seq_len(nLevels-1)) {
      rhs <- paste(levels[(i+1):nLevels], collapse=', ')
      if (nchar(rhs)>1) rhs<-paste0(" (",rhs,")")
      labels[[i]] <- paste(levels[i], '-', rhs)
    }
    
  } else if (type == 'repeated') {
    
    for (i in seq_len(nLevels-1))
      labels[[i]] <- paste(levels[i], '-', levels[i+1])
    
  } else if (type == 'polynomial') {
    
    names <- c('linear', 'quadratic', 'cubic', 'quartic', 'quintic', 'sextic', 'septic', 'octic')
    
    for (i in seq_len(nLevels-1)) {
      if (i <= length(names)) {
        labels[[i]] <- names[i]
      } else {
        labels[[i]] <- paste('degree', i, 'polynomial')
      }
    }
  } else {
    all <- paste(levels, collapse=', ')
    for (i in seq_len(nLevels-1))
      labels[[i]] <- paste(levels[i+1], '- (', all,")")
  }
  labels
}

.scaleContinuous<-function(var,method,by=NULL) {
  
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
  vars<-as.character(attr(terms.formula(formula),"variables"))
  vars<-vars[c(-1,-2)]
  laylist<-sapply(layout,function(a) strsplit(a,":",fixed = T))
  
  for (i in seq_along(vars)) {
    if (vars[i] %in% names(labels)) {
      laylist<-sapply(laylist, function(a) {
        a[which(a == vars[i])]<-paste(unlist(labels[vars[i]]),collapse = "#")
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
    modelterms<-terms(model)
    ff<-as.data.frame(attr(modelterms,"factors"))
    termorder<-.term.order(term)
    terms<-unlist(strsplit(term,":",fixed=T))
  
    for (aterm in terms) {
         if(sum(ff[rownames(ff)==aterm,])>termorder)
            return(TRUE)
    }
    
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
    ff<-strsplit(ff,":")
    for(f in ff)
        if(all(f %in% aList) & all(aList %in% f) )
           return(paste(f,collapse = ":"))
}



########## simple effects computation #########

#### this estimates a model at some level of the moderator

.atSomeLevel<-function(model,moderator,level) {

    data<-mf.getModelData(model)
    
    if (is.factor(data[,moderator])) {
          .levels<-levels(data[,moderator])
           index<-which(.levels==level,arr.ind = T)
           ### we need the moderator to be treatment to condition the other effect to its reference groups
           contrasts(data[,moderator])<-contr.treatment(length(.levels),base=index)
      } else {
           data[,moderator]<-data[,moderator]-level
      }
  
      form<-formula(model)
      FUN<-mf.estimate(model)
      FUN(form,data)
}

.extractParameters<-function(model,variable,moderator,level) {
  ### get the parameters
  ##### understand how the dummies are named #####
       data<-mf.getModelData(model)
  
        if (is.factor(data[[variable]])) {
            varname<-paste(variable,seq_along(levels(data[,variable])),sep="")
         }  else {
            varname<-variable
         }
  ##### get the summary of the model. This must be a ...
        ss<-mf.summary(model)
        ##### extract only the estimates of the x-axis variable
        if ("variable" %in% names(ss))
             ss<-ss[ss$variable==varname,]
        else {
             a<-as.logical(apply(sapply(varname, function(a) a==rownames(ss)),1,sum))
             ss<-ss[a,]
        }
        #### prettify the levels ####
        if (is.numeric(level)) 
            level<-round(level,digits=2)
        
        ss$level<-paste(moderator,level,sep=" at ")
        ss$variable<-variable
        as.data.frame(ss,stringsAsFactors=F)
}


.extractFtests<-function(model,variable,moderator,level) {
  ### get the ANOVA
  if (.which.class(model)=="lmer") {
    if (!lme4::getME(model,"is_REML"))
      return(FALSE)
  }
  ano<-mf.anova(model)
  ano<-ano[rownames(ano)==variable,]
  if (is.numeric(level)) level<-round(level,digits=2)
  ano$level<-paste(moderator,level,sep=" at ")
  ano$variable<-variable
  as.data.frame(ano,stringsAsFactors=F)
}



lf.simpleEffects<-function(model,variable,moderator){
  
  # The simple effects function computes up to 3-ways simple effects
  # for any (hopefully) linear model. Given the different estimation
  # method of the linear models (lm(), glm(), lmer()), the module
  # must have three functions: 
  # mf.estimate() : should return the model estimate function
  # mf.summary() : should return the model summary table as a 
  #                      dataset (usually the model$coefficients table)
  # mf.anova() : should return the anova table or an equivalent
  
   
  data<-mf.getModelData(model)
  
  modvar<-data[,moderator]
  if (is.factor(modvar)) {
    levels<-levels(modvar)
  } else {
    levels<-c(mean(modvar)+sd(modvar),mean(modvar),mean(modvar)-sd(modvar))
  }
  params<-data.frame(stringsAsFactors = F)
  ftests<-data.frame(stringsAsFactors = F)
  for (i in levels) {
       model0<-.atSomeLevel(model,moderator,i)
       params<-rbind(params,.extractParameters(model0,variable,moderator,i))
       ftests<-rbind(ftests,.extractFtests(model0,variable,moderator,i))
  }
  list(params,ftests)
}


lf.dependencies<-function(model,term,modelTerms,what) {
  if (.is.scaleDependent(model,term))
    return(paste(what,"interactions",sep="."))
   else if (.term.develop(term)<length(modelTerms))
       return(paste(what,"covariates",sep="."))
  FALSE
  }


lf.meansTables<-function(model,terms) {
  factorsAvailable<-mf.getModelFactors(model)
  tables<-list()
  for (term in terms)
    if (all(term %in% factorsAvailable)) {
      table<-lsmeans::lsmeans(model,term,transform = "response")
      table<-as.data.frame(summary(table))
      print(table)
      table<-table[,-(1:length(term))]
      attr(table,"title")<-term
      depend<-lf.dependencies(model,term,terms,"means")
      if (depend!=FALSE) {
        attr(table,"note")<-depend
      }
      tables[[length(tables)+1]]<-table
    }
  tables
}
