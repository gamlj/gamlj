#' @import ggplot2

gamljMixedClass <- R6::R6Class(
  "gamljMixedClass",
  inherit=gamljMixedBase,
  private=list(
    .model=NA,
    .names64=NA,
    .postHocRows=NA,
    .init=function() {
      mark("init")
      private$.names64<-names64$new()
      n64<-private$.names64
      reml<-self$options$reml
      infoTable<-self$results$info
      
      getout<-FALSE
      if (is.null(self$options$dep)) {
        infoTable$addRow(rowKey="gs1",list(info="Get started",value="Select the dependent variable"))
        getout<-TRUE
      }
      if (is.null(self$options$cluster)) {
        infoTable$addRow(rowKey="gs2",list(info="Get started",value="Select at least one cluster variable"))
        getout=TRUE
      }
      if (length(self$options$randomTerms)==0) {
        infoTable$addRow(rowKey="gs3",list(info="Get started",value="Select at least one term in Random Effects"))
        getout=TRUE
      }
      
      if (getout) {
        if (length(self$options$modelTerms) == 0) {
          infoTable$addRow(rowKey="gs4",list(info="Optional",value="Select factors and covariates"))
        }
        return(FALSE)
      }
      modelTerms<-private$.modelTerms()
      data<-private$.cleandata()

      #### info table #####
      infoTable<-self$results$info
      infoTable$addRow(rowKey="est",list(info="Estimate"))
      
      infoTable$addRow(rowKey="call",list(info="Call"))
      infoTable$addRow(rowKey="aic",list(info="AIC"))
      if (!(reml)) {
      infoTable$addRow(rowKey="bic",list(info="BIC"))
      infoTable$addRow(rowKey="log",list(info="LogLikel."))
      }
      infoTable$addRow(rowKey="r2m",list(info="R-squared Marginal"))
      infoTable$addRow(rowKey="r2c",list(info="R-squared Conditional"))
      
      
      ## random table
      aTable<-self$results$random
#      if (length(self$options$randomTerms)>0) {
#        nr<-1
#        for(term in self$options$randomTerms) {
#          labels<-gsub(" ","",strsplit(term,"|",fixed = T)[[1]],fixed=T)
#          dummies<-mi.getDummiesNames(labels[1],data)
#          for (dummy in dummies) {
#            aTable$addRow(rowKey=nr,list(groups=labels[2],name=dummy))
#            nr<-nr+1
#          }
#        }
        aTable$addRow(rowKey="res",list(groups="Residuals"))
#      }
      
      ## anova Table 
      if (length(modelTerms)>0) {
          aTable<- self$results$anova
          for (i in seq_along(modelTerms)) 
                  aTable$addRow(rowKey=i, list(name=" "))
      }
      
      ## fixed effects parameters

      aTable<-self$results$fixed
      formula<-private$.fixedFormula()
      mynames64<-colnames(model.matrix(formula,data))
      terms<-n64$nicenames(mynames64)  
      labels<-n64$nicelabels(mynames64)
      ciWidth<-self$options$paramCIWidth
      aTable$getColumn('cilow')$setSuperTitle(jmvcore::format('{}% Confidence Interval', ciWidth))
      aTable$getColumn('cihig')$setSuperTitle(jmvcore::format('{}% Confidence Interval', ciWidth))
      for(i in seq_along(terms)) 
          aTable$addRow(rowKey=i,list(source=.nicifyTerms(terms[[i]]),label=.nicifyTerms(labels[i])))

      ######### simple effects tables ##############
      
      
        
        # other inits
        private$.initPlots(data)
        tables   <- self$results$postHoc
        tables<-rf.initPostHoc(data,self$options, tables,modelType="linear")     
        rf.initEMeans(data,self$options,self$results$emeansTables)
        rf.initSimpleEffects(data,self$options,self$results)
    },
    .run=function() {
      n64<-private$.names64
      mark("run")
      # collect some option
      dep <- self$options$dep
      factors <- self$options$factors
      covs <- self$options$covs
      clusters<-self$options$cluster
      reml<-self$options$reml
      
      if (length(self$options$modelTerms)==0)
        return()
      
      if (self$options$simpleScale=="mean_offset" && self$options$cvalue==0)
          return()
      if (self$options$simpleScale=="percent_offset" && self$options$percvalue==0)
        return()

      modelTerms<-private$.modelTerms()
      
      ### collect the tables #######
      infoTable<-self$results$info
      fixedTable <- self$results$fixed
      randomTable <- self$results$random
      randomCovTable<-self$results$randomCov
      aTable<-self$results$anova
      
      ####  prepare the model formula              ####      
            
      modelFormula<-private$.modelFormula()

      if (modelFormula!=FALSE)    {
        
        data<-private$.cleandata()
        
        data<-mf.checkData(self$options,data,"mixed")
        if (!is.data.frame(data))
          reject(data)

        for (scaling in self$options$scaling) {
          cluster<-jmvcore::toB64(clusters[[1]])
          data[[jmvcore::toB64(scaling$var)]]<-.scaleContinuous(data[[jmvcore::toB64(scaling$var)]],scaling$type,data[[jmvcore::toB64(cluster)]])  
        }
        
        aRun <- try({
            model<-private$.estimate(modelFormula, data=data,REML = reml)
            wars<-warnings()
            })
        if (jmvcore::isError(aRun)) 
          jmvcore::reject(jmvcore::extractErrorMessage(aRun), code='error')
          test_parameters<-try(parameters<-mf.summary(model))
          if (isError(test_parameters)) {
            message <- extractErrorMessage(test_parameters)
          }
        if (!is.null(attr(parameters,"warning"))) 
            fixedTable$setNote(attr(parameters,"warning"),WARNS[as.character(attr(parameters,"warning"))])

        private$.model <- model
        ss<-summary(model)
        ### prepare info table #########       
        info.call<-n64$translate(as.character(model@call)[[2]])
        info.title<-paste("Linear mixed model fit by",ifelse(reml,"REML","ML"))
        info.aic<-ss$AICtab[1]
        info.bic<-ss$AICtab[2]
        info.loglik<-ss$AICtab[3]
        r2<-try(r.squared(model))
        if (jmvcore::isError(r2)){
          info.r2m<-NaN        
          info.r2c<-NaN
          infoTable$setNote("r2","R-squared cannot be computed.")  
        } else {
          info.r2m<-r2[[4]]        
          info.r2c<-r2[[5]]     
        }
        infoTable$setRow(rowKey="est", list(value=info.title))
        infoTable$setRow(rowKey="call",list(value=info.call))
        infoTable$setRow(rowKey="aic",list(value=info.aic))
        if (!(reml)) {
            infoTable$setRow(rowKey="bic",list(value=info.bic))
            infoTable$setRow(rowKey="log",list(value=info.loglik))
        }
        infoTable$setRow(rowKey="r2m",list(value=info.r2m))
        infoTable$setRow(rowKey="r2c",list(value=info.r2c))
        
        ### end of info table ###
        
        ### random table ######        
        vc<-as.data.frame(ss$varcor)
        vcv<-vc[is.na(vc[,3]),]
        vcv$var1[is.na(vcv$var1)]<-""
        grp<-unlist(lapply(vcv$grp, function(a) gsub("\\.[0-9]$","",a)))
        realgroups<-n64$nicenames(grp)
        realnames<-n64$nicenames(vcv$var1)
        for (i in 1:dim(vcv)[1]) {
          if (i<=randomTable$rowCount)
            randomTable$setRow(rowNo=i, list(groups=realgroups[[i]],name=realnames[[i]],std=vcv$sdcor[i],var=vcv$sdcor[i]^2))
          else
            randomTable$addRow(rowKey=i, list(groups=realgroups[[i]],name=realnames[[i]],std=vcv$sdcor[i],var=vcv$sdcor[i]^2))
        }
        info<-    paste("Numer of Obs:", ss$devcomp$dims["n"],", groups:",n64$nicenames(names(ss$ngrps)),",",ss$ngrps,collapse = "")
        randomTable$setNote('info', info)
        

        ### Covariance among random effects ###
        vcv<-vc[!is.na(vc[,3]),]
        grp<-unlist(lapply(vcv$grp, function(a) gsub("\\.[0-9]$","",a)))
        realgroups<-n64$nicenames(grp)
        realnames1<-n64$nicenames(vcv$var1)
        realnames2<-n64$nicenames(vcv$var2)

        if (dim(vcv)[1]>0) {
          for (i in 1:dim(vcv)[1]) {
            randomCovTable$addRow(rowKey=realgroups[[i]], list(groups=realgroups[[i]],name1=realnames1[[i]],name2=realnames2[[i]],cov=vcv$sdcor[i]))
          }
          randomCovTable$setVisible(TRUE)
        }
        ### ### ### ### ###
        
        # anova table ##
        if (length(modelTerms)==0) {
                    aTable$setNote("warning","F-Tests cannot be computed without fixed effects")
        } else {
               suppressWarnings({
                      anova <- try(mf.anova(model), silent=TRUE) # end suppressWarnings
           })
        if (jmvcore::isError(anova)) 
          jmvcore::reject(jmvcore::extractErrorMessage(anova), code='error')
        rawlabels<-rownames(anova)
        labels<-n64$nicenames(rawlabels)
        for (i in seq_len(dim(anova)[1])) {
            tableRow<-anova[i,]  
            aTable$setRow(rowNo=i,tableRow)
            aTable$setRow(rowNo=i,list(name=.nicifyTerms(labels[i])))
         }
        messages<-mf.getModelMessages(model)
        for (i in seq_along(messages)) {
                 aTable$setNote(names(messages)[i],messages[[i]])
                 infoTable$setNote(names(messages)[i],messages[[i]])
        }
        if (length(messages)>0) {
            infoTable$setNote("lmer.nogood",WARNS["lmer.nogood"])
        }
        if (attr(anova,"statistic")=="Chisq") {
            aTable$setNote("lmer.chisq",WARNS["lmer.chisq"])
            aTable$getColumn('test')$setTitle("Chi-squared")
            aTable$getColumn('df1')$setTitle("df")
            aTable$getColumn('df2')$setVisible(FALSE)
        } else
             aTable$setNote("df",paste(attr(anova,"method"),"method for degrees of freedom"))
        
        
        ### parameter table ####
        
        #### confidence intervals ######
        ciWidth<-self$options$paramCIWidth/100
        if (self$options$showParamsCI) {
          citry<-try({
            ci<-mf.confint(model,level=ciWidth)
            colnames(ci)<-c("cilow","cihig")
            parameters<-cbind(parameters,ci) 
          })
          if (isError(citry)) {
            message <- extractErrorMessage(citry)
            fixedTable$setNote("cicrash",paste(message,". CI cannot be computed"))
          }
        }

        
        rownames(parameters)<-n64$nicenames(rownames(parameters))
        for (i in 1:nrow(parameters)) {
                tableRow=parameters[i,]
                fixedTable$setRow(rowNo=i,tableRow)
          }

        
       if (mf.aliased(model)) {
          fixedTable$setNote("aliased",WARNS["ano.aliased"])
          infoTable$setNote("aliased",WARNS["ano.aliased"])
       }
        }
        
        
        private$.preparePlots(private$.model)
        private$.populateSimple(private$.model)
        private$.populatePostHoc(private$.model)
        private$.populateDescriptives(private$.model)

  }

    },
  .buildreffects=function(terms,correl=TRUE) {
    terms<-lapply(terms,jmvcore::toB64)

    flatterms<-lapply(terms,function(x) c(paste0(head(x,-1),collapse = ":"),tail(x,1)))
    res<-do.call("rbind",flatterms)
    res<-tapply(res[,1],res[,2],paste)
    if (correl) {
      res<-sapply(res, function(x) paste(x,collapse = " + "))
      form<-paste(res,names(res),sep="|")
      form<-paste("(",form,")")
    } else {
      form<-sapply(names(res), function(x) sapply(res[[x]], function(z) paste0("(0+",paste(z,x,sep = "|"),")")))
      form<-sapply(form, function(x) paste(x,collapse = "+"))
    } 
    form<-gsub(jmvcore::toB64('Intercept'),1,form,fixed = T)
    form<-gsub('0+1',1,form,fixed = T)
    form=paste(form,collapse = "+")
    form
  },
  .cleandata=function() {
      n64<-private$.names64
      
      dep <- self$options$dep
      factors <- self$options$factors
      covs <- self$options$covs
      clusters<-self$options$cluster
      
      dataRaw <- self$data
      data <- list()
      for (factor in factors) {
        data[[jmvcore::toB64(factor)]] <- dataRaw[[factor]]
        levels <- base::levels(data[[jmvcore::toB64(factor)]])
        stats::contrasts(data[[jmvcore::toB64(factor)]]) <- lf.createContrasts(levels,"deviation")
        n64$addFactor(factor,levels)
        n64$addLabel(factor,lf.contrastLabels(levels, "deviation")) 
        attr(data[[jmvcore::toB64(factor)]],"jcontrast")<-"deviation"
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
      for (cluster in clusters) {
        data[[jmvcore::toB64(cluster)]] <- as.factor(dataRaw[[cluster]])
        n64$addVar(cluster)
      }
      
      for (covariate in covs) {
        data[[jmvcore::toB64(covariate)]] <- jmvcore::toNumeric(dataRaw[[covariate]])
        n64$addVar(covariate)
      }
      private$.names64<-n64
      attr(data, 'row.names') <- seq_len(length(data[[1]]))
      attr(data, 'class') <- 'data.frame'      
      data <- jmvcore::naOmit(data)
      return(data)
      
    },
    .estimate = function(form, data,REML=TRUE) {
      ## there is a bug in LmerTest and it does not work
      ## when called within an restricted environment such as a function.
      ## the do.call is a workaround.
      lm = do.call(lmerTest::lmer, list(formula=form, data=data,REML=REML))
      return(lm)
    },
    .modelFormula=function() {

      if (length(self$options$randomTerms)>0)  {
        rands<-self$options$randomTerms
        rands<-private$.buildreffects(rands,self$options$correlatedEffects)
      } else return(FALSE)

      if (!is.null(self$options$dep))  {
        dep<-jmvcore::toB64(self$options$dep)
      } else return(FALSE)
      
      intercept<-as.numeric(self$options$fixedIntercept)
      terms<-sapply(private$.modelTerms(),jmvcore::toB64)
      fixs<-jmvcore::constructFormula(dep=NULL,terms) 
      paste(paste(dep,intercept,sep="~"),rands,fixs,sep = "+")
    },


.populatePostHoc=function(model) {
  terms <- self$options$postHoc
  dep<-self$options$dep
  if (length(terms) == 0)
    return()
  
  tables <- self$results$postHoc
  
  postHocRows <- list()
  
  for (ph in terms) {
    
    table <- tables$get(key=ph)
    term <- jmvcore::composeTerm(ph)
    termB64 <- jmvcore::composeTerm(toB64(ph))
    suppressWarnings({
      none <- mf.posthoc(model,termB64,"none")
      bonferroni <- mf.posthoc(model,termB64,"bonferroni")
      holm <-mf.posthoc(model,termB64,"holm")
      tukey <-mf.posthoc(model,termB64,"tukey")
      
    }) # suppressWarnings
    if (is.character(none))
      table$setNote("nojoy",WARNS["ph.nojoy"])
    else {        
      tableData<-as.data.frame(none)
      tableData$contrast<-as.character(tableData$contrast)
      colnames(tableData)<-c("contrast","estimate","se","df","test","p")
      tableData$pbonf<-bonferroni[,6]
      tableData$pholm<-holm[,6]
      tableData$ptukey<-tukey[,6]
    }
    .labs<-sapply(tableData$contrast, function(a) {
      strsplit(a,"[-,]")
    })
    labs<-do.call("rbind",.labs)   
    colnames(labs)<-paste0("c",1:ncol(labs))
    for (i in 1:nrow(tableData)) {
      row<-tableData[i,]
      l<-labs[i,]
      table$setRow(rowNo=i, values=c(row,l))
    }
  }
},

    .initPlots=function(data) {
       isAxis <- ! is.null(self$options$plotHAxis)
       isMulti <- ! is.null(self$options$plotSepPlots)
       
         self$results$get('descPlot')$setVisible( ! isMulti && isAxis)
         self$results$get('descPlots')$setVisible(isMulti)
    
       if (isMulti) {
         sepPlotsName <- self$options$plotSepPlots
         sepPlotsVar <- data[[toB64(sepPlotsName)]]
         if(is.factor(sepPlotsVar))
            sepPlotsLevels <- levels(sepPlotsVar)
         else {
             if (self$options$simpleScale=="mean_sd")
               sepPlotsLevels <- c("-1 SD","Mean","+1 SD")   
             if (self$options$simpleScale=="mean_offset")
               sepPlotsLevels <- c(paste0("Mean-",self$options$cvalue),"Mean",paste0("Mean+",self$options$cvalue))   
             if (self$options$simpleScale=="percent")
               sepPlotsLevels <- c("25%","50%","75%")   
             if (self$options$simpleScale=="percent_offset") {
               sepPlotsLevels <- c(paste0((50-self$options$percvalue),"%"),"50%",paste0((50+self$options$percvalue),"%"))   
             }
         }
         array <- self$results$descPlots
         for (level in sepPlotsLevels) {
             title<-paste(sepPlotsName,"=",level)
             array$addItem(title)
         }
        }
  },
.preparePlots=function(model) {
  
  depName <- self$options$dep
  groupName <- self$options$plotHAxis
  if (length(depName) == 0 || length(groupName) == 0)
    return()
  
  linesName <- self$options$plotSepLines
  plotsName <- self$options$plotSepPlots
  errorBarType<-self$options$plotError
  ciWidth   <- self$options$ciWidth
  optionRaw<-self$options$plotRaw
  optionRange<-self$options$plotDvScale
  referToData<-(optionRaw || optionRange)
  
  plotScale<-self$options$simpleScale
  offset<-ifelse(self$options$simpleScale=="percent_offset",self$options$percvalue,self$options$cvalue)
  
  
  if (referToData)
    rawData=lp.rawData(model,depName,groupName,linesName)
  else 
    rawData<-NULL
  
  predData<-lp.preparePlotData(model,
                               groupName,
                               linesName,
                               plotsName,
                               errorBarType,
                               ciWidth,
                               conditioning=self$options$simpleScale,span=offset,
                               labels=self$options$simpleScaleLabels)
  yAxisRange <- lp.range(model,depName,predData,rawData)
  
  if (!optionRaw)
    rawData<-NULL
  
  
  if (is.null(plotsName)) {
    image <- self$results$get('descPlot')
    image$setState(list(data=predData, raw=rawData, range=yAxisRange))
  } else {
    images <- self$results$descPlots
    i<-1
    levels<-levels(factor(predData$plots))
    for (key in images$itemKeys) {
      real<-levels[i]
      i<-i+1
      image <- images$get(key=key)
      image$setState(list(data=subset(predData,plots==real),raw=rawData, range=yAxisRange))
    }
  }
  
},

.descPlot=function(image, ggtheme, theme, ...) {
  library(ggplot2)
  if (is.null(image$state))
    return(FALSE)
  
  depName <- self$options$dep
  groupName <- self$options$plotHAxis
  linesName <- self$options$plotSepLines
  plotsName <- self$options$plotSepPlots
  errorType <- self$options$plotError
  ciWidth   <- self$options$ciWidth
  
  
  if (errorType=="ci")
    errorType<-paste0(ciWidth,"% ",toupper(errorType))
  
  if ( ! is.null(linesName)) {
    p<-.twoWaysPlot(image,theme,depName,groupName,linesName,errorType)
  } else {
    p<-.oneWayPlot(image,theme,depName,groupName,errorType)
  }       
  p<-p+ggtheme
  print(p)
  TRUE
},
.populateDescriptives=function(model) {

  terms<-private$.modelTerms()
  if (self$options$eDesc) {
    meanTables<-self$results$emeansTables
    tables<-lf.meansTables(model,terms)  
    for (table in tables)  {
      key<-.nicifyTerms(jmvcore::composeTerm(attr(table,"title")))    
      aTable<-meanTables$get(key=key)
      for (i in seq_len(nrow(table))) {
        values<-as.data.frame(table[i,])
        aTable$setRow(rowNo=i,values)
      }
      note<-attr(table,"note")
      if (!is.null(note)) aTable$setNote(note,WARNS[note])
    }
  } # end of eDesc              
  
  
  
},
.populateSimple=function(model) {
  variable<-self$options$simpleVariable
  moderator<-self$options$simpleModerator
  threeway<-self$options$simple3way
  simpleScale<-self$options$simpleScale
  offset<-ifelse(self$options$simpleScale=="percent_offset",self$options$percvalue,self$options$cvalue)
  interval<-self$options$paramCIWidth

    if (is.null(variable) | is.null(moderator)) 
          return()
  aTable<-self$results$simpleEffectsAnova
  pTable<-self$results$simpleEffectsParams
  
  aRun <- try({
    tables<-pred.simpleEstimates(model,
                                 toB64(variable),toB64(moderator),toB64(threeway),
                                 conditioning=simpleScale,span=offset,
                                 interval=interval,
                                 labels=self$options$simpleScaleLabels) 
  })
  if (jmvcore::isError(aRun)) {
     aTable$setNote("se.noluck",WARNS[["se.noluck"]])
  mark(jmvcore::extractErrorMessage(aRun))
  return()
  }
    
  ### fill the Anova Table ###
  table<-tables[[2]]
  for(r in seq_len(nrow(table))) {
    aTable$setRow(rowNo=r,table[r,])
  }
  table<-tables[[1]]
  
  for(r in seq_len(nrow(table))) {
    pTable$setRow(rowNo=r,table[r,])
  }
  
},

.fixedFormula=function() {
  # If we are in interactive mode the model should be well specified, otherwise (if R mode)
  # no modelTerms means full model. fix this
  modelTerms <- private$.modelTerms()
  dep <- self$options$dep
  dep64 <- jmvcore::toB64(dep)
  intercept<-as.numeric(self$options$fixedIntercept)
  terms<-sapply(private$.modelTerms(),jmvcore::toB64)
  fixs<-jmvcore::constructFormula(dep=NULL,terms) 
  lformula<-paste(paste(dep64,intercept,sep="~"),fixs,sep = "+")
  
  return(as.formula(lformula))
},


.modelTerms=function() {
  # If we are in interactive mode the model should be well specified, otherwise (if R mode)
  # no modelTerms means full model
  modelTerms <- self$options$modelTerms
  if (class(modelTerms)!="list")
       modelTerms<-private$.ff()
  modelTerms
},
  .ff=function() {
    modelTerms <- as.list(c(self$options$factors,self$options$covs))
},
    .sourcifyOption = function(option) {
  
  name <- option$name
  value <- option$value
  
  if (name == 'contrasts') {
    i <- 1
    while (i <= length(value)) {
      item <- value[[i]]
      if (item$type == 'default')
        value[[i]] <- NULL
      else
        i <- i + 1
    }
    if (length(value) == 0)
      return('')
  } else if (name == 'modelTerms') {
    if (base::identical(as.list(value), private$.ff()))
      return('')
  } else if (name == 'postHoc') {
    if (length(value) == 0)
      return('')
  }
  
  super$.sourcifyOption(option)
}
))


