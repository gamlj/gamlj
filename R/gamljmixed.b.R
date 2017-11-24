#' @import ggplot2
gamljMixedClass <- R6::R6Class(
  "gamljMixedClass",
  inherit=gamljMixedBase,
  private=list(
    .model=NA,
    .postHocRows=NA,
    .init=function() {
      print("init")
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
      if (length(self$options$randomTerms)>0) {
        nr<-1
        for(term in self$options$randomTerms) {
          labels<-gsub(" ","",strsplit(term,"|",fixed = T)[[1]],fixed=T)
          dummies<-.getDummiesNames(labels[1],data)
          for (dummy in dummies) {
            aTable$addRow(rowKey=nr,list(groups=labels[2],name=dummy))
            nr<-nr+1
          }
        }
        aTable$addRow(rowKey=nr,list(groups="Residuals"))
      }
      
      ## anova Table 
      if (length(modelTerms)>0) {
          aTable<- self$results$anova
          modelTerms<-self$options$modelTerms
          if (length(modelTerms) > 0) {
              for (i in seq_along(modelTerms)) 
                  aTable$addRow(rowKey=i, list(name=" "))
          }
      }
      
      ## fixed effects parameters
      
      aTable<-self$results$fixed
      formula<-as.formula(private$.constructFormula(self$options$dep, modelTerms))
      terms<-colnames(model.matrix(formula,data))  
      labels<-.getFormulaContrastsLabels(self$options$contrasts,formula,data) 
      ciWidth<-self$options$paramCIWidth
      aTable$getColumn('cilow')$setSuperTitle(jmvcore::format('{}% Confidence Interval', ciWidth))
      aTable$getColumn('cihig')$setSuperTitle(jmvcore::format('{}% Confidence Interval', ciWidth))
      
      for(i in seq_along(terms)) 
          aTable$addRow(rowKey=i,list(source=.nicifyTerms(terms[i]),label=.nicifyTerms(labels[i])))

      ######### simple effects tables ##############
      
      variable<-self$options$simpleVariable
      moderator<-self$options$simpleModerator
      threeway<-self$options$simple3way
      if (!is.null(variable) & !is.null(moderator)) {
        # N dummies in X-axis
        xlevels<-length(levels(data[[variable]]))
        xlevels<-ifelse(xlevels>1,(xlevels-1),1)
        # N levels of moderator
        modlevels<-length(levels(data[[moderator]]))
        modlevels<-ifelse(modlevels>1,modlevels,3)
        nrows<-modlevels*xlevels  
        # create the tables with right rows
        simpleEffectsTables<-self$results$simpleEffects
        simpleEffectsAnovas<-self$results$simpleEffectsAnovas
        if (!is.null(threeway)) {
          mod2levels<-length(levels(data[[threeway]]))
          mod2levels<-ifelse(mod2levels>1,mod2levels,3)
        } else
          mod2levels<-1
        
        for (j in 1:mod2levels) {
          title<-paste("Simple effects of",variable)
          key<-paste(variable,j,sep="")
          
          ## init simple ANOVA tables
          ftable<-simpleEffectsAnovas$addItem(key=key)
          ftable$setTitle(title)
          
          for (i in 1:modlevels) 
            ftable$addRow(rowKey=i,list(variable=" "))        
          
          ## init simple parameters tables
          ptable<-simpleEffectsTables$addItem(key=key)
          ptable$setTitle(title)
          for (i in 1:nrows) 
            ptable$addRow(rowKey=i,list(variable=" "))        
          
        }  
      }  # end of simple effects tables
      
        
        # other inits
        private$.initPlots(data)
        tables   <- self$results$postHoc
        tables<-rf.initPostHoc(data,self$options, tables,modelType="linear")     
        private$.initMeanTables(data)
    },
    .run=function() {
      print("run")
      # collect some option
      dep <- self$options$dep
      factors <- self$options$factors
      covs <- self$options$covs
      clusters<-self$options$cluster
      reml<-self$options$reml
      
      if (length(self$options$modelTerms)==0)
        return()
      modelTerms<-private$.modelTerms()
      
      ### collect the tables #######
      infoTable<-self$results$info
      fixedTable <- self$results$fixed
      randomTable <- self$results$random
      randomCovTable<-self$results$randomCov
      aTable<-self$results$anova
      
      ####  prepare the model formula              ####      
            
      modelFormula<-private$.checkModel()
      if (modelFormula!=FALSE)    {
        
        data<-private$.cleandata() 
        data<-mf.checkData(self$options,data,"mixed")
        
        if (!is.data.frame(data))
          reject(data)
        
        for (scaling in self$options$scaling) {
          cluster<-clusters[[1]]
          data[[scaling$var]]<-.scaleContinuous(data[[scaling$var]],scaling$type,data[[cluster]])  
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
        info.call<-as.character(model@call)[[2]]
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
        for (i in 1:dim(vcv)[1]) {
          if (i<=randomTable$rowCount)
             randomTable$setRow(rowNo=i, list(groups=vcv$grp[i],name=vcv$var1[i],std=vcv$sdcor[i],var=vcv$sdcor[i]^2))
          else
            randomTable$addRow(rowKey=i, list(groups=vcv$grp[i],name=vcv$var1[i],std=vcv$sdcor[i],var=vcv$sdcor[i]^2))
        }
        info<-    paste("Numer of Obs:", ss$devcomp$dims["n"],", groups:",names(ss$ngrps),",",ss$ngrps,collapse = "")
        randomTable$setNote('info', info)
        

        ### Covariance among random effects ###
        vcv<-vc[!is.na(vc[,3]),]
        if (dim(vcv)[1]>0) {
          for (i in 1:dim(vcv)[1]) {
            randomCovTable$addRow(rowKey=vcv$grp[i], list(groups=vcv$grp[i],name1=vcv$var1[i],name2=vcv$var2[i],cov=vcv$sdcor[i]))
          }
          randomCovTable$setVisible(TRUE)
        }
        ### ### ### ### ###
        
        # anova table ##
        if (length(modelTerms)==0) {
          aTable$setNote("warning","F-Tests cannot be computed without fixed effects")
        } else {
          suppressWarnings({
                          anova <- try(mf.lmeranova(model), silent=TRUE) # end suppressWarnings
           })
        if (jmvcore::isError(anova)) 
          jmvcore::reject(jmvcore::extractErrorMessage(anova), code='error')
        
        labels<-rownames(anova)
        for (i in seq_len(dim(anova)[1])) {
            tableRow<-anova[i,]  
            aTable$setRow(rowNo=i,tableRow)
            aTable$setRow(rowNo=i,list(name=labels[i]))
         }
        aTable$setNote("df",paste(attr(anova,"method"),"method for degrees of freedom"))
        messages<-mf.getModelMessages(model)
        for (i in seq_along(messages)) {
                 aTable$setNote(names(messages)[i],messages[[i]])
                 infoTable$setNote(names(messages)[i],messages[[i]])
        }
        if (length(messages)>0) {
          aTable$setNote("lmer.nogood",WARNS["lmer.nogood"])
          infoTable$setNote("lmer.nogood",WARNS["lmer.nogood"])
        }
        }
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
        for (i in 1:nrow(parameters)) {
                tableRow=parameters[i,]
                fixedTable$setRow(rowNo=i,tableRow)
          }

        
       if (mf.aliased(model)) {
          fixedTable$setNote("aliased",WARNS["ano.aliased"])
          infoTable$setNote("aliased",WARNS["ano.aliased"])
        }
        
        
      ####### filling in contrast table definition ##############
        contrastsTables <- self$results$contrasts
        for (contrast in self$options$contrasts) {
          table <- contrastsTables$addItem(contrast)
          var <- data[[contrast$var]]
          if (contrast$type=="default")
                 contrast$type="deviation"
          
          levels <- base::levels(var)
          labels <- .contrastLabels(levels, contrast$type)
          dummies<-paste(contrast$var,1:length(labels),sep="")
          groups<-paste(1:length(levels),levels,sep = "=", collapse = ", ")
          
          for (i in 1:length(labels)) 
            table$addRow(rowKey=labels[[i]], values=list(contrast=labels[[i]],term=dummies[[i]],groups=groups))
        } 
        
        private$.preparePlots(private$.model)
        private$.populateSimple(private$.model)
        private$.populatePostHoc(private$.model)
        private$.populateDescriptives(private$.model)
        
        
  }

    },
  .buildreffects=function(terms,correl=TRUE) {
    terms<-lapply(terms,function(x) unlist(sapply(x,function(z) paste0("`",z,"`"))))
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
    
    form<-gsub('`Intercept`',1,form,fixed = T)
    form<-gsub('0+1',1,form,fixed = T)
    form=paste(form,collapse = "+")
    form
  },
  
    .cleandata=function() {
      dep <- self$options$dep
      factors <- self$options$factors
      covs <- self$options$covs
      clusters<-self$options$cluster

      data <- self$data
      if ( ! is.null(dep))
        data[[dep]] <- jmvcore::toNumeric(data[[dep]])
      
      for (factor in factors) {
        data[[factor]] <- as.factor(data[[factor]])
        levels <- base::levels(data[[factor]])
        stats::contrasts(data[[factor]])<-.createContrasts(levels, "deviation")
      }
      for (cluster in clusters) {
        data[[cluster]] <- as.factor(data[[cluster]])
      }
      
      for (contrast in self$options$contrasts) {
        levels <- base::levels(data[[contrast$var]])
        stats::contrasts(data[[contrast$var]]) <- .createContrasts(levels, contrast$type)
      }
      
      for (covariate in covs)
        data[[covariate]] <- jmvcore::toNumeric(data[[covariate]])
      
      
      vars<-c(factors,covs,clusters,dep)
      data<-data[vars]
      data <- na.omit(data)
      data
    },
    .estimate = function(form, data,REML=TRUE) {
      ## there is a bug in LmerTest and it does not work
      ## when called within an restricted environment such as a function.
      ## the do.call is a workaround.
      print("estimating the model")
      lm = do.call(lmerTest::lmer, list(formula=form, data=data,REML=REML))
      return(lm)
    },
    .constructFormula=function(dep,terms) {
#      terms<-c("1",terms)
      form<-jmvcore::constructFormula(dep,terms)
#      gsub('`',"",form)
      form
    },
    .checkModel=function() {
      print("checking the model")
      modelTerms<-private$.modelTerms()
      if (length(self$options$randomTerms)>0)  {
        rands<-self$options$randomTerms
        rands<-private$.buildreffects(rands,self$options$correlatedEffects)
      } else return(FALSE)
      
      intercept<-as.numeric(self$options$fixedIntercept)
      
      if (!is.null(self$options$dep)) {
        dep<-paste(self$options$dep," ~ ", intercept,"+")
      } else return(FALSE)
      
      fixs<-""
      if (length(modelTerms)>0)  {
        for (i in 1:length(modelTerms)) {
          if (length(modelTerms[[i]])>1)  modelTerms[i]<-paste(modelTerms[[i]],collapse = ":")
        }
        fixs<-paste(" + ",paste(modelTerms,collapse = " + " ))
      }
      paste(dep,rands,fixs,sep = " ")
    },

.initMeanTables=function(data) {
  
  
  #### expected means ####
  if (self$options$eDesc) {
    emeansTables <- self$results$emeansTables
    factorsAvailable <- self$options$factors
    modelTerms<-self$options$modelTerms
    if (length(factorsAvailable) == 0)
      return()
    for (term in modelTerms)
      if (all(term %in% factorsAvailable)) {
        aTable<-emeansTables$addItem(key=.nicifyTerms(jmvcore::composeTerm(term)))
        aTable$getColumn('upper')$setSuperTitle(jmvcore::format('{}% Confidence Interval', 95))
        aTable$getColumn('lower')$setSuperTitle(jmvcore::format('{}% Confidence Interval', 95))

        ldata <- data[,term]
        ll <- sapply(term, function(a) base::levels(data[[a]]), simplify=F)
        ll$stringsAsFactors <- FALSE
        grid <- do.call(base::expand.grid, ll)
        grid <- as.data.frame(grid,stringsAsFactors=F)
        for (i in seq_len(ncol(grid))) {
          colName <- colnames(grid)[[i]]
          aTable$addColumn(name=colName, title=term[i], index=i)
        }
        for (rowNo in seq_len(nrow(grid))) {
          row <- as.data.frame(grid[rowNo,],stringsAsFactors=F)
          colnames(row)<-term
          tableRow<-row
          aTable$addRow(rowKey=row, values=tableRow)
        }
      }
  } # end of  means
  
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
      none <- mf.posthoc(model,ph,"none")
      bonferroni <- mf.posthoc(model,ph,"bonferroni")
      holm <-mf.posthoc(model,ph,"holm")
      tukey <-mf.posthoc(model,ph,"tukey")
      
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
         sepPlotsVar <- data[[sepPlotsName]]
         if(is.factor(sepPlotsVar))
            sepPlotsLevels <- levels(sepPlotsVar)
         else 
            sepPlotsLevels <- c("-1 SD","Mean","+1 SD")   
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
  errorBarType="none"
  optionRaw<-self$options$plotRaw
  optionRange<-self$options$plotDvScale
  referToData<-(optionRaw || optionRange)
  
  
  if (referToData)
    rawData=lp.rawData(model,depName,groupName,linesName)
  else 
    rawData<-NULL
  
  predData<-lp.preparePlotData(model,groupName,linesName,plotsName,errorBarType)
  yAxisRange <- lp.range(model,depName,predData,rawData)
  
  if (!optionRaw)
    rawData<-NULL
  
  
  if (is.null(plotsName)) {
    image <- self$results$get('descPlot')
    image$setState(list(data=predData, raw=rawData, range=yAxisRange))
  } else {
    images <- self$results$descPlots
    i<-1
    levels<-levels(predData$plots)
    
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
  errorType <- "none"
  ciWidth   <- 0
  
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
  terms<-self$options$modelTerms
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
  data<-mf.getModelData(model)
  simpleEffectsTables<-self$results$simpleEffects
  simpleEffectsAnovas<-self$results$simpleEffectsAnovas
  
  .fillTheFTable<-function(results,aTable) {
    ftests<-results[[2]]
    ### ftests  
    if (any(ftests==FALSE))
      aTable$setNote("dffail",WARNS["lmer.norelm"])
    else
      for (i in seq_len(dim(ftests)[1])) {
      tableRow<-ftests[i,]
      aTable$setRow(rowNo=i,tableRow)
    }
  } #### end of .fillTheFTable
  
  .fillThePTable<-function(results,aTable) {
    params<-results[[1]]
    what<-params$level
    for (i in seq_len(dim(params)[1])) {
      tableRow<-params[i,]
        
      aTable$setRow(rowNo=i,tableRow)
      if (what!=tableRow$level)  
        aTable$addFormat(col=1, rowNo=i,format=Cell.BEGIN_GROUP)
      what<-tableRow$level
    }
  }  ##### end of .fillThePTable
  
  if (is.null(variable) | is.null(moderator)) 
    return()
  
  if (is.null(threeway)) {
    
    results<-lf.simpleEffects(model,variable,moderator)
    
    ### ftests
    key=paste(variable,1,sep="")
    ftable<-simpleEffectsAnovas$get(key=key)
    .fillTheFTable(results,ftable)      
    ### parameters
    ptable<-simpleEffectsTables$get(key=key)
    .fillThePTable(results,ptable)
    #### add some warning ####
    term<-.interaction.term(private$.model,c(variable,moderator))
    if (!is.null(term)) {
      if (.is.scaleDependent(private$.model,term))
        ptable$setNote("inter",WARNS["se.interactions"])
      else if (.term.develop(term)<length(private$.modelTerms()))
        ptable$setNote("covs",WARNS["se.covariates"])
    } else 
      ptable$setNote("noint",WARNS["se.noint"])
    
    ### end of warnings ###
  } else {
    data$mod2<-data[,threeway]
    if (is.factor(data$mod2)) {
      levs<-levels(data$mod2)
    } else 
      levs<-c(mean(data$mod2)+sd(data$mod2),mean(data$mod2),mean(data$mod2)-sd(data$mod2))
    for(i in seq_along(levs)) {
      newdata<-data
      if (is.factor(data$mod2))
        contrasts(newdata[,threeway])<-contr.treatment(length(levs),base=i)
      else
        newdata[,threeway]<-newdata[,threeway]-levs[i]
      
      ## make nice labels and titles
      lev<-ifelse(is.numeric(levs[i]),round(levs[i],digits=2),levs[i])
      title<-paste("Simple effects of ",variable," computed for",threeway,"at",lev)
      # re-estimate the model
      form<-formula(model)
      FUN<-mf.estimate(model)
      model0<-FUN(form,newdata)
      #### populate the R table       
      results<-lf.simpleEffects(model0,variable,moderator)
      
      ### populate the Jamovi table
      key=paste(variable,i,sep="")
      ### F table
      ftable<-simpleEffectsAnovas$get(key=key)
      ftable$setTitle(title)
      .fillTheFTable(results,ftable)      
      ### parameters
      ptable<-simpleEffectsTables$get(key=key)
      ptable$setTitle(title)
      .fillThePTable(results,ptable)
    } 
  } # end of if (is.null(threeway)) 
  
},


  .modelTerms=function() {
  modelTerms <- self$options$modelTerms
  if (class(modelTerms)!="list")
       modelTerms<-private$.ff()
  for (i in 1:length(modelTerms)) {
    modelTerms[[i]]<-unlist(sapply(modelTerms[[i]],function(x) paste0("`",x,"`")))
  }
  # If we are in interactive mode the model should be well specified, otherwise (if R mode)
  # no modelTerms means full model
  modelTerms
},
    .ff=function() {
  factors <- c(self$options$factors,self$options$covs)
  if (length(factors) > 1) {
    formula <- as.formula(paste('~', paste(paste0('`', factors, '`'), collapse='*')))
    terms   <- attr(stats::terms(formula), 'term.labels')
    modelTerms <- sapply(terms, function(x) as.list(strsplit(x, ':')), USE.NAMES=FALSE)
  } else {
    modelTerms <- as.list(factors)
  }
  
  for (i in seq_along(modelTerms)) {
    term <- modelTerms[[i]]
    quoted <- grepl('^`.*`$', term)
    term[quoted] <- substring(term[quoted], 2, nchar(term[quoted])-1)
    modelTerms[[i]] <- term
  }
  
  modelTerms
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


