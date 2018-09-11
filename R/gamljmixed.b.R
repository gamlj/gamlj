#' @import lmerTest
library(lmerTest)

gamljMixedClass <- R6::R6Class(
  "gamljMixedClass",
  inherit=gamljMixedBase,
  private=list(
    .model=NA,
    .names64=NA,
    .cov_condition=conditioning$new(),
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
      
      ### initialize conditioning of covariates
      if (!is.null(self$options$covs)) {
      span<-ifelse(self$options$simpleScale=="mean_sd",self$options$cvalue,self$options$percvalue)
      private$.cov_condition<-conditioning$new(self$options$covs,self$options$simpleScale,span)
      }
      #####################

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
      aTable<-self$results$main$random
      aTable$addRow(rowKey="res",list(groups="Residuals"))

      ## anova Table 
      if (length(modelTerms)>0) {
          aTable<- self$results$main$anova
          for (i in seq_along(modelTerms)) 
                  aTable$addRow(rowKey=i, list(name=" "))
      }
      
      ## fixed effects parameters

      aTable<-self$results$main$fixed
      formula<-as.formula(private$.fixedFormula())
      mynames64<-colnames(model.matrix(formula,data))
      terms<-n64$nicenames(mynames64)  
      labels<-n64$nicelabels(mynames64)
      ciWidth<-self$options$paramCIWidth
      aTable$getColumn('cilow')$setSuperTitle(jmvcore::format('{}% Confidence Interval', ciWidth))
      aTable$getColumn('cihig')$setSuperTitle(jmvcore::format('{}% Confidence Interval', ciWidth))
      for(i in seq_along(terms)) 
          aTable$addRow(rowKey=i,list(source=.nicifyTerms(terms[[i]]),label=.nicifyTerms(labels[i])))

        # other inits
        private$.initPlots(data)
        rf.initPostHoc(data,self$options, self$results$postHocs,modelType="linear")     
        rf.initEMeans(data,self$options,self$results$emeansTables,private$.cov_condition)
        rf.initSimpleEffects(data,self$options,self$results)
        rf.initContrastCode(data,self$options,self$results,n64)
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
      
      if (self$options$simpleScale=="mean_sd" && self$options$cvalue==0)
          return()
      if (self$options$simpleScale=="percent" && self$options$percvalue==0)
         return()
 
      ###############      
      modelTerms<-private$.modelTerms()
      modelFormula<-private$.modelFormula()
      if (modelFormula==FALSE)  
          return()
    
      
      ### collect the tables #######
      infoTable<-self$results$info
      estimatesTable <- self$results$main$fixed
      randomTable <- self$results$main$random
      randomCovTable<-self$results$main$randomCov
      anovaTable<-self$results$main$anova

      ##### clean the data ####
      data<-private$.cleandata()
      data<-mf.checkData(self$options,data,"mixed")
      if (!is.data.frame(data))
        reject(data)
      for (scaling in self$options$scaling) {
        cluster<-jmvcore::toB64(clusters[[1]])
        data[[jmvcore::toB64(scaling$var)]]<-lf.scaleContinuous(data[[jmvcore::toB64(scaling$var)]],scaling$type,data[[cluster]])  
      }
      
      if (!is.null(covs)) {
        names(data)<-jmvcore::fromB64(names(data))
        private$.cov_condition$storeValues(data)
        names(data)<-jmvcore::toB64(names(data))
        private$.cov_condition$labels_type=self$options$simpleScaleLabels
      }
      
      
      ## check if changed: if not, retrieve the results, otherwise estimate them ####
      
             if (!is.null(anovaTable$state)) {
               model<-anovaTable$state[[1]]
               anova_res<-anovaTable$state[[2]]
               private$.model <- model
               model_summary<-estimatesTable$state[[1]]
               parameters<-estimatesTable$state[[2]]
               mark("the model has been recycled")
             }
             else {
               ## because we have no model to recycle, we need to estimate several things:
               ## model: we need the basic model, estimated with private$.estimate() which changes depending on the particular model
               ## model_summary: a summary of a model, full R summary
               ## parameters: a table of coefficients, produced by mf.summary() which understands the class of the model
               ## model_anova: a table of F or X^2, produced by mf.anova(), which undestands the class of the model
               ## These four objects are saved in the tables states: model and model_anova in anovaTable, model_summary and parameters in estimatesTable

               mark("the model has been estimated")
               
               ##### model ####
           
               model_test <- try({
                           model<-private$.estimate(modelFormula, data=data,REML = reml)
                           wars<-warnings()
                       })
               if (jmvcore::isError(model_test)) {
                        msg<-jmvcore::extractErrorMessage(model_test)
                        msg<-n64$translate(msg)
                        jmvcore::reject(msg, code='error')
               }
               ### anova results ####
               anova_res<-NULL
               if (length(modelTerms)==0) {
                 anovaTable$setNote("warning","F-Tests cannot be computed without fixed effects")
               } else {
                 suppressWarnings({
                   anova_test <- try(anova_res<-mf.anova(model), silent=TRUE) # end suppressWarnings
                 })
                 if (jmvcore::isError(anova_test)) 
                      jmvcore::reject(jmvcore::extractErrorMessage(anova_test), code='error')
               }
               anovaTable$setState(list(model,anova_res))

               ### full summary results ####
               
               test_summary<-try(model_summary<-summary(model))
               
               if (jmvcore::isError(test_summary)) {
                   msg <- extractErrorMessage(test_summary)
                   msg<-n64$translate(msg)
                   jmvcore::reject(msg, code='error')
               }

               ### coefficients summary results ####
               
               test_parameters<-try(parameters<-mf.summary(model))
               if (!is.null(attr(parameters,"warning"))) 
                     estimatesTable$setNote(attr(parameters,"warning"),WARNS[as.character(attr(parameters,"warning"))])
               private$.model <- model
               estimatesTable$setState(list(model_summary,parameters))
             } # end of check state 
             
        ### prepare info table #########       
               
               info.call<-n64$translate(as.character(model@call)[[2]])
               info.title<-paste("Linear mixed model fit by",ifelse(reml,"REML","ML"))
               info.aic<-model_summary$AICtab[1]
               info.bic<-model_summary$AICtab[2]
               info.loglik<-model_summary$AICtab[3]
               r2<-try(r.squared(model))
               if (jmvcore::isError(r2)){
                   note<-"R-squared cannot be computed."
                   info.r2m<-NaN        
                   info.r2c<-NaN
                   infoTable$setNote("r2",note)  
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
                vc<-as.data.frame(model_summary$varcor)
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
                info<-paste("Numer of Obs:", model_summary$devcomp$dims["n"],", groups:",n64$nicenames(names(model_summary$ngrps)),",",model_summary$ngrps,collapse = "")
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
                ### we still need to check for modelTerms, because it may be a intercept only model, where no F is computed
                 if (length(modelTerms)==0) {
                     anovaTable$setNote("warning","F-Tests cannot be computed without fixed effects")
                 } else {
                       rawlabels<-rownames(anova_res)
                       labels<-n64$nicenames(rawlabels)
                       for (i in seq_len(dim(anova_res)[1])) {
                              tableRow<-anova_res[i,]  
                              anovaTable$setRow(rowNo=i,tableRow)
                              anovaTable$setRow(rowNo=i,list(name=.nicifyTerms(labels[i])))
                       }
                      messages<-mf.getModelMessages(model)
                      for (i in seq_along(messages)) {
                              anovaTable$setNote(names(messages)[i],messages[[i]])
                              infoTable$setNote(names(messages)[i],messages[[i]])
                      }
                      if (length(messages)>0) {
                           infoTable$setNote("lmer.nogood",WARNS["lmer.nogood"])
                      }
                      if (attr(anova_res,"statistic")=="Chisq") {
                          anovaTable$setNote("lmer.chisq",WARNS["lmer.chisq"])
                          anovaTable$getColumn('test')$setTitle("Chi-squared")
                          anovaTable$getColumn('df1')$setTitle("df")
                          anovaTable$getColumn('df2')$setVisible(FALSE)
                       } else
                              anovaTable$setNote("df",paste(attr(anova_res,"method"),"method for degrees of freedom"))
        
                 }

        #### LRT for random effects ####
        if (self$options$lrtRandomEffects) {
          
          lrtTable<-self$results$main$lrtRandomEffectsTable
          ranova_test<-try(res<-as.data.frame(lmerTest::ranova(model)[-1,]))
          if (jmvcore::isError(ranova_test)) {
              message <- extractErrorMessage(ranova_test)
              lrtTable$setNote("noluck",paste(message,". LRT cannot be computed"))
          } else {
             res$test<-n64$translate(rownames(res))
             res$test<-as.character(res$test)
             for (i in seq_len(nrow(res)))
                  lrtTable$addRow(rowKey=i,res[i,])
          } 
        }
        ### parameter table ####
        
        #### confidence intervals ######
        ciWidth<-self$options$paramCIWidth/100
        citry<-try({
            ci<-mf.confint(model,level=ciWidth)
            colnames(ci)<-c("cilow","cihig")
            parameters<-cbind(parameters,ci) 
          })
          if (jmvcore::isError(citry)) {
            message <- extractErrorMessage(citry)
            estimatesTable$setNote("cicrash",paste(message,". CI cannot be computed"))
          }
        rownames(parameters)<-n64$nicenames(rownames(parameters))
        for (i in 1:nrow(parameters)) {
                tableRow=parameters[i,]
                estimatesTable$setRow(rowNo=i,tableRow)
          }

        
       if (mf.aliased(model)) {
          estimatesTable$setNote("aliased",WARNS["ano.aliased"])
          infoTable$setNote("aliased",WARNS["ano.aliased"])
       }
  
           
        private$.preparePlots(private$.model)
        private$.populateSimple(private$.model)
        private$.populatePostHoc(private$.model)
        private$.populateDescriptives(private$.model)

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
      form<-sapply(names(res), function(x) sapply(res[[x]], function(z) paste("(",paste(z,x,sep = "|"),")")))
      form<-sapply(form, function(x) paste(x,collapse = "+"))
    } 
    form<-gsub(jmvcore::toB64('Intercept'),1,form,fixed = T)
    # fix the formula in case there is no intercept

    form<-lapply(form, function(x) {
              if(!grepl(" 1",x,fixed = T)) 
                 gsub("(","(0+",x,fixed = T)
              else
                x})
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
        ### we need this for Rinterface ####
        if (!("factor" %in% class(dataRaw[[factor]]))) {
          mark(paste("Warning, variable",factor," has been coerced to factor"))
          dataRaw[[factor]]<-factor(dataRaw[[factor]])
        }
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
      
      fixed<-private$.fixedFormula()
      mf<-paste(fixed,rands,sep =  "+")
      mf
    },


.populatePostHoc=function(model) {

    terms <- self$options$postHoc
    dep<-self$options$dep

     if (length(terms) == 0)
         return()
  
  tables <- self$results$postHocs 
  if (tables$isFilled()) {
    mark("post-hoc recycled")
    return()
  }
  mark("post-hoc computed")

  postHocRows <- list()
  
  for (ph in terms) {
    
    table <- tables$get(key=ph)
    table$setState(list(1:10))
    term <- jmvcore::composeTerm(ph)
    termB64 <- jmvcore::composeTerm(jmvcore::toB64(ph))
    suppressWarnings({
      none <- mf.posthoc(model,termB64,"none")
      bonferroni <- mf.posthoc(model,termB64,"bonferroni")
      holm <-mf.posthoc(model,termB64,"holm")
      tukey <-mf.posthoc(model,termB64,"tukey")
      
    }) # suppressWarnings
    if (is.character(none))
      table$setNote("nojoy",WARNS["ph.nojoy"])
    else {        
      table$setState("there is work done")
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
         sepPlotsVar <- data[[jmvcore::toB64(sepPlotsName)]]
         if(is.factor(sepPlotsVar))
            sepPlotsLevels <- levels(sepPlotsVar)
         else {
           sepPlotsLevels<-private$.cov_condition$labels(sepPlotsName)
         }
         mark("levs",sepPlotsLevels)

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
  offset<-ifelse(self$options$simpleScale=="percent",self$options$percvalue,self$options$cvalue)
  
  
  
  
  if (referToData)
    rawData=lp.rawData(model,depName,groupName,linesName)
  else 
    rawData<-NULL
  ### this is specific of mixed model #####
  cluster<-self$options$cluster[[1]]
  preds<-c.real(cluster,groupName,linesName,plotsName)
  preds64<-jmvcore::toB64(preds)
  
  if (self$options$plotRandomEffects) {
    pd<-predict(model)
    data<-model@frame
    randomData<-as.data.frame(cbind(pd,data[,preds64]))
    pnames<-c("cluster","group","lines","plots")
    names(randomData)<-c("y",pnames[1:length(preds)])
  } else
    randomData<-NULL

  predData<-lp.preparePlotData(model,
                               groupName,
                               linesName,
                               plotsName,
                               errorBarType,
                               ciWidth,
                               conditioning=private$.cov_condition)
 
  if (!optionRaw)
    rawData<-NULL
 
  yAxisRange <- lp.range(model,depName,predData,rawData)

  ### this for mixed only ###
  if (!is.null(randomData)) {
             yAxisRange[which.max(yAxisRange)]<-max(max(randomData$y),max(yAxisRange))
             yAxisRange[which.min(yAxisRange)]<-min(min(randomData$y),min(yAxisRange))
    }
  
  
  if (is.null(plotsName)) {
    image <- self$results$get('descPlot')
    image$setState(list(data=predData, raw=rawData, range=yAxisRange, randomData=randomData))
  } else {
    images <- self$results$descPlots
    i<-1
    levels<-levels(factor(predData$plots))
    for (key in images$itemKeys) {
      real<-levels[i]
      i<-i+1
      image <- images$get(key=key)
      sdata<-subset(predData,plots==real)
      sraw<-NULL
      if (!is.null(rawData))
           sraw<-subset(rawData,w==real)
      srand<-NULL
      if (!is.null(randomData))
           srand<-subset(randomData,plots==real)
          
      image$setState(list(data=sdata,raw=sraw, range=yAxisRange,randomData=srand))
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
    p<-lp.twoWaysPlot(image,theme,depName,groupName,linesName,errorType)
  } else {
    p<-lp.oneWayPlot(image,theme,depName,groupName,errorType)
  }       
  p<-p+ggtheme
  print(p)
  TRUE
},
.populateDescriptives=function(model) {

  if (self$options$eDesc) {
       
       terms<-private$.modelTerms()
       if (self$options$eCovs)
           terms<-private$.modelTerms()
       else 
           terms<-jmvcore::fromB64(mf.getModelFactors(model))

       meanTables<-self$results$emeansTables
       
       if (meanTables$isFilled()) {
         mark("Estimated marginal means recycled")
         return()
       }
       mark("Estimated marginal means computed")
       for (term in terms) {
            term64<-jmvcore::toB64(term)
            table<-pred.means(model,term64,cov_conditioning = private$.cov_condition)  
            key<-.nicifyTerms(jmvcore::composeTerm(term))    
            names(table)[1:length(term)]<-term
            aTable<-meanTables$get(key=key)
               
            for (i in seq_len(nrow(table))) {
                     values<-table[i,]
                     aTable$setRow(rowNo=i,values)
            }
            depend<-lf.dependencies(model,term64,"means")
            if (depend!=FALSE) 
            aTable$setNote(depend,WARNS[depend])
         }
       
       meanTables$setState(TRUE)
  } # end of eDesc              
  
},
.populateSimple=function(model) {
  
  variable<-self$options$simpleVariable
  moderator<-self$options$simpleModerator
  threeway<-self$options$simple3way
  simpleScale<-self$options$simpleScale
  offset<-ifelse(self$options$simpleScale=="percent",self$options$percvalue,self$options$cvalue)
  

  interval<-self$options$paramCIWidth

    if (is.null(variable) | is.null(moderator)) 
          return()
  
  ### collect the tables
    
  anovaTable<-self$results$simpleEffects$Anova
  parametersTable<-self$results$simpleEffects$Params

  #### check if estimation is needed
  if (!is.null(anovaTable$state)) {
    mark("simple effects have been recycled")
    anovaTableData<-anovaTable$state
    parametersTableData<-parametersTable$state
  } else {
         simple_test <- try({
                            tables<-pred.simpleEstimates(model,
                                 jmvcore::toB64(variable),jmvcore::toB64(moderator),jmvcore::toB64(threeway),
                                 cov_conditioning=private$.cov_condition,
                                 interval=interval) 
                         })
          if (jmvcore::isError(simple_test)) {
              anovaTable$setNote("se.noluck",WARNS[["se.noluck"]])
              return()
          }
         
          anovaTableData<-tables[[2]]
          anovaTable$setState(anovaTableData)
          parametersTableData<-tables[[1]]  
          parametersTable$setState(parametersTableData)
  }
  ### fill the Anova Table ###
  for(r in seq_len(nrow(anovaTableData))) {
    anovaTable$setRow(rowNo=r,anovaTableData[r,])
  }
  for(r in seq_len(nrow(parametersTableData))) {
    parametersTable$setRow(rowNo=r,parametersTableData[r,])
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
  sep<-ifelse(fixs!="","+"," ")
  lformula<-paste(paste(dep64,intercept,sep="~"),fixs,sep = sep)
  
  return(lformula)
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


