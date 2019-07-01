gamljGzlmClass <- R6::R6Class(
  "gamljGzlmClass",
  inherit = gamljGzlmBase,
  private=list(
    .model=NA,
    .names64=NA,
    .cov_condition=conditioning$new(),
    .postHocRows=NA,
    .init=function() {
      ginfo("init")
      private$.names64<-names64$new()
      n64<-private$.names64
      factors<-self$options$factors
      covs<-self$options$covs
      modelTerms<-self$options$modelTerms
      modelType<-self$options$modelSelection
      afamily<-mf.give_family(modelType)
      dep<-self$options$dep
      fixedIntercept<-self$options$fixedIntercept
      
      if (!is.something(afamily))
        return()
      

      if (is.null(dep)) 
        return()

      aOne<-which(unlist(modelTerms)=="1")
      if (is.something(aOne)) {
        modelTerms[[aOne]]<-NULL
        fixedIntercept=TRUE
      }
      if (length(modelTerms) == 0 && fixedIntercept==FALSE) {
        if (is.null(factors) && is.null(covs))
          infoTable$addRow(rowKey="gs4",list(info="Optional",value="Select factors and covariates"))
        else
          jmvcore::reject("Please specify the model with modelTerms option")            
        getout<-TRUE
      }
      
      data<-private$.cleandata()
      if (!is.data.frame(data))
         jmvcore::reject(data)

      modelFormula<-lf.constructFormula(dep,modelTerms,self$options$fixedIntercept)
      
      infoTable<-self$results$info
      info<-MINFO[[modelType]]
      infoTable$addRow(rowKey="mod",list(info="Model Type",value=info$name[[1]],comm=info$name[[2]]))
      infoTable$addRow(rowKey="call",list(info="Call",comm=n64$translate(modelFormula),value=info$call))
      infoTable$addRow(rowKey="link",list(info="Link function",value=info$link[[1]],comm=info$link[[2]]))
      ep<-mi.explainPrediction(modelType,data,dep)
      if (!is.null(ep))
              infoTable$addRow(rowKey="dir",list(info="Direction",value=ep[1],comm=ep[2]))
      infoTable$addRow(rowKey="family",list(info="Distribution",value=info$distribution[[1]],comm=info$distribution[[2]]))
      infoTable$addRow(rowKey="r2",list(info="R-squared",comm="Proportion of reduction of error"))
      infoTable$addRow(rowKey="aic",list(info="AIC",comm="Less is better"))
      infoTable$addRow(rowKey="dev",list(info="Deviance",comm="Less is better"))
      infoTable$addRow(rowKey="resdf",list(info="Residual DF",comm=""))
      
      if (modelType=="nb" || modelType=="poiover" || modelType=="poisson")
        infoTable$addRow(rowKey="devdf",list(info="Value/DF",comm="Close to 1 is better"))
      
      infoTable$addRow(rowKey="conv",list(info="Converged",comm="Whether the estimation found a solution"))
      if ("note" %in% names(info))
        infoTable$addRow(rowKey="note",list(info="Note",value=info$note[[1]],comm=info$note[[2]]))
      
      ### initialize conditioning of covariates
      if (is.something(self$options$covs)) {
      span<-ifelse(self$options$simpleScale=="mean_sd",self$options$cvalue,self$options$percvalue)
      private$.cov_condition<-conditioning$new(self$options$covs,self$options$simpleScale,span)
      }
      #####################

      ## anova Table 
      if (length(modelTerms)>0) {
          aTable<- self$results$main$anova
          for (i in seq_along(modelTerms)) {
                  lab<-jmvcore::stringifyTerm(modelTerms[[i]],raise=T)
                  aTable$addRow(rowKey=i, list(name=lab))
          }
      }
      
      ## fixed effects parameters
      
      aTable<-self$results$main$fixed
      dep64<-jmvcore::toB64(dep)
      modelTerms64<-lapply(modelTerms,jmvcore::toB64)
      formula64<-as.formula(lf.constructFormula(dep64,modelTerms64,self$options$fixedIntercept))
      mynames64<-colnames(model.matrix(formula64,data))
      terms<-n64$nicenames(mynames64)  
      labels<-n64$nicelabels(mynames64)
      ciWidth<-self$options$paramCIWidth
      aTable$getColumn('cilow')$setSuperTitle(jmvcore::format('{}% Confidence Interval', ciWidth))
      aTable$getColumn('cihig')$setSuperTitle(jmvcore::format('{}% Confidence Interval', ciWidth))
      aTable$getColumn('ecilow')$setSuperTitle(jmvcore::format('{}% Exp(B) Confidence Interval', ciWidth))
      aTable$getColumn('ecihig')$setSuperTitle(jmvcore::format('{}% Exp(B) Confidence Interval', ciWidth))
      
      if (modelType=="linear") {
        aTable$getColumn("expb")$setVisible(FALSE)
        aTable$getColumn("ecilow")$setVisible(FALSE)
        aTable$getColumn("ecihig")$setVisible(FALSE)
        }
      if (!is.something(self$options$factors))
        aTable$getColumn('label')$setVisible(FALSE)
      
      if (modelType=="multinomial") {
        ylabs<-lf.contrastLabels(levels(data[[jmvcore::toB64(dep)]]),"simple")
        for (j in seq_along(ylabs)) 
          for(i in seq_along(terms)) 
            aTable$addRow(rowKey=paste0(i,j),list(dep=ylabs[[j]],source=jmvcore::stringifyTerm(terms[[i]],raise=T),label=lf.nicifyLabels(labels[i])))
      } else
           for(i in seq_along(terms)) {
              aTable$addRow(rowKey=i,list(source=jmvcore::stringifyTerm(terms[[i]],raise=T),label=lf.nicifyLabels(labels[i])))
           }

      ## relative risks
      if (self$options$modelSelection=="logistic" && "RR" %in% self$options$effectSize) {
        
           aTable<-self$results$main$relativerisk
           aTable$getColumn('cilow')$setSuperTitle(jmvcore::format('{}% Confidence Interval', ciWidth))
           aTable$getColumn('cihig')$setSuperTitle(jmvcore::format('{}% Confidence Interval', ciWidth))
           
           if (self$options$fixedIntercept==TRUE & length(terms)>1 ) {
               terms<-terms[2:length(terms)]
               labels<-labels[2:length(labels)]        
           }
           for(i in seq_along(terms)) {
               aTable$addRow(rowKey=i,list(source=jmvcore::stringifyTerm(terms[[i]],raise=T),label=lf.nicifyLabels(labels[i])))
           }
      }
      
      # other inits

        gplots.initPlots(self,data,private$.cov_condition)
        gposthoc.init(data,self$options, self$results$postHocs)     
        gmeans.init(data,self$options,self$results$emeansTables,private$.cov_condition)
        gsimple.init(data,self$options,self$results$simpleEffects,n64,private$.cov_condition)
        mi.initContrastCode(data,self$options,self$results,n64)
    },
    .run=function() {
      n64<-private$.names64
      ginfo("run")
      # collect some option
      dep <- self$options$dep
      factors <- self$options$factors
      covs <- self$options$covs
      modelType<-self$options$modelSelection
      if (is.null(dep)) 
        return()
      
      if (self$options$simpleScale=="mean_sd" && self$options$cvalue==0)
          return()
      if (self$options$simpleScale=="percent" && self$options$percvalue==0)
         return()
      ###############      
      
      modelTerms<-lapply(self$options$modelTerms,jmvcore::toB64)
      modelFormula<-lf.constructFormula(jmvcore::toB64(dep),modelTerms,self$options$fixedIntercept)
      
      if (modelFormula==FALSE)  
          return()
    
      
      ### collect the tables #######
      infoTable<-self$results$info
      estimatesTable <- self$results$main$fixed
      anovaTable<-self$results$main$anova

      ##### clean the data ####
      data<-private$.cleandata()
      data<-mf.checkData(self$options,data,modelType)
      if (!is.data.frame(data))
        jmvcore::reject(data)
      for (scaling in self$options$scaling) {
        data[[jmvcore::toB64(scaling$var)]]<-lf.scaleContinuous(data[[jmvcore::toB64(scaling$var)]],scaling$type)  
      }
      
      if (!is.null(covs)) {
        names(data)<-jmvcore::fromB64(names(data))
        private$.cov_condition$storeValues(data)
        names(data)<-jmvcore::toB64(names(data))
        private$.cov_condition$labels_type=self$options$simpleScaleLabels
      }
      
      
               
               ##### model ####
               ## for some reason the (g)lm model is very heavy to save in table$state
               ## so we estimate it every time
           
               model_test <- try({
                           model<-private$.estimate(modelFormula, data=data)
                           wars<-warnings()
                       })
               if (jmvcore::isError(model_test)) {
                        msg<-jmvcore::extractErrorMessage(model_test)
                        msg<-n64$translate(msg)
                        jmvcore::reject(msg, code='error')
               }
      private$.model <- model
      
      ### if it worked before, we skip building the tables, 
      ### otherwise we store a flag  in parameters table state so next time we know it worked

      if (is.null(estimatesTable$state)) {
               ginfo("anova and parameters have been estimated")
               test_summary<-try(model_summary<-summary(model))
               if (jmvcore::isError(test_summary)) {
                   msg <- jmvcore::extractErrorMessage(test_summary)
                   msg<-n64$translate(msg)
                   jmvcore::reject(msg, code='error')
               }
            
      ### coefficients summary results ####
      
               test_parameters<-try(parameters<-mf.summary(model))
               if (jmvcore::isError(test_parameters)) {
                    msg <- jmvcore::extractErrorMessage(test_parameters)
                    msg<-n64$translate(msg)
                    jmvcore::reject(msg, code='error')
               }
      
                if (!is.null(attr(parameters,"warning"))) 
                    estimatesTable$setNote(attr(parameters,"warning"),WARNS[as.character(attr(parameters,"warning"))])
  
               
               ### anova results ####
               anova_res<-NULL
               if (length(modelTerms)==0) {
                   anovaTable$setNote("warning","Tests cannot be computed")
               } else {
                   suppressWarnings({
                   anova_test <- try(anova_res<-mf.anova(model)) # end suppressWarnings
                 })
                   if (jmvcore::isError(anova_test)) 
                      jmvcore::reject(jmvcore::extractErrorMessage(anova_test), code='error')
               }

               
             
               ### fill info table #########   
               modelType<-self$options$modelSelection
               
               infoTable$setRow(rowKey="r2",list(value=mi.rsquared(model)))
               infoTable$setRow(rowKey="aic",list(value=mi.getAIC(model)))
               infoTable$setRow(rowKey="dev",list(value=model$deviance))
               infoTable$setRow(rowKey="resdf",list(value=mi.getResDf(model)))
               if (modelType=="nb" || modelType=="poiover" || modelType=="poisson")
                 infoTable$setRow(rowKey="devdf",list(value=mi.getValueDf(model)))
               if (modelType=="poiover") {
                 infoTable$setRow(rowKey="r2",list(comm="Not available for quasi-poisson"))
                 infoTable$setRow(rowKey="aic",list(comm="Not available for quasi-poisson"))
               }
               infoTable$setRow(rowKey="conv",mi.converged(model))
               
               
               ### end of info table ###
        
               # anova table ##
                ### we still need to check for modelTerms, because it may be a intercept only model, where no F is computed
                 if (length(modelTerms)>0) {
                       rawlabels<-rownames(anova_res)
                       labels<-n64$nicenames(rawlabels)
                       trows<-dim(anova_res)[1]
                       for (i in seq_len(trows)) {
                              tableRow<-anova_res[i,]  
                              anovaTable$setRow(rowNo=i,tableRow)
                       }

                      messages<-mf.getModelMessages(model)
                      for (i in seq_along(messages)) {
                              anovaTable$setNote(names(messages)[i],messages[[i]])
                              infoTable$setNote(names(messages)[i],messages[[i]])
                      }
                      if (length(messages)>0) {
                           infoTable$setNote("lmer.nogood",WARNS["lmer.nogood"])
                      }

                 }
    

        ### parameter table ####
        #### confidence intervals ######
        ciWidth<-self$options$paramCIWidth/100

        if (self$options$showParamsCI) {
             citry<-try({
                 ci<-mf.confint(model,level=ciWidth)
                 if (is.null(dim(ci)))
                   ci<-t(as.matrix(ci))
                 colnames(ci)<-c("cilow","cihig")
                 parameters<-cbind(parameters,ci) 
                 })
              if (jmvcore::isError(citry)) {
                  message <- jmvcore::extractErrorMessage(citry)
                  estimatesTable$setNote("cicrash","CI cannot be computed")
              }
        }
        if (self$options$showExpbCI) {
                 citry<-try({
                   ci<-mf.confint(model,level=ciWidth)
                   if (is.null(dim(ci)))
                     ci<-t(as.matrix(ci))
                   ci[,1]<-exp(ci[,1])
                   ci[,2]<-exp(ci[,2])
                   colnames(ci)<-c("ecilow","ecihig")
                   parameters<-cbind(parameters,ci) 
                 })
                 if (jmvcore::isError(citry)) {
                   message <- jmvcore::extractErrorMessage(citry)
                   estimatesTable$setNote("cicrash",paste(message,". CI cannot be computed"))
                 }
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
        estimatesTable$setState(TRUE)
        
  
        # end of check state
        } else
          ginfo("anova and parameters have been recycled")

        ############   Relative Risks      ##################
        if (self$options$modelSelection=="logistic" && "RR" %in% self$options$effectSize) {
          data$id_id_id<-seq_len(dim(data)[1])
          levs<-levels(data[[jmvcore::toB64(dep)]])
          data[,jmvcore::toB64(dep)]<-as.numeric(data[[jmvcore::toB64(dep)]]==levs[2])
          geemodel<-geepack::geeglm(as.formula(modelFormula), family = poisson(link = "log"), id = id_id_id, corstr = "exchangeable", data = data)
          summ<-summary(geemodel)
          estimate<-summ$coefficients
          names(estimate)[1]<-"estimate"
          if ("(Intercept)" %in% rownames(estimate) ) {
           last<-length(estimate[,1])
            estimate<-estimate[2:last,] 
          }
          pp<-qnorm(1-ciWidth / 2)
          estimate$cihig<-estimate$estimate+ pp * estimate$Std.err
          estimate$cilow<-estimate$estimate- pp * estimate$Std.err
          estimate<-as.matrix(estimate[,c("estimate","cilow","cihig")])
          estimate[]<-vapply(estimate, exp,numeric(1))
          table<-self$results$main$relativerisk
          for (i in 1:nrow(estimate))
            table$setRow(rowKey=i,estimate[i,])
        }
      
      
        ####################################
        private$.preparePlots(private$.model)
        gsimple.populate(model,self$options,self$results$simpleEffects,private$.cov_condition)
        gposthoc.populate(model,self$options,self$results$postHocs)
        gmeans.populate(model,self$options,self$results$emeansTables,private$.cov_condition)
        

    },
  .cleandata=function() {
      n64<-private$.names64
      
      dep <- self$options$dep
      factors <- self$options$factors
      covs <- self$options$covs
      
      dataRaw <- self$data
      data <- list()
      for (factor in factors) {
        ### we need this for Rinterface ####
        if (!("factor" %in% class(dataRaw[[factor]]))) {
          ginfo(paste("Warning, variable",factor," has been coerced to factor"))
          dataRaw[[factor]]<-factor(dataRaw[[factor]])
        }
        data[[jmvcore::toB64(factor)]] <- dataRaw[[factor]]
        levels <- base::levels(data[[jmvcore::toB64(factor)]])
        stats::contrasts(data[[jmvcore::toB64(factor)]]) <- lf.createContrasts(levels,"simple")
        n64$addFactor(factor,levels)
        n64$addLabel(factor,lf.contrastLabels(levels, "simple")) 
        attr(data[[jmvcore::toB64(factor)]],"jcontrast")<-"simple"
      }
      
      for (contrast in self$options$contrasts) {
        levels <- base::levels(data[[jmvcore::toB64(contrast$var)]])
        stats::contrasts(data[[jmvcore::toB64(contrast$var)]]) <- lf.createContrasts(levels, contrast$type)
        n64$addLabel(contrast$var,lf.contrastLabels(levels, contrast$type)) 
        attr(data[[jmvcore::toB64(contrast$var)]],"jcontrast")<-contrast$type
      }

      modelType<-self$options$modelSelection

      depOk<-FALSE
      dep64<-jmvcore::toB64(dep)

      if (modelType=="logistic" || modelType=="probit") {
 
                 if (!is.factor(dataRaw[[dep]]))
                     dataRaw[[dep]]<-factor(dataRaw[[dep]])

                 data[[dep64]] <- dataRaw[[dep]] 
                 levs<-base::levels(data[[dep64]])
                 if (length(levs)!=2)
                     return(paste(modelType,"model requires two levels in the dependent variable"))
                 n64$addFactor(dep,levs)
                 depOk<-TRUE

              }
      
      if (modelType=="multinomial") {
 
                if (!is.factor(dataRaw[[dep]]))
                     dataRaw[[dep]]<-factor(dataRaw[[dep]])
        
                data[[dep64]] <- dataRaw[[dep]] 
                levs<-base::levels(data[[dep64]])
                if (length(levs)<3)
                     return("For 2-levels factors please use a logistic model")
                n64$addFactor(dep,levs)
                n64$addLabel(dep,lf.contrastLabels(levels, "simple")) 
                depOk<-TRUE
          }
      if (!depOk) {
             data[[dep64]] <- jmvcore::toNumeric(dataRaw[[dep]])
             n64$addVar(dep)
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
  .estimate=function(form,data) {
    modelType<-self$options$modelSelection
    if (modelType=="multinomial") {
      mod<-nnet::multinom(form,data,model = T)
      mod$call$formula<-as.formula(mod)
      return(mod)
    }
    if (modelType=="nb") {
      mod<-MASS::glm.nb(form,data)
      return(mod)
    }
    
    stats::glm(form,data,family=mf.give_family(modelType))
  },
  
      .modelFormula=function() {

      if (!is.null(self$options$dep))  {
        dep<-jmvcore::toB64(self$options$dep)
      } else return(FALSE)
      private$.fixedFormula()
    },


.preparePlots=function(model) {
  
  depName <- self$options$dep
  groupName <- self$options$plotHAxis
  if (length(depName) == 0 || length(groupName) == 0)
    return()
  
  linesName <- self$options$plotSepLines
  plotsName <- self$options$plotSepPlots
  if (is.null(linesName))
    plotsName<-NULL

  errorBarType<-self$options$plotError
  ciWidth   <- self$options$ciWidth
  optionRaw<-self$options$plotRaw
  optionRange<-self$options$plotDvScale
  referToData<-(optionRaw || optionRange)
  plotScale<-self$options$simpleScale
  offset<-ifelse(self$options$simpleScale=="percent",self$options$percvalue,self$options$cvalue)
  
  
  
  if (referToData)
    rawData=gplots.rawData(model,depName,groupName,linesName,plotsName)
  else 
    rawData<-NULL

  predData<-gplots.preparePlotData(model,
                               groupName,
                               linesName,
                               plotsName,
                               errorBarType,
                               ciWidth,
                               conditioning=private$.cov_condition)
 
  yAxisRange <- gplots.range(model,depName,predData,rawData)

  if (!optionRaw)
    rawData<-NULL
  image<-gplots.images(self=self,data=predData,raw=rawData,range=yAxisRange)
  if (self$options$modelSelection=="multinomial" && !is.null(linesName)) {
      if (is.null(plotsName)) {
            n<-length(levels(factor(predData[["plots2"]])))
            image$setSize(800,(200*n))
          } else {
             glevels<-image$itemKeys
             for (i in seq_along(glevels)) {
               im <- image$get(key=glevels[[i]])
                  n<-length(levels(factor(predData[["plots2"]])))
                  im$setSize(800,(200*n))
             }
          }
  }
  
},

.descPlot=function(image, ggtheme, theme, ...) {
  if (is.null(image$state))
    return(FALSE)

  depName <- self$options$dep
  groupName <- self$options$plotHAxis
  linesName <- self$options$plotSepLines
  plotsName <- self$options$plotSepPlots
  errorType <- self$options$plotError
  ciWidth   <- self$options$ciWidth
  modelType <- self$options$modelSelection
  
  if (errorType=="ci")
    errorType<-paste0(ciWidth,"% ",toupper(errorType))
  
  if (modelType!="multinomial") {
  if ( ! is.null(linesName)) {
    p<-gplots.twoWaysPlot(image,ggtheme,depName,groupName,linesName,errorType)

  } else {
    p<-gplots.oneWayPlot(image,ggtheme,depName,groupName,errorType)

  }       
  } else {
    p<-gplots.linesMultiPlot(image,ggtheme,depName,groupName,linesName,plotsName,errorType)
  }
  return(p)
},

.formula=function() {
  jmvcore:::composeFormula(self$options$dep, self$options$modelTerms)
},
.sourcifyOption = function(option) {
  
  name <- option$name
  value <- option$value
  
  if (!is.something(value))
    return('')
  
  if (option$name %in% c('factors', 'dep', 'covs', 'modelTerms'))
    return('')
  
  if (name == 'scaling') {
    i <- 1
    while (i <= length(value)) {
      item <- value[[i]]
      if (item$type == 'centered')
        value[[i]] <- NULL
      else
        i <- i + 1
    }
    if (length(value) == 0)
      return('')
  }
  if (name == 'contrasts') {
    i <- 1
    while (i <= length(value)) {
      item <- value[[i]]
      if (item$type == 'simple')
        value[[i]] <- NULL
      else
        i <- i + 1
    }
    if (length(value) == 0)
      return('')
  }  else if (name == 'postHoc') {
    if (length(value) == 0)
      return('')
  }
  
  super$.sourcifyOption(option)
}
  )


)


