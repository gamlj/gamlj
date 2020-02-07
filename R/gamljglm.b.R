gamljGLMClass <- R6::R6Class(
  "gamljGLMClass",
  inherit = gamljGLMBase,
  private=list(
    .model=NA,
    .names64=NA,
    .cov_condition=conditioning$new(),
    .postHocRows=NA,
    .init=function() {
      ginfo("init")
      private$.names64<-names64$new()
      n64<-private$.names64
      dep<-self$options$dep
      modelTerms<-self$options$modelTerms
      fixedIntercept<-self$options$fixedIntercept
      factors<-self$options$factors
      covs<-self$options$covs
      ### here we initialize the info table ####
      getout<-FALSE
      
      infoTable<-self$results$info

      if (is.null(self$options$dep)) {
        infoTable$addRow(rowKey="gs1",list(info="Get started",value="Select the dependent variable"))
        getout<-TRUE
      }
      if (getout)
        return()
      
      # this allows intercept only model to be passed by syntax interphase
      aOne<-which(unlist(modelTerms)=="1")
      if (is.something(aOne)) {
        modelTerms[[aOne]]<-NULL
        fixedIntercept=TRUE
      }

      
      data<-private$.cleandata()
      
      
      modelFormula<-lf.constructFormula(dep,modelTerms,fixedIntercept)
      
      infoTable$addRow(rowKey="est",list(info="Estimate",value="Linear model fit by OLS"))
      infoTable$addRow(rowKey="call",list(info="Call",value=n64$translate(modelFormula)))
      infoTable$addRow(rowKey="r2m",list(info="R-squared"))
      infoTable$addRow(rowKey="r2c",list(info="Adj. R-squared"))
      ### initialize conditioning of covariates
      if (is.something(covs)) {
      span<-ifelse(self$options$simpleScale=="mean_sd",self$options$cvalue,self$options$percvalue)
      private$.cov_condition<-conditioning$new(covs,self$options$simpleScale,span)
      }
      #####################

      ## anova Table 
      aTable<- self$results$main$anova
      if (length(modelTerms)>0) {
          aTable$addRow(rowKey=1, list(name="Model"))
          
          for (i in seq_along(modelTerms)) {
                  lab<-jmvcore::stringifyTerm(modelTerms[[i]],raise=T)
                  aTable$addRow(rowKey=i+1, list(name=lab))
          }
         aTable$addRow(rowKey=i+2, list(name="Residuals",f="",p="",etaSq="",etaSqP="",omegaSq=""))
         aTable$addRow(rowKey=i+3, list(name="Total",f="",p="",etaSq="",etaSqP="",omegaSq=""))
         
         aTable$addFormat(col=1, rowNo=i+2, format=jmvcore::Cell.BEGIN_END_GROUP)
         aTable$addFormat(col=1, rowNo=2, format=jmvcore::Cell.BEGIN_GROUP)
         
      }
      ### we want to output SS error and total for intercept-only models ######
      if (length(modelTerms) == 0) {
        aTable$addRow(rowKey=1, list(name="Residuals",f="",p="",etaSq="",etaSqP="",omegaSq=""))
        aTable$addRow(rowKey=2, list(name="Total",f="",p="",etaSq="",etaSqP="",omegaSq=""))
      }
      ## fixed effects parameters

      aTable<-self$results$main$fixed
      dep64<-jmvcore::toB64(dep)
      modelTerms64<-lapply(modelTerms,jmvcore::toB64)
      formula64<-as.formula(lf.constructFormula(dep64,modelTerms64,fixedIntercept))
      mynames64<-colnames(model.matrix(formula64,data))
      terms<-n64$nicenames(mynames64)  
      labels<-n64$nicelabels(mynames64)
      ciWidth<-self$options$paramCIWidth
      aTable$getColumn('cilow')$setSuperTitle(jmvcore::format('{}% Confidence Interval', ciWidth))
      aTable$getColumn('cihig')$setSuperTitle(jmvcore::format('{}% Confidence Interval', ciWidth))
      for(i in seq_along(terms)) 
          aTable$addRow(rowKey=i,list(source=lf.nicifyTerms(terms[[i]]),label=lf.nicifyLabels(labels[[i]])))

      ## hide effects labels if no factor is there
      if (!is.something(factors))
         aTable$getColumn('label')$setVisible(FALSE)
        # other inits
        gplots.initPlots(self,data,private$.cov_condition)
        gposthoc.init(data,self$options, self$results$postHocs)     
        gmeans.init(data,self$options,self$results$emeansTables,private$.cov_condition)
        gsimple.init(data,self$options,self$results$simpleEffects)
        mi.initContrastCode(data,self$options,self$results,n64)
    },
    .run=function() {
      n64<-private$.names64
      ginfo("run")
      # collect some option
      dep <- self$options$dep
      if (is.null(dep))
        return()
      modelTerms<-self$options$modelTerms
      fixedIntercept<-self$options$fixedIntercept
      factors <- self$options$factors
      covs<-self$options$covs
      
      if (self$options$simpleScale=="mean_sd" && self$options$cvalue==0)
          return()
      if (self$options$simpleScale=="percent" && self$options$percvalue==0)
         return()
      ###############
      # this allows intercept only model to be passed by syntax interfase
      aOne<-which(unlist(modelTerms)=="1")
      if (is.something(aOne)) {
        modelTerms[[aOne]]<-NULL
        fixedIntercept=TRUE
      }
      modelTerms<-lapply(modelTerms,jmvcore::toB64)
      modelFormula<-lf.constructFormula(jmvcore::toB64(dep),modelTerms,fixedIntercept)
      if (modelFormula==FALSE)  
          return()
      ### collect the tables #######
      infoTable<-self$results$info
      estimatesTable <- self$results$main$fixed
      anovaTable<-self$results$main$anova

      ##### clean the data ####
      data<-private$.cleandata()
      data<-mf.checkData(self$options,data)
      

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
      ## for some reason the lm model is very heavy to save in table$state
      ## so we estimate it every time
      model<- try(private$.estimate(modelFormula, data=data))
      mi.check_estimation(model,n64)
      model<-mi.model_check(model)
      private$.model <- model
      #### save the model for R interface ####
      self$results$.setModel(model)
      ### if it worked before, we skip building the tables, 
      ### otherwise we store a flag  in parameters table state so next time we know it worked

      if (is.null(estimatesTable$state)) {
               ginfo("Parameters have been estimated")
               gwarnings<-list()
               model_summary<-try(summary(model))
               mi.check_estimation(model_summary,n64)
      
      ### coefficients summary results ####
      
               parameters<-try(parameters<-mf.summary(model))
               mi.check_estimation(parameters,n64)

               if ("beta" %in% self$options$effectSize) {
                        .beta<-NA
                        zdata<-data
                        zdata[[jmvcore::toB64(dep)]]<-scale(zdata[[jmvcore::toB64(dep)]])
                        for (var in covs)
                             zdata[[jmvcore::toB64(var)]]<-scale(zdata[[jmvcore::toB64(var)]])
                        
                         zmodel<-try(stats::lm(modelFormula,data=zdata))
                         warn<-mi.warn_estimation(zmodel,n64)
                         if (is.something(warn)) {
                             attr(parameters,"warning")<-paste0(warn,". Betas cannot be computed.")
                         } else {
                            beta<-coef(zmodel)
                            beta[1]<-0
                            parameters<-cbind(parameters,beta) 
                          }
                  }
                  #### confidence intervals ######
                  ciWidth<-self$options$paramCIWidth/100
                  parameters<-mf.confint(model,level=ciWidth,parameters)
                  rownames(parameters)<-n64$nicenames(rownames(parameters))
                  ######  fill the table ########
                  for (i in 1:nrow(parameters)) {
                    tableRow=parameters[i,]
                    estimatesTable$setRow(rowNo=i,tableRow)
                  }
                  
                   
               estimatesTable$setState(attributes(parameters))
      }
      if (is.null(anovaTable$state)) {
        
               ### anova results ####
               anova_res<-data.frame()
               if (length(modelTerms)==0) {
                   attr(anova_res,"warning")<-"F-Tests cannot be computed"
               } else {
                   suppressWarnings({
                   anova_res <- try(mf.anova(model)) # end suppressWarnings
                 })
                 mi.check_estimation(anova_res,n64)
               }

               attr(anova_res,"warning")<-append(attr(anova_res,"warning"),"First warning")

               ### prepare info table #########   
               
               info.r2m<-model_summary$r.squared   
               info.r2c<-model_summary$adj.r.squared
                   
               infoTable$setRow(rowKey="r2m",list(value=info.r2m))
               infoTable$setRow(rowKey="r2c",list(value=info.r2c))
        
               ### end of info table ###
        
        # anova table ##
                ### we still need to check for modelTerms, because it may be a intercept only model, where no F is computed
                 if (length(modelTerms)>0) {
                       rawlabels<-rownames(anova_res)
                       labels<-n64$nicenames(rawlabels)
                       trows<-dim(anova_res)[1]-1
                       for (i in seq_len(trows)) {
                              tableRow<-anova_res[i,]  
                              anovaTable$setRow(rowNo=i,tableRow)
                       }
                       tss<-sum(anova_res[c(1,i+1),"ss"])
                       tdf<-sum(anova_res[c(1,i+1),"df"])
                       anovaTable$setRow(rowNo=i+1,anova_res[i+1,c("ss","df")])
                       anovaTable$setRow(rowNo=i+2,list("ss"=tss,"df"=tdf))
                       
 
                 }
               ### we want to output the error SS for intercept only model
               if (length(modelTerms)==0 & fixedIntercept==TRUE) {
                 ss<-var(model$residuals)*(model$df.residual)
                 anovaTable$setRow(rowKey=1,list("ss"=ss,df=model$df.residual,f="",p="",etaSq="",etaSqP="",omegaSq=""))
                 anovaTable$setRow(rowKey=2, list("ss"=ss,df=model$df.residual,p="",etaSq="",etaSqP="",omegaSq=""))
               }                 
               
               ### we want to output the error SS for zero only model
               if (length(modelTerms)==0 & fixedIntercept==FALSE) {
                 ss<-sum(data[[jmvcore::toB64(dep)]]^2)
                 df<-dim(data)[1]
                 anovaTable$setRow(rowKey=1,list("ss"=ss,df=df,f="",p="",etaSq="",etaSqP="",omegaSq=""))
                 anovaTable$setRow(rowKey=2, list("ss"=ss,df=df,p="",etaSq="",etaSqP="",omegaSq=""))
                 attr(anova_res,"warning")<-append(attr(anova_res,"warning"),WARNS["glm.zeromodel"])
               }                 

                anovaTable$setState(attributes(anova_res))               
        # end of check state
        } else
            ginfo("anova and parameters have been recycled")

        pstate<-estimatesTable$state      
        ########## update notes ##########
        iatt<-attr(model,"infoTable")
        mi.infotable_footnotes(infoTable,iatt)
        patt<-estimatesTable$state$warning
        for (w in patt)
          estimatesTable$setNote(w,w)
        aatt<-anovaTable$state$warning
        for (w in aatt)
          anovaTable$setNote(w,w)
        

        private$.preparePlots(model)
        gposthoc.populate(model,self$options,self$results$postHocs)
        gmeans.populate(model,self$options,self$results$emeansTables,private$.cov_condition)
        gsimple.populate(model,self$options,self$results$simpleEffects,private$.cov_condition)        
        private$.populateLevenes(model)
        private$.populateNormTest(model)
        
    },
  .cleandata=function() {
      Sys.setlocale("LC_NUMERIC", "C")
      n64<-private$.names64
      dep <- self$options$dep
      factors <- self$options$factors
      modelTerms<-self$options$modelTerms
      covs<-self$options$covs     
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
      
      if ( ! is.null(dep)) {
        data[[jmvcore::toB64(dep)]] <- jmvcore::toNumeric(dataRaw[[dep]])
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
    .estimate = function(form, data) {
      stats::lm(form, data=data)
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

  gplots.images(self=self,data=predData,raw=rawData,range=yAxisRange)
    
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
  
  
  if (errorType=="ci")
    errorType<-paste0(ciWidth,"% ",toupper(errorType))
  
  if ( ! is.null(linesName)) {
    p<-gplots.twoWaysPlot(image,ggtheme,depName,groupName,linesName,errorType)
  } else {
    p<-gplots.oneWayPlot(image,ggtheme,depName,groupName,errorType)
  }       
  return(p)
},


.populateLevenes=function(model) {
  
  if ( ! self$options$homoTest)
    return()
  
  data<-model$model
  data$res<-residuals(model)
  factors <- mf.getModelFactors(model)
  if (is.null(factors))
    return()
  rhs <- paste0('`', factors, '`', collapse=':')
  formula <- as.formula(paste0('`res`~', rhs))
  result <- car::leveneTest(formula, data, center="mean")
  table <- self$results$get('assumptions')$get('homoTest')
   
  table$setRow(rowNo=1, values=list(
    F=result[1,'F value'],
    df1=result[1,'Df'],
    df2=result[2,'Df'],
    p=result[1,'Pr(>F)']))
},


.populateNormTest=function(model) {
  
  if ( ! self$options$normTest)
    return()
  rr<-residuals(model)
  ks<-ks.test(rr,"pnorm",mean(rr),sd(rr))
  st<-shapiro.test(rr)
  
  result<-rbind(cbind(ks$statistic,ks$p.value),
             cbind(st$statistic,st$p.value))

  table <- self$results$get('assumptions')$get('normTest')

  table$setRow(rowNo=1, values=list(test="Kolmogorov-Smirnov",stat=result[1,1],p=result[1,2]))
  table$setRow(rowNo=2, values=list(test="Shapiro-Wilk",stat=result[2,1],p=result[2,2]))
  
},


.qqPlot=function(image, ggtheme, theme, ...) {
  dep <- self$options$dep
  factors <- self$options$factors
  model<-private$.model      
  if (is.null(model) )
    return(FALSE)
  
  data <- model$model
  residuals <- rstandard(model)
  df <- as.data.frame(qqnorm(residuals, plot.it=FALSE))
  print(ggplot2::ggplot(data=df, aes(y=y, x=x)) +
          geom_abline(slope=1, intercept=0, colour=theme$color[1]) +
          geom_point(aes(x=x,y=y), size=2, colour=theme$color[1]) +
          xlab("Theoretical Quantiles") +
          ylab("Standardized Residuals") +
          ggtheme)
  
  TRUE
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


