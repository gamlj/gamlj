gamljGlmClass <- R6::R6Class(
  "gamljGlmClass",
  inherit = gamljGlmBase,
  private=list(
    .model=NA,
    .names64=NA,
    .data_machine=NULL,
    .estimate_machine=NULL,
    .ready=NULL,
    .init=function() {
      ginfo("init")
      
      class(private$.results) <- c('gamlj', class(private$.results))
      
      private$.ready<-readiness(self$options)
      if (!private$.ready$ready) {
        if(private$.ready$report)
          self$results$info$addRow("info",list(info="Setup",specs=private$.ready$reason))
        return()
      }
      
      ### set up the R6 workhourse class
      
      data_machine<-Datamatic$new(self$options,self$data)
      estimate_machine<-Syntax$new(self$options,data_machine)

      ### info table ###
      j.init_table(self$results$info,estimate_machine$tab_info) 

      ### anova table ###
      j.init_table(self$results$main$anova,estimate_machine$tab_anova) 

      ### estimates table ###
      j.init_table(self$results$main$coefficients,estimate_machine$tab_coefficients, ci=T,ciwidth=self$options$ciWidth)

      if (!is.something(self$options$factors))
        self$results$main$coefficients$getColumn('label')$setVisible(FALSE)
      
      ### intercept table ###
      j.init_table(self$results$main$intercept,estimate_machine$tab_intercept) 
      
      ### effectsizes table ###
      j.init_table(self$results$main$effectsizes,estimate_machine$tab_effectsizes) 
      
      
      ### estimate marginal means
      
      if (is.something(self$options$emmeans)) {
  
              self$results$emmeans$setVisible(TRUE)
              for (i in seq_along(self$options$emmeans)) {
                    term<-self$options$emmeans[[i]]
                    aTable <- self$results$emmeans$addItem(key = jmvcore::stringifyTerm(term))
                    j.expand_table(aTable,term)
                    j.init_table(aTable,estimate_machine$tab_emmeans[[i]], ci=T,ciwidth=self$options$ciWidth)
              } 
      
      }
      
      ### simple effects ####
      if (is.something(estimate_machine$tab_simpleAnova)) {

         j.expand_table(self$results$simpleEffects$anova,self$options$simpleModerators) 
         j.init_table(self$results$simpleEffects$anova,estimate_machine$tab_simpleAnova)
        
         j.expand_table(self$results$simpleEffects$coefficients,self$options$simpleModerators) 
         j.init_table(self$results$simpleEffects$coefficients,estimate_machine$tab_simpleCoefficients, ci=T,ciwidth=self$options$ciWidth)

      }

      ### posthoc ####
      
      if (is.something(self$options$posthoc)) {
        
        for (i in seq_along(self$options$posthoc)) {
          term<-self$options$posthoc[[i]]
          aTable<-self$results$posthoc$get(key = term)
          aTable$setTitle(paste0("Post Hoc Comparisons - ", jmvcore::stringifyTerm(term)))
          
          j.expand_table(aTable,names(estimate_machine$tab_posthoc[[i]]),superTitle="Comparison")
          j.init_table(aTable,estimate_machine$tab_posthoc[[i]])
        } 
        
      }

      if (is.something(estimate_machine$tab_contrastcodes)) {
        
          
        for (i in seq_along(self$options$factors)) {
          term<-self$options$factors[[i]]
          aTable<-self$results$main$contrastCodeTables$get(key = term)
          eTable<-estimate_machine$tab_contrastcodes[[i]]
          levels_pos<-seq_len(length(names(eTable))-2)
          j.expand_table(aTable,names(eTable)[levels_pos],append=T,type="number") 
          names(eTable)[levels_pos]<-tob64(names(eTable)[levels_pos])
          j.init_table(aTable,eTable)
        

        }
      }
      
      
      gplots.initPlots(self,data,private$.cov_condition)
      
      private$.data_machine<-data_machine
      private$.estimate_machine<-estimate_machine
      
    },
    .run=function() {
      return()
      private$.data_machine$cleandata(self$data)
      n64<-private$.names64
      ginfo("run")
      ciWidthp<-self$options$paramCIWidth/100
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
      self$results$.setModel(model)
      ### if it worked before, we skip building the tables, 
      ### otherwise we store a flag  in parameters table state so next time we know it worked
      if (is.null(estimatesTable$state) & model$rank>0) {
               ginfo("Parameters have been estimated")
      ### coefficients summary results ####
               parameters<-try(parameters<-mf.summary(model))
               mi.check_estimation(parameters,n64)

               if ("beta" %in% self$options$effectSize) {
                         ginfo("computing betas...")
                        zdata<-data
                        zdata[[jmvcore::toB64(dep)]]<-scale(zdata[[jmvcore::toB64(dep)]])
                        for (var in covs)
                             zdata[[jmvcore::toB64(var)]]<-scale(zdata[[jmvcore::toB64(var)]])
                         zmodel<-try(stats::lm(modelFormula,data=zdata))
                         warn<-mi.warn_estimation(zmodel,n64)
                         if (is.something(warn)) {
                             attr(parameters,"warning")<-paste0(warn,". Betas cannot be computed.")
                         } else {
                            beta<-coef(zmodel,complete = T)
                            if (fixedIntercept==TRUE)
                                beta[1]<-0
                            if (any(is.na(beta)))
                              attr(parameters,"warning")<-paste0(warn,"Some betas cannot be computed.")
                            parameters<-cbind(parameters,beta)

                         }
                         ginfo("...done")
                         
                  }
                  #### confidence intervals ######
                  
                  parameters<-mf.confint(model,level=ciWidthp,parameters)
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


               ### prepare info table #########   
               model_summary<-try(summary(model))
               mi.check_estimation(model_summary,n64)
               
               info.r2m<-model_summary$r.squared   
               info.r2c<-model_summary$adj.r.squared
                   
               infoTable$setRow(rowKey="r2m",list(value=info.r2m))
               infoTable$setRow(rowKey="r2c",list(value=info.r2c))
               infoTable$setState(list(warning=attr(model,"warning")))
               ### end of info table ###
        
        # anova table ##
                ### we still need to check for modelTerms, because it may be a intercept only model, where no F is computed
                 if (length(modelTerms)>0) {
                       rawlabels<-names(anova_res)
                       labels<-n64$nicenames(rawlabels)
                       trows<-dim(anova_res)[1]-1
                       for (i in seq_along(anova_res)) {
                              tableRow<-anova_res[[i]]  
                              anovaTable$setRow(rowNo=i,tableRow)
                       }
                       
                       
 
                 }
               ### we want to output the error SS for intercept only model
               if (length(modelTerms)==0 & fixedIntercept==TRUE) {
                 ss<-var(model$residuals)*(model$df.residual)
                 anovaTable$setRow(rowKey=1,list("ss"=ss,df=model$df.residual,f="",p="",etaSq="",etaSqP="",omegaSq="",epsilonSq=""))
                 anovaTable$setRow(rowKey=2, list("ss"=ss,df=model$df.residual,p="",etaSq="",etaSqP="",omegaSq="",epsilonSq=""))
               }                 
               
               ### we want to output the error SS for zero only model
               if (length(modelTerms)==0 & fixedIntercept==FALSE) {
                 ss<-sum(data[[jmvcore::toB64(dep)]]^2)
                 df<-dim(data)[1]
                 anovaTable$setRow(rowKey=1,list("ss"=ss,df=df,f="",p="",etaSq="",etaSqP="",omegaSq="",epsilonSq=""))
                 anovaTable$setRow(rowKey=2, list("ss"=ss,df=df,p="",etaSq="",etaSqP="",omegaSq="",epsilonSq=""))
                 attr(anova_res,"warning")<-append(attr(anova_res,"warning"),WARNS["glm.zeromodel"])
               }                 

                anovaTable$setState(attributes(anova_res))               
        # end of check state
        } else
            ginfo("anova have been recycled")

        pstate<-estimatesTable$state      
        ########## update notes ##########
        iatt<-attr(model,"infoTable")
        out.infotable_footnotes(infoTable,iatt)
        out.table_notes(infoTable)
        out.table_notes(estimatesTable)
        out.table_notes(anovaTable)

        private$.populateInterceptInfo(model)
        private$.populateEffectSizeInfo(model,ciWidthp)
        
        private$.preparePlots(model)
        gposthoc.populate(model,self$options,self$results$postHocs)
        gmeans.populate(model,self$options,self$results$emeansTables,private$.cov_condition)
        gsimple.populate(model,self$options,self$results$simpleEffects,private$.cov_condition)        
        private$.populateLevenes(model)
        private$.populateNormTest(model)
        
        mf.savePredRes(self$options,self$results,model) 
          
        
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
      model<-stats::lm(form, data=data)
      attr(model,"refit")<-list(command="lm",
                                coptions=list(formula=private$.names64$translate(form)),
                                eoptions=list(formula=private$.names64$translate(form)))
      
      model
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
  table <- self$results$get('assumptions')$get('normTest')
  
  rr<-residuals(model)
  ks<-ks.test(rr,"pnorm",mean(rr),sd(rr))
  table$setRow(rowNo=1, values=list(test="Kolmogorov-Smirnov",stat=ks$statistic,p=ks$p.value))

  st<-try(shapiro.test(rr))
  if (jmvcore::isError(st)) {
     table$setNote("noshapiro","Shapiro-Wilk not available due to too large number of cases")
     table$setRow(rowNo=2, values=list(test="Shapiro-Wilk",stat="",p=""))
  }
  else
    table$setRow(rowNo=2, values=list(test="Shapiro-Wilk",stat=st$statistic,p=st$p.value))
  
},

.populateInterceptInfo=function(model) {
  
  if (!self$options$interceptInfo || ! self$options$fixedIntercept) 
    return()
  ss<-summary(model)
  tt<-ss$coefficients[1,3]
  f<-tt^2
  df<-stats::df.residual(model)
  p<-ss$coefficients[1,4]
  peta<-effectsize::t_to_eta2(tt,df_error = df)
  omega<-effectsize::t_to_omega2(tt,df_error = df)
  epsilon<-effectsize::t_to_epsilon2(tt,df_error = df)
  tableRow<-list(df=df,f=f,etaSqP=peta$Eta_Sq_partial,omegaSq=omega$Omega_Sq_partial,epsilonSq=epsilon$Epsilon2_partial,p=p)
  aTable<-self$results$main$interceptTable
  aTable$setRow(rowNo=1,tableRow)
},

.populateEffectSizeInfo=function(model,ciWidth) {
  
  if (!self$options$effectSizeInfo) 
    return()
  ano<-car::Anova(model,type=3)
      eta<-effectsize::eta_squared(ano,partial = F,ci=ciWidth,verbose=F)
      peta<-effectsize::eta_squared(ano,partial = T,ci=ciWidth,verbose=F)
      omega<-  effectsize::omega_squared(ano,partial = T,ci=ciWidth,verbose=F)
      epsilon<-  effectsize::epsilon_squared(ano,partial = T,ci=ciWidth,verbose=F)
  aTable<-self$results$main$effectSizeTable
  j<-1
  i<-1
  for (i in seq_along(eta$Parameter)) {
       aTable$setRow(rowNo=j,list(estimate=eta[[2]][i],cilow=eta$CI_low[i],cihig=eta$CI_high[i]))
       aTable$setRow(rowNo=j+1,list(estimate=peta[[2]][i],cilow=peta$CI_low[i],cihig=peta$CI_high[i]))
       aTable$setRow(rowNo=j+2,list(estimate=omega[[2]][i],cilow=omega$CI_low[i],cihig=omega$CI_high[i]))
       aTable$setRow(rowNo=j+3,list(estimate=epsilon[[2]][i],cilow=epsilon$CI_low[i],cihig=epsilon$CI_high[i]))
    j<-j+4
  }


},

.qqPlot=function(image, ggtheme, theme, ...) {
  dep <- self$options$dep
  factors <- self$options$factors
  model<-private$.model      
  if (is.null(model) )
    return(FALSE)
  
  data <- model$model
  residuals <- as.numeric(scale(residuals(model)))
  df <- as.data.frame(qqnorm(residuals, plot.it=FALSE))
  plot<-ggplot2::ggplot(data=df, aes(y=y, x=x)) +
          geom_abline(slope=1, intercept=0, colour=theme$color[1]) +
          geom_point(aes(x=x,y=y), size=2, colour=theme$color[1]) +
          xlab("Theoretical Quantiles") +
          ylab("Standardized Residuals") +ggtheme
  
  return(plot)
},
.normPlot=function(image, ggtheme, theme, ...) {
  
  model<-private$.model      
  if (is.null(model) )
    return(FALSE)
  plot<-gplots.normPlot(model,ggtheme,theme)
  return(plot)
},

.residPlot=function(image, ggtheme, theme, ...) {
  
  model<-private$.model      
  if (is.null(model) )
    return(FALSE)
  plot<-gplots.residPlot(model,ggtheme,theme)
  return(plot)
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
  
  if (name =='scaling') {
    vec<-sourcifyList(option,"centered")
    return(vec)
  }
  if (name =='contrasts') {
    vec<-sourcifyList(option,"simple")
    return(vec)
  }
  if (name == 'postHoc') {
    if (length(value) == 0)
      return('')
  }
  
  super$.sourcifyOption(option)
}
)
)


