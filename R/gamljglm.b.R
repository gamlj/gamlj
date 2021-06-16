gamljGlmClass <- R6::R6Class(
  "gamljGlmClass",
  inherit = gamljGlmBase,
  private=list(
    .model=NA,
    .names64=NA,
    .data_machine=NULL,
    .estimate_machine=NULL,
    .plotter_machine=NULL,
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
      
      ### set up the R6 workhorse class
      
      data_machine<-Datamatic$new(self$options,self$data)
      estimate_machine<-Estimate$new(self$options,data_machine)
      plotter_machine<-Plotter$new(estimate_machine,self$results)

      ### info table ###
      j.init_table(self$results$info,estimate_machine$tab_info) 

      ### r2 table does not need initializing ###

      ### anova table ###
      j.init_table(self$results$main$anova,estimate_machine$tab_anova) 

      ### estimates table ###
      j.init_table(self$results$main$coefficients,estimate_machine$tab_coefficients, ci=T,ciwidth=self$options$ciWidth)

      if (!is.something(self$options$factors))
        self$results$main$coefficients$getColumn('label')$setVisible(FALSE)
      
      ### intercept initialized in yalm ###

      ### effectsizes table ###
      j.init_table(self$results$main$effectsizes,estimate_machine$tab_effectsizes,ci=T,ciwidth=self$options$ciWidth) 
      
      
      ### estimate marginal means
      
      if (is.something(self$options$emmeans)) {
  
              self$results$emmeans$setVisible(TRUE)
              for (i in seq_along(self$options$emmeans)) {
                    term<-self$options$emmeans[[i]]
                    spaceby=NULL
                    if (length(term)>1) spaceby<-1
                    
                    aTable <- self$results$emmeans$addItem(key = jmvcore::stringifyTerm(term))
                    j.expand_table(aTable,term)
                    j.init_table(aTable,estimate_machine$tab_emmeans[[i]], ci=T,ciwidth=self$options$ciWidth,spaceby = spaceby)
              } 
      
      }
      
      ### simple effects ####
      if (is.something(estimate_machine$tab_simpleAnova)) {

         j.expand_table(self$results$simpleEffects$anova,self$options$simpleModerators) 
         title<-paste("Omnibus test for variable",self$options$simpleVariable)
         j.init_table(self$results$simpleEffects$anova,estimate_machine$tab_simpleAnova,title=title)
         title<-paste("Parameter estimates for variable",self$options$simpleVariable)
         j.expand_table(self$results$simpleEffects$coefficients,self$options$simpleModerators,superTitle = "Moderators") 
         j.init_table(self$results$simpleEffects$coefficients,estimate_machine$tab_simpleCoefficients, ci=T,ciwidth=self$options$ciWidth,title=title)

      }
      
      ### simple interactions ######
      
      if (is.something(estimate_machine$tab_simpleInteractionCoefficients)) {
  
        self$results$simpleInteractions$setVisible(TRUE)
        ### moderators should be reverted in order so they make sense
        terms<-c(self$options$simpleVariable,self$options$simpleModerators)
        for (i in seq_along(estimate_machine$tab_simpleInteractionCoefficients)) {
          aGroup <- self$results$simpleInteractions$addItem(key = i)
          aTable<-aGroup$interactionCoefficients
          term<-setdiff(terms,estimate_machine$tab_simpleInteractionCoefficients[[i]])
          j.expand_table(aTable,estimate_machine$tab_simpleInteractionCoefficients[[i]],superTitle="Moderator")
          title<-paste("Parameter Estimates for simple interaction",  jmvcore::stringifyTerm(term))
          j.init_table(aTable,FALSE, ci=T,ciwidth=self$options$ciWidth,title=title)

          aTable<-aGroup$interactionAnova
          j.expand_table(aTable,estimate_machine$tab_simpleInteractionAnova[[i]],superTitle="Moderator")
          title<-paste("ANOVA test for simple interaction",  jmvcore::stringifyTerm(term))
          j.init_table(aTable,FALSE, ci=F,title=title)
          
          title<-paste("Simple interaction:",  jmvcore::stringifyTerm(term))
          aGroup$setTitle(title)
          
        } 
        
        
      }
        
      
      

      ### posthoc ####
      
      if (is.something(self$options$posthoc)) {
        
        for (i in seq_along(self$options$posthoc)) {
          term<-self$options$posthoc[[i]]
          aTable<-self$results$posthoc$get(key = term)
          aTable$setTitle(paste0("Post Hoc Comparisons - ", jmvcore::stringifyTerm(term)))
          j.expand_table(aTable,names(estimate_machine$tab_posthoc[[i]]),superTitle="Comparison",names64=FALSE)
          j.init_table(aTable,estimate_machine$tab_posthoc[[i]],ci=T,ciwidth = self$options$ciWidth)
        } 
        
      }

      if (is.something(estimate_machine$tab_contrastcodes)) {
        
          
        for (i in seq_along(self$options$factors)) {
          term<-self$options$factors[[i]]
          aTable<-self$results$main$contrastCodeTables$get(key = term)
          eTable<-estimate_machine$tab_contrastcodes[[i]]
          levels_pos<-seq_len(length(names(eTable))-2)
          j.expand_table(aTable,names(eTable)[levels_pos],append=T,type="number",names64=FALSE) 
          names(eTable)[levels_pos]<-make.names(names(eTable)[levels_pos])
          j.init_table(aTable,eTable)
        

        }
      }
      plotter_machine$initPlots()
      
      private$.data_machine<-data_machine
      private$.estimate_machine<-estimate_machine
      private$.plotter_machine<-plotter_machine
    },
    .run=function() {
      ginfo("run")
      
      private$.ready<-readiness(self$options)
      if (!private$.ready$ready) {
        return()
      }
      
      
      data<-private$.data_machine$cleandata(self$data)
      private$.estimate_machine$estimate(data)
      
      j.add_warnings(self$results$info,private$.data_machine,"data")
      j.add_warnings(self$results$info,private$.estimate_machine,"info")
      
      ## R2 table ###
      j.fill_table(self$results$main$r2,private$.estimate_machine$tab_r2)
      j.add_warnings(self$results$main$anova,private$.estimate_machine,"tab_r2")
      
      ## Intercept table ###
      j.fill_table(self$results$main$intercept,private$.estimate_machine$tab_intercept)
      j.add_warnings(self$results$main$anova,private$.estimate_machine,"tab_intercept")
      
      ## anova table ###
      j.fill_table(self$results$main$anova,private$.estimate_machine$tab_anova)
      j.add_warnings(self$results$main$anova,private$.estimate_machine,"tab_anova")

      
      ## effect sizes table ###
      j.fill_table(self$results$main$effectsizes,private$.estimate_machine$tab_effectsizes)
      j.add_warnings(self$results$main$effectsizes,private$.estimate_machine,"tab_effectsizes")
      
      ### parameter estimates
      j.fill_table(self$results$main$coefficients,private$.estimate_machine$tab_coefficients)
      j.add_warnings(self$results$main$coefficients,private$.estimate_machine,"tab_coefficients")

      
      ### simple effects
      j.fill_table(self$results$simpleEffects$anova,private$.estimate_machine$tab_simpleAnova)
      j.add_warnings(self$results$simpleEffects$anova,private$.estimate_machine,"tab_simpleAnova")
      
      j.fill_table(self$results$simpleEffects$coefficients,private$.estimate_machine$tab_simpleCoefficients)
      j.add_warnings(self$results$simpleEffects$coefficients,private$.estimate_machine,"tab_simpleCoefficients")
      
      ### simple interactions
      mark("simple")
      mark(private$.estimate_machine$tab_simpleInteractionCoefficients)
      mark("int")
      if (is.something(private$.estimate_machine$tab_simpleInteractionCoefficients)) {
        
        for (i in seq_along(private$.estimate_machine$tab_simpleInteractionCoefficients)) {
          aGroup <- self$results$simpleInteractions$get(key = i)
          aTable<-aGroup$interactionCoefficients
          j.fill_table(aTable,private$.estimate_machine$tab_simpleInteractionCoefficients[[i]],append = T)
          aTable<-aGroup$interactionAnova
          j.fill_table(aTable,private$.estimate_machine$tab_simpleInteractionAnova[[i]],append = T)
          
 
        } 
      }
      
      
      ### post hoc
      
      if (is.something(private$.estimate_machine$tab_posthoc)) {
        
        for (i in seq_along(self$options$posthoc)) {
          term<-self$options$posthoc[[i]]
          aTable<-self$results$posthoc$get(key = term)
          j.fill_table(aTable,private$.estimate_machine$tab_posthoc[[i]])
        } 
      }

      if (is.something(private$.estimate_machine$tab_emmeans)) {
        
        for (i in seq_along(self$options$emmeans)) {
          term<-self$options$emmeans[[i]]
          aTable<-self$results$emmeans$get(key = jmvcore::stringifyTerm(term))
          j.fill_table(aTable,private$.estimate_machine$tab_emmeans[[i]])
        } 
      }
      
      private$.plotter_machine$preparePlots()
      j.add_warnings(self$results$plotnotes,private$.plotter_machine,"plot")
      
      #save model preds and resids            
      private$.estimate_machine$savePredRes(self$results) 
      

      mark("end")
      return()
          
        
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

.mainPlot=function(image, ggtheme, theme, ...) {

  plot<-private$.plotter_machine$scatterPlot(image)
  plot<-plot + ggtheme
  
  return(plot)
  
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


