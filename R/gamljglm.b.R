#' @import lmerTest
library(lmerTest)

gamljGLMClass <- R6::R6Class(
  "gamljGLMClass",
  inherit = gamljGLMBase,
  private=list(
    .model=NA,
    .names64=NA,
    .cov_condition=conditioning$new(),
    .postHocRows=NA,
    .init=function() {
      mark("init")
      private$.names64<-names64$new()
      n64<-private$.names64
      infoTable<-self$results$info
      
      getout<-FALSE
      if (is.null(self$options$dep)) {
        infoTable$addRow(rowKey="gs1",list(info="Get started",value="Select the dependent variable"))
        getout<-TRUE
      }
      if (getout) {
        if (length(self$options$modelTerms) == 0) {
          infoTable$addRow(rowKey="gs4",list(info="Optional",value="Select factors and covariates"))
        }
        return(FALSE)
      }
      modelTerms<-private$.modelTerms()
      modelFormula<-private$.modelFormula()
      data<-private$.cleandata()
      
      ### initialize conditioning of covariates
      if (!is.null(self$options$covs)) {
      span<-ifelse(self$options$simpleScale=="mean_sd",self$options$cvalue,self$options$percvalue)
      private$.cov_condition<-conditioning$new(self$options$covs,self$options$simpleScale,span)
      }
      #####################
      
      #### info table #####
      infoTable<-self$results$info

      infoTable$addRow(rowKey="est",list(info="Estimate",value="Linear mixed model fit by OLS"))
      infoTable$addRow(rowKey="call",list(info="Call",value=n64$translate(modelFormula)))
      infoTable$addRow(rowKey="r2m",list(info="R-squared"))
      infoTable$addRow(rowKey="r2c",list(info="Adj. R-squared"))
      
      

      ## anova Table 
      if (length(modelTerms)>0) {
          aTable<- self$results$main$anova
          aTable$addRow(rowKey=1, list(name="Model"))
          aTable$addFormat(col=1, rowNo=1, format=jmvcore::Cell.BEGIN_END_GROUP)
          
          for (i in seq_along(modelTerms)) {
                  lab<-.nicifyTerms(jmvcore::composeTerm(modelTerms[i]))
                  aTable$addRow(rowKey=i+1, list(name=lab))
          }
         aTable$addRow(rowKey=i+2, list(name="Residuals",f="",p="",etaSq="",etaSqP="",omegaSq=""))
         aTable$addFormat(col=1, rowNo=i+2, format=jmvcore::Cell.BEGIN_END_GROUP)
         
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
      anovaTable<-self$results$main$anova

      ##### clean the data ####
      data<-private$.cleandata()
      data<-mf.checkData(self$options,data)
      if (!is.data.frame(data))
        reject(data)
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
      ### otherwise we store a flag  in parameters table state
      if (is.null(estimatesTable$state)) {
               mark("anova and parameters have been estimated")
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
      
                if ("beta" %in% self$options$effectSize) {
                    zdata<-data
                    zdata[[jmvcore::toB64(dep)]]<-scale(zdata[[jmvcore::toB64(dep)]])
                    for (var in covs)
                         zdata[[jmvcore::toB64(var)]]<-scale(zdata[[jmvcore::toB64(var)]])
                         beta<-coef(stats::lm(modelFormula,data=zdata))
                         if (any(is.na(beta)))
                             estimatesTable$setNote("nobeta",WARNS["ano.aliased"])
                        else {
                             beta[1]<-0
                             parameters<-cbind(parameters,beta) 
                        }
                }
      
               
               ### anova results ####
               anova_res<-NULL
               if (length(modelTerms)==0) {
                   anovaTable$setNote("warning","F-Tests cannot be computed")
               } else {
                   suppressWarnings({
                   anova_test <- try(anova_res<-mf.anova(model)) # end suppressWarnings
                 })
                   if (jmvcore::isError(anova_test)) 
                      jmvcore::reject(jmvcore::extractErrorMessage(anova_test), code='error')
               }


             
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
                       anovaTable$setRow(rowNo=i+1,anova_res[i+1,c("ss","df")])
                       
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
        citry<-try({
            ci<-mf.confint(model,level=ciWidth)
            colnames(ci)<-c("cilow","cihig")
            parameters<-cbind(parameters,ci) 
          })
          if (jmvcore::isError(citry)) {
            message <- jmvcore::extractErrorMessage(citry)
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
        estimatesTable$setState(TRUE)
        
  
        # end of check state
        } else
          mark("anova and parameters have been recycled")
    
        private$.preparePlots(private$.model)
        private$.populateSimple(private$.model)
        private$.populatePostHoc(private$.model)
        private$.populateDescriptives(private$.model)
        private$.populateLevenes(model)
        private$.populateNormTest(model)
        
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

      dataRaw <- self$data
      data <- list()
      for (factor in factors) {
        ### we need this for Rinterface ####
        if (!("factor" %in% class(dataRaw[[factor]]))) {
          info(paste("Warning, variable",factor," has been coerced to factor"))
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
    .modelFormula=function() {

      if (!is.null(self$options$dep))  {
        dep<-jmvcore::toB64(self$options$dep)
      } else return(FALSE)
      private$.fixedFormula()
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
    rawData=lp.rawData(model,depName,groupName,linesName,plotsName)
  else 
    rawData<-NULL

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


  if (is.null(plotsName)) {
    image <- self$results$get('descPlot')
    image$setState(list(data=predData, raw=rawData, range=yAxisRange, randomData=NULL))
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
      image$setState(list(data=sdata,raw=sraw, range=yAxisRange,randomData=NULL))
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
          mark("simple effects have been computed")
          
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

.populateLevenes=function(model) {
  
  if ( ! self$options$homo)
    return()
  
  data<-model$model
  data$res<-residuals(model)
  factors <- mf.getModelFactors(model)
  if (is.null(factors))
    return()
  rhs <- paste0('`', factors, '`', collapse=':')
  formula <- as.formula(paste0('`res`~', rhs))
  result <- car::leveneTest(formula, data, center="mean")
  table <- self$results$get('assump')$get('homo')
   
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

  table <- self$results$get('assump')$get('normTest')

  table$setRow(rowNo=1, values=list(test="Kolmogorov-Smirnov",stat=result[1,1],p=result[1,2]))
  table$setRow(rowNo=2, values=list(test="Shapiro-Wilk",stat=result[2,1],p=result[2,2]))
  
},


.qqPlot=function(image, ggtheme, theme, ...) {
  library(ggplot2)
  dep <- self$options$dep
  factors <- self$options$factors
  modelTerms <- private$.modelTerms()
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


