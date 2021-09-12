
gamljMixedClass <- R6::R6Class(
  "gamljMixedClass",
  inherit=gamljMixedBase,
  private=list(
    .model=NA,
    .names64=NA,
    .cov_condition=conditioning$new(),
    .postHocRows=NA,
    .init=function() {
      ginfo("init")
      class(private$.results) <- c('gamlj', class(private$.results))
      private$.names64<-names64$new()
      n64<-private$.names64
      reml<-self$options$reml
      infoTable<-self$results$info
      dep<-self$options$dep
      modelTerms<-self$options$modelTerms
      factors<-self$options$factors
      covs<-self$options$covs
      fixedIntercept<-self$options$fixedIntercept
      emmeans::emm_options(lmerTest.limit = 25000)  
      ciWidth<-self$options$paramCIWidth
      
            
      getout<-FALSE
      if (is.null(dep)) {
        infoTable$addRow(rowKey="gs1",list(info="Get started",value="Select the dependent variable"))
        getout<-TRUE
      }
      if (is.null(self$options$cluster)) {
        infoTable$addRow(rowKey="gs2",list(info="Get started",value="Select at least one cluster variable"))
        getout=TRUE
      }
      if (!is.something(unlist(self$options$randomTerms))) {
        infoTable$addRow(rowKey="gs3",list(info="Get started",value="Select at least one term in Random Effects"))
        getout=TRUE
      }
      
      aOne<-which(unlist(modelTerms)=="1")
      if (is.something(aOne)) {
        modelTerms[[aOne]]<-NULL
        fixedIntercept=TRUE
      }
      
      if (length(modelTerms) == 0 && fixedIntercept==FALSE) {
        if (is.null(factors) && is.null(covs))
          infoTable$addRow(rowKey="gs4",list(info="Optional",value="Select factors and covariates"))
        else
          jmvcore::reject("Please specify a model")            
        getout<-TRUE
      }
      
      if (getout) 
        return(FALSE)

      data<-private$.cleandata()
      
      ### initialize conditioning of covariates
      if (is.something(self$options$covs)) {
      span<-ifelse(self$options$simpleScale=="mean_sd",self$options$cvalue,self$options$percvalue)
      private$.cov_condition<-conditioning$new(self$options$covs,self$options$simpleScale,span)
      }
      #####################
      #### info table #####
      infoTable<-self$results$info
      infoTable$addRow(rowKey="est",list(info="Estimate"))
      infoTable$addRow(rowKey="call",list(info="Call"))
      infoTable$addRow(rowKey="aic",list(info="AIC"))
      infoTable$addRow(rowKey="bic",list(info="BIC"))
      infoTable$addRow(rowKey="log",list(info="LogLikel."))
      infoTable$addRow(rowKey="r2m",list(info="R-squared Marginal"))
      infoTable$addRow(rowKey="r2c",list(info="R-squared Conditional"))
      infoTable$addRow(rowKey="conv",list(info="Converged"))
      infoTable$addRow(rowKey="opt",list(info="Optimizer"))
      
      
      ## random table
      aTable<-self$results$main$random
      aTable$addRow(rowKey="res",list(groups="Residuals",name=""))

      if (self$options$ciRE==TRUE) {
            aTable$getColumn('cilow')$setSuperTitle(jmvcore::format('Variance {}% C.I.', ciWidth))
            aTable$getColumn('cihig')$setSuperTitle(jmvcore::format('Variance {}% C.I.', ciWidth))
      }

      ## anova Table 
      if (length(modelTerms)>0) {
          aTable<- self$results$main$anova
          for (i in seq_along(modelTerms)) 
                  aTable$addRow(rowKey=i, list(name=" "))
      }
      
      ## fixed effects parameters
      modelFormula<-lf.constructFormula(dep,modelTerms,self$options$fixedIntercept)
      
      aTable<-self$results$main$fixed
      dep64<-jmvcore::toB64(dep)
      modelTerms64<-lapply(modelTerms,jmvcore::toB64)
      formula64<-as.formula(lf.constructFormula(dep64,modelTerms64,self$options$fixedIntercept))
      mynames64<-colnames(model.matrix(formula64,data))
      terms<-n64$nicenames(mynames64)  
      labels<-n64$nicelabels(mynames64)
      aTable$getColumn('cilow')$setSuperTitle(jmvcore::format('{}% Confidence Interval', ciWidth))
      aTable$getColumn('cihig')$setSuperTitle(jmvcore::format('{}% Confidence Interval', ciWidth))

      for(i in seq_along(terms)) 
          aTable$addRow(rowKey=i,list(source=lf.nicifyTerms(terms[[i]]),label=lf.nicifyLabels(labels[[i]])))
      
      if (!is.something(self$options$factors))
           aTable$getColumn('label')$setVisible(FALSE)
      
       
      
        # other inits
        gplots.initPlots(self,data,private$.cov_condition)
        gposthoc.init(data,self$options, self$results$postHocs)     
        gmeans.init(data,self$options,self$results$emeansTables,private$.cov_condition)
        gsimple.init(data,self$options,self$results$simpleEffects)
        mi.initContrastCode(data,self$options,self$results,n64)
        
        note<-self$results$plotnotes
        note$setVisible(FALSE)
        
    },
    .run=function() {
      n64<-private$.names64
      ginfo("run")
      # collect some option
      dep <- self$options$dep
      
      if (is.null(dep))
        return()
      
      factors <- self$options$factors
      covs <- self$options$covs
      clusters<-self$options$cluster
      reml<-self$options$reml
      dfmethod<-self$options$dfmethod
      
      if (self$options$simpleScale=="mean_sd" && self$options$cvalue==0)
          return()
      if (self$options$simpleScale=="percent" && self$options$percvalue==0)
         return()
 
      ###############      
      modelTerms<-self$options$modelTerms
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
      model<- try(private$.estimate(modelFormula, data=data, REML=reml))
      data<-mf.checkData(self$options,data,cluster=clusters[[1]],modelType="mixed")
      if (!is.data.frame(data))
        reject(data)
      if (is.something(covs)) {
        names(data)<-jmvcore::fromB64(names(data))
        private$.cov_condition$storeValues(data)
        names(data)<-jmvcore::toB64(names(data))
        private$.cov_condition$labels_type=self$options$simpleScaleLabels
      }
      ## saving the whole set of results proved to be too heavy for memory issues.
      ## so we estimate the model every time. In case it is not needed, we just trick
      ## the module to believe that the other results are saved, when in reality we
      ## just leave them the way they are :-)
       ginfo("the model has been estimated")
       ##### model ####
       model<- try(private$.estimate(modelFormula, data=data, REML=reml))
       
       mi.check_estimation(model,n64)
       model<-mi.model_check(model)
       private$.model <- model
       self$results$.setModel(model)
       ginfo("...done")

       ### random components data frame ######
       vc<-as.data.frame(lme4::VarCorr(model))
       params<-vc[is.na(vc[,3]),]
       params$var1[is.na(params$var1)]<-""
       grp<-unlist(lapply(params$grp, function(a) gsub("\\.[0-9]$","",a)))
       realgroups<-n64$nicenames(grp)
       realnames<-n64$nicenames(params$var1)
       realnames<-lapply(realnames,lf.nicifyTerms)
       
       ### RE confidence intervals ###
       if (self$options$ciRE==TRUE) {
           ginfo("Estimating CI for RE")
           test<-try({
                 pp<-stats::profile(model,which="theta_",optimizer=model@optinfo$optimizer,prof.scale="varcov")
                 ci<-confint(pp,parm = "theta_",level = self$options$paramCIWidth/100)
                 colnames(ci)<-c("cilow","cihig")
                 params<-cbind(params,ci)
                 })
           if (jmvcore::isError(test)) {
                 randomTable$setNote("reci","Random effects C.I. cannot be computed")
            }
            ginfo("done")
       }
       for (i in 1:dim(params)[1]) {
            if (!is.null(realnames[[i]]) && realnames[[i]]=="(Intercept)")
                   icc<-params$sdcor[i]^2/(params$sdcor[i]^2+params$sdcor[dim(params)[1]]^2)
             else
                   icc<-""
             if (i<=randomTable$rowCount)
                   randomTable$setRow(rowNo=i, list(groups=realgroups[[i]],
                                                    name=realnames[[i]],
                                                    std=params$sdcor[i],
                                                    var=params$vcov[i],
                                                    cilow=params$cilow[i],
                                                    cihig=params$cihig[i],
                                                    icc=icc))
             else
                   randomTable$addRow(rowKey=i, list(groups=realgroups[[i]],
                                                     name=realnames[[i]],
                                                     std=params$sdcor[i],
                                                     var=params$vcov[i],
                                                     cilow=params$cilow[i],
                                                     cihig=params$cihig[i],
                                                     icc=icc))
       }
        N<-as.numeric(model@devcomp$dims['n'])
        groups<-vapply(model@flist,nlevels,1)
        info<-paste("Number of Obs:", N,", groups:",paste(n64$nicenames(names(groups)),groups,collapse = ", "))
        randomTable$setState(list("warning"=info))
        
        ### Covariance among random effects ###
        vcv<-vc[!is.na(vc[,3]),]
        grp<-unlist(lapply(vcv$grp, function(a) gsub("\\.[0-9]$","",a)))
        realgroups<-n64$nicenames(grp)
        realnames1<-lapply(n64$nicenames(vcv$var1),lf.nicifyTerms)
        realnames2<-lapply(n64$nicenames(vcv$var2),lf.nicifyTerms)
        if (dim(vcv)[1]>0) {
             for (i in 1:dim(vcv)[1]) {
                   randomCovTable$addRow(rowKey=realgroups[[i]], list(groups=realgroups[[i]],name1=realnames1[[i]],name2=realnames2[[i]],cov=vcv$sdcor[i]))
             }
            randomCovTable$setVisible(TRUE)
        }
         
               

        
       ### anova results ####
       if (is.null(anovaTable$state)) {
             ginfo("compute the Anova stuff")
             anova_res<-data.frame()
             if (length(modelTerms)==0) {
                  attr(anova_res,"warning")<-"F-Tests cannot be computed without fixed effects"
             } else {
                  suppressWarnings({anova_res <- try(mf.anova(model,df=dfmethod ), silent=TRUE) })
                  mi.check_estimation(anova_res,n64)   
                  if (length(modelTerms)==0) {
                       attr(anova_res,"warning")<-append(attr(anova_res,"warning"),"F-Tests cannot be computed without fixed effects")
                   } else {
                       rawlabels<-rownames(anova_res)
                       labels<-n64$nicenames(rawlabels)
                       for (i in seq_len(dim(anova_res)[1])) {
                           tableRow<-anova_res[i,]  
                           anovaTable$setRow(rowNo=i,tableRow)
                           anovaTable$setRow(rowNo=i,list(name=lf.nicifyTerms(labels[[i]])))
                       }
                       if (attr(anova_res,"statistic")=="Chisq") {
                           attr(anova_res,"warning")<-append(attr(anova_res,"warning"),WARNS["lmer.chisq"])
                           anovaTable$getColumn('test')$setTitle("Chi-squared")
                           anovaTable$getColumn('df1')$setTitle("df")
                           anovaTable$getColumn('df2')$setVisible(FALSE)
                        } else
                           attr(anova_res,"warning")<-append(attr(anova_res,"warning"),paste(attr(anova_res,"method"),"method for degrees of freedom"))
                   }
                   anovaTable$setState(list(warning=attr(anova_res,"warning")))
             }
       } else ginfo("Anova results recycled")
                    
       if (is.null(estimatesTable$state)) {
                 ### coefficients summary results ####
                 parameters<-try(mf.summary(model))
                 ginfo("...done")
                 if (nrow(parameters)>0) {
                  #### confidence intervals ######
                       ciWidth<-self$options$paramCIWidth/100
                       parameters<-mf.confint(model,ciWidth,parameters,method=self$options$cimethod)
                       rownames(parameters)<-n64$nicenames(rownames(parameters))
                  ##### fill the table ############
                       for (i in 1:nrow(parameters)) {
                             tableRow=parameters[i,]
                             estimatesTable$setRow(rowNo=i,tableRow)
                       }
                 }
                 estimatesTable$setState(list(warning=attr(parameters,"warning")))
                 ### prepare info table #########       
                 ginfo("updating the info table")
                 info.call<-n64$translate(as.character(model@call)[[2]])
                 info.title<-paste("Linear mixed model fit by",ifelse(reml,"REML","ML"))
                 info.aic<-round(stats::extractAIC(model)[2],digits=4)
                 info.bic<-round(stats::BIC(model),digits=4)
                 loglik<-lme4::llikAIC(model)
                 info.loglik<-ifelse("logLik" %in% names(loglik),loglik['logLik'],loglik['REML'])
                 info.loglik<-as.numeric(info.loglik)

                 r2<-try(r.squared(model),silent = TRUE)
                 if (jmvcore::isError(r2)){
                   note<-"R-squared cannot be computed."
                   attr(model,"warning")<-append(attr(model,"warning"),note)
                   info.r2m<-NaN        
                   info.r2c<-NaN
                 } else {
                   info.r2m<-r2[[4]]        
                   info.r2c<-r2[[5]]     
                 }
                 infoTable$setRow(rowKey="est", list(value=info.title))
                 infoTable$setRow(rowKey="call",list(value=info.call))
                 infoTable$setRow(rowKey="aic",list(value=info.aic))
                 infoTable$setRow(rowKey="bic",list(value=info.bic))
                 infoTable$setRow(rowKey="log",list(value=info.loglik))
                 infoTable$setRow(rowKey="r2m",list(value=info.r2m))
                 infoTable$setRow(rowKey="r2c",list(value=info.r2c))
                 modelInfo<-attr(model,"infoTable" )
                 conv<-ifelse(modelInfo$conv,"yes","no")
                 infoTable$setRow(rowKey="conv",list(value=conv))
                 if (modelInfo$conv==FALSE)
                   opt<-paste(OPTIMIZERS,collapse=", ")
                 else 
                   opt<-model@optinfo$optimizer
                 infoTable$setRow(rowKey="opt",list(value=opt))
                 ### end of info table ###
                 infoTable$setState(list(warning=attr(model,"warning")))
               } else ginfo("infotable and parameters recycled")
               
        #### LRT for random effects ####
        lrtTable<-self$results$main$lrtRandomEffectsTable
        if (self$options$lrtRandomEffects) {
          .warning=NULL
          ranova_test<-try(as.data.frame(lmerTest::ranova(model)[-1,]))
          if (jmvcore::isError(ranova_test)) {
            message <- jmvcore::extractErrorMessage(ranova_test)
            .warning=paste(message,". LRT cannot be computed")
          } else {
            ranova_test$test<-n64$translate(rownames(ranova_test))
            ranova_test$test<-as.character(ranova_test$test)
            for (i in seq_len(nrow(ranova_test)))
              lrtTable$addRow(rowKey=i,ranova_test[i,])
          }
          lrtTable$setVisible(TRUE)
          lrtTable$setState(list(warning=.warning))
        }
        
        out.infotable_footnotes(infoTable,attr(model,"infoTable"))        
        out.table_notes(infoTable)
        out.table_notes(anovaTable)
        out.table_notes(estimatesTable)
        out.table_notes(randomTable)
        out.table_notes(lrtTable)
        
        private$.preparePlots(model)
        private$.prepareRandHist()
        private$.prepareClusterBoxplot()
        gposthoc.populate(model,self$options,self$results$postHocs)
        gsimple.populate(model,self$options,self$results$simpleEffects,private$.cov_condition)
        gmeans.populate(model,self$options,self$results$emeansTables,private$.cov_condition)
        private$.populateNormTest(model)
        
        mf.savePredRes(self$options,self$results,model) 
        
        
        
    },
  .buildreffects=function(terms,correl=TRUE) {
 
    ## this is for R. It overrides the correlatedEffect option 
    if (length(terms)>1)
         correl<-"block"
    # remove empty sublists
    terms<-terms[sapply(terms, function(a) !is.null(unlist(a)))]
    # split in sublists if option=nocorr
    if (correl=="nocorr") {
      termslist<-terms[[1]]
      terms<-lapply(termslist,list)
    }
    rterms<-""    
    for(i in seq_along(terms)) {
      one<-terms[[i]]
      one64<-lapply(one,jmvcore::toB64)
      flatterms<-lapply(one64,function(x) c(jmvcore::composeTerm(head(x,-1)),tail(x,1)))
      res<-do.call("rbind",flatterms)
      if (length(unique(res[,2]))>1 && correl=="block")
         jmvcore::reject("Correlated random effects by block should have the same cluster variable within each block. Please specify different blocks for random coefficients with different clusters.")
      res<-tapply(res[,1],res[,2],paste)
      res<-sapply(res, function(x) paste(x,collapse = " + "))
      test<-grep(jmvcore::toB64("Intercept"),res,fixed=TRUE)
      if (is.something(test))
        res<-gsub(jmvcore::toB64("Intercept"),1,res)
      else
        res[[1]]<-paste(0,res[[1]],sep = "+")
      form<-paste(res,names(res),sep=" | ")
      form<-paste("(",form,")")
      rterms<-paste(rterms,form,sep = "+")
    }
    rterms<-paste(rterms,collapse = "")
    rterms
  },
  .cleandata=function() {
      Sys.setlocale("LC_NUMERIC", "C")
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
          warning(paste("Warning, variable",factor," has been coerced to factor"))
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
      
      for (opt in OPTIMIZERS) {
          ctr=lme4::lmerControl(optimizer = opt)
          lm = do.call(lmerTest::lmer, list(formula=form, data=data,REML=REML,control=ctr))
          model<-mi.model_check(lm)
          info<-attr(model,"infoTable")
          if (info$conv==TRUE)
             break()
      }

      ## set info for R refit ###
      attr(model,"refit")<-list(lib="lme4",
                                command="lmer",
                                coptions=list(formula=private$.names64$translate(form),REML=REML),
                                eoptions=list(formula=private$.names64$translate(form),REML=REML,control=ctr))
      return(model)
    },
    .modelFormula=function() {
      
      if (!is.something(unlist(self$options$randomTerms))) 
         return(FALSE)

      if (!is.null(self$options$dep))  {
        dep<-jmvcore::toB64(self$options$dep)
      } else return(FALSE)
      
      rands<-self$options$randomTerms
      
      rands<-private$.buildreffects(rands,self$options$correlatedEffects)

      modelTerms64<-sapply(self$options$modelTerms,jmvcore::toB64)
      fixed<-lf.constructFormula(dep,modelTerms64,self$options$fixedIntercept)
      mf<-paste(fixed,rands,sep =  "")
      mf
    },


.preparePlots=function(model) {
  
  depName <- self$options$dep
  dep64 <- jmvcore::toB64(depName)
  groupName <- self$options$plotHAxis
  groupName64 <- jmvcore::toB64(groupName)
  
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
  ### this is specific of mixed model #####
  preds<-c(groupName,linesName,plotsName)
  preds64<-jmvcore::toB64(preds)
  clusters<-jmvcore::toB64(self$options$cluster)
  cluster<-clusters[which(clusters %in% names(model@cnms))][[1]]
  
  preds64<-c(cluster,preds64)
  if (self$options$plotRandomEffects) {
    
    data<-model@frame
    # here we set all model predictors but the x-axis variable to zero
    # to smooth random effects predicted values
    mvars<-names(data)
    tozero<-setdiff(mvars,c(groupName64,clusters,dep64))
    newdata<-data
    toaggregate<-list()
    for(v in tozero)
      if (!is.factor(newdata[,v])) {
        center<-mean(newdata[,v])
        newdata[,v]<-center
      } else {
        d<-dim(contrasts(newdata[,v]))
        contrasts(newdata[,v])<-matrix(0,d[1],d[2])
      }
    pd<-stats::predict(model,type="response",newdata=newdata,allow.new.levels=TRUE)
    
    # end of zeroing 
    
    randomData<-as.data.frame(cbind(pd,data[,preds64]))
    pnames<-c("cluster","group","lines","plots")
    names(randomData)<-c("y",pnames[1:length(preds64)])
    note<-self$results$plotnotes
    note$setContent(paste('<i>Note</i>: Random effects are plotted by',jmvcore::fromB64(cluster)))
    note$setVisible(TRUE)
  } else
    randomData<-NULL
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
  
  ### this for mixed only ###
  if (!is.null(randomData)) {
             yAxisRange[which.max(yAxisRange)]<-max(max(randomData$y),max(yAxisRange))
             yAxisRange[which.min(yAxisRange)]<-min(min(randomData$y),min(yAxisRange))
    }
  
  gplots.images(self,data=predData,raw=rawData,range=yAxisRange,randomData=randomData)
  
  ####### random effect plots #########


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
  order<-1

  if (self$options$plotRandomEffects) {
       mterms<-self$options$modelTerms
       forder<-max(sapply(mterms[grep(groupName,mterms)],function(x) length(grep(groupName,x))))
       mterms<-self$options$randomTerms
       rorder<-1
       if (is.something(grep(groupName,mterms)))
           rorder<-max(sapply(mterms[grep(groupName,mterms)],function(x) length(grep(groupName,x))))
       order<-max(forder,rorder)
  }
  if (errorType=="ci")
    errorType<-paste0(ciWidth,"% ",toupper(errorType))
  
  if ( ! is.null(linesName)) {
    p<-gplots.twoWaysPlot(image,ggtheme,depName,groupName,linesName,errorType,order=order)
  } else {
    p<-gplots.oneWayPlot(image,ggtheme,depName,groupName,errorType,order=order)
  }       
  return(p)
},


.qqPlot=function(image, ggtheme, theme, ...) {

  if (!self$options$qq)
    return()
  
  model<-private$.model      
  if (!is.something(model) )
    return(FALSE)
  
  residuals <- as.numeric(scale(residuals(model)))
  df <- as.data.frame(qqnorm(residuals, plot.it=FALSE))
  plot<-ggplot2::ggplot(data=df, aes(y=y, x=x)) +
          geom_abline(slope=1, intercept=0, colour=theme$color[1]) +
          geom_point(aes(x=x,y=y), size=2, colour=theme$color[1]) +
          xlab("Theoretical Quantiles") +
          ylab("Standardized Residuals") +ggtheme
  
  plot
},
.normPlot=function(image, ggtheme, theme, ...) {
  if (!self$options$normPlot)
    return()
  model<-private$.model      
  if (!is.something(model) )
    return(FALSE)
  plot<-gplots.normPlot(model,ggtheme,theme)
  return(plot)
},

.residPlot=function(image, ggtheme, theme, ...) {

  if (!self$options$residPlot)
    return()
  model<-private$.model      
  if (!is.something(model) )
    return(FALSE)
  plot<-gplots.residPlot(model,ggtheme,theme)
  
  return(plot)
},

.prepareClusterBoxplot=function() {
  
  if (!self$options$clusterBoxplot)
    return()
  
  model<-private$.model
  if (!is.something(model))
    return()
  n64<-private$.names64
  clusters64<-names(model@cnms)
  image<-self$results$assumptions$clusterBoxplot
  for (cluster in clusters64) {
    label<-n64$nicenames(cluster)
    title<-paste("Clustering variable:",jmvcore::fromB64(cluster))
    id<-cluster
    image$addItem(id)
    image$get(key=id)$setTitle(title)
    image$get(key=id)$setState(list(cluster=cluster,label=label))
    }
},


.clusterBoxplot=function(image, ggtheme, theme, ...) {
  
  if (!self$options$clusterBoxplot)
    return()
  
  model<-private$.model      
  if (!is.something(model) )
    return(FALSE)
  label<-image$state$label
  cluster<-image$state$cluster
  fmodel<-lme4::fortify.merMod(model)
  plot<-ggplot(fmodel, aes_string(cluster,".resid")) + geom_boxplot() + coord_flip()
  plot<-plot+xlab(jmvcore::fromB64(cluster))+ylab("Residuals")
  plot<-plot+ ggtheme 
  note<-self$results$plotnotes
  note$setContent(paste('<i>Note</i>: Residuals plotted by',jmvcore::fromB64(cluster)))
  note$setVisible(TRUE)
  
  return(plot)
},

.prepareRandHist=function() {
  
  if (!self$options$randHist)
    return()
  
  model<-private$.model
  if ((!self$options$randHist) | !is.something(model))
     return()
  n64<-private$.names64
  res<-lme4::ranef(model)
  clusters64<-names(res)
  image<-self$results$assumptions$randHist
  for (cluster in clusters64) {
       clusterres<-res[[cluster]]
       vars<-names(clusterres)
       for (v in vars) {
           data<-data.frame(clusterres[,v])
           names(data)<-"x"
           label<-n64$nicenames(v)
           title<-paste("Coefficient",label," random across",jmvcore::fromB64(cluster))
           id<-paste0(v,cluster)
           image$addItem(id)
           image$get(key=id)$setTitle(title)
           image$get(key=id)$setState(list(data=data,label=label))
       }

  }


},

.randHist=function(image, ggtheme, theme, ...) {

  if (!self$options$randHist)
    return()
  
  label<-image$state$label
  data<-image$state$data
  fill <- theme$fill[2]
  color <- theme$color[1]
  alpha <- 0.4
  plot <- ggplot(data=data, aes(x=x)) +
    labs(x="Coefficients", y='density')
  
  plot <- plot + geom_histogram(aes(y=..density..), position="identity",
                                stat="bin", color=color, fill=fill)
  plot <- plot + stat_function(fun = dnorm, args = list(mean = mean(data$x), sd = sd(data$x)))  
  
  themeSpec <- theme(axis.text.y=element_blank(),
                     axis.ticks.y=element_blank())
  plot <- plot + ggtheme + themeSpec
  
  
  return(plot)
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


.marshalFormula= function(formula, data, name) {
  
      fixed<-lme4::nobars(formula)
      bars<-lme4::findbars(formula)
      rterms<-lapply(bars,all.vars)
      rvars<-unlist(sapply(rterms,function(a) if (length(a)>1) a[[length(a)-1]]))
      if (name=="dep")
        return(jmvcore::marshalFormula(fixed,data,from = "lhs"))  
      if (name=="factors") {
        ffactors<-jmvcore::marshalFormula(fixed,data,from='rhs',type='vars',permitted='factor')
        rfactors<-unlist(lapply(rvars, function(a) {if (is.factor(data[[a]])) a}))
        return(c(ffactors,rfactors))
      }
      if (name=="covs") {
        fcovs<-jmvcore::marshalFormula(fixed,data,from='rhs',type='vars',permitted='numeric')
        rcovs<-unlist(lapply(rvars, function(a) {if (is.numeric(data[[a]])) a}))
        return(c(fcovs,rcovs))
      }
      if (name=="cluster") {
         return(sapply(rterms,function(a) a[[length(a)]] ))
      }
      if (name=="randomTerms") {
        bars<-lme4::findbars(formula)
        fullist<-list()
        for (b in seq_along(bars)) {
          cluster=bars[[b]][[3]]
          bar<-strsplit(as.character(bars[[b]])[[2]],"+",fixed=T)[[1]]
          barlist<-list()
          j<-0
          for (term in bar) {
            term<-trimws(jmvcore::decomposeTerm(term))
            if (length(term)==1 && term=="1")
                   term="Intercept"
            if (length(term)==1 && term=="0")
                next()              
            alist<-c(term,as.character(cluster))
            j<-j+1
            barlist[[j]]<-alist
          }
          fullist[[b]]<-barlist
        }
        return(fullist)
      }
      
      if (name=="modelTerms") {
        return(jmvcore::marshalFormula(fixed,data,from='rhs',type='terms'))
      }
      
},


.formula = function() {

  private$.names64$translate(private$.modelFormula())
  
},
.sourcifyOption = function(option) {
  
  name <- option$name
  value <- option$value
  
  if (!is.something(value))
    return('')

  
  if (option$name %in% c('factors', 'dep', 'covs', 'cluster', 'modelTerms','randomTerms'))
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
))

jmvcore::sourcify
