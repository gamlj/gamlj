
gamljGlmMixedClass <- R6::R6Class(
  "gamljGlmMixedClass",
  inherit=gamljGlmMixedBase,
  private=list(
    .model=NA,
    .names64=NA,
    .postHocRows=NA,
    .init=function() {
      ginfo("init")
#       class(private$.results) <- c('gamlj', class(private$.results))
#       private$.names64<-names64$new()
#       n64<-private$.names64
#       infoTable<-self$results$info
#       dep<-self$options$dep
#       modelTerms<-self$options$modelTerms
#       factors<-self$options$factors
#       covs<-self$options$covs
#       fixedIntercept<-self$options$fixedIntercept
#       modelType<-self$options$modeltype
#       afamily<-mf.give_family(modelType,self$options$custom_family,self$options$custom_link)
#       ciWidth<-self$options$paramCIWidth
#       
#       
#             
#       getout<-FALSE
#       if (is.null(dep)) {
#         infoTable$addRow(rowKey="gs1",list(info="Get started",value="Select the dependent variable"))
#         getout<-TRUE
#       }
#       if (is.null(self$options$cluster)) {
#         infoTable$addRow(rowKey="gs2",list(info="Get started",value="Select at least one cluster variable"))
#         getout=TRUE
#       }
#       if (!is.something(unlist(self$options$randomTerms))) {
#         infoTable$addRow(rowKey="gs3",list(info="Get started",value="Select at least one term in Random Effects"))
#         getout=TRUE
#       }
#       
#       aOne<-which(unlist(modelTerms)=="1")
#       if (is.something(aOne)) {
#         modelTerms[[aOne]]<-NULL
#         fixedIntercept=TRUE
#       }
#       
# #      if (length(modelTerms) == 0 && fixedIntercept==FALSE) {
# #        if (is.null(factors) && is.null(covs))
# #          infoTable$addRow(rowKey="gs4",list(info="Optional",value="Select factors and covariates"))
# #        else
# #          jmvcore::reject("Please specify a model")            
# #        getout<-TRUE
# #      }
#       
#       if (getout) 
#         return(FALSE)
#       data<-private$.cleandata()
#       data<-mf.checkData(self$options,data,modelType=modelType)
#       
#       if (!is.data.frame(data))
#         jmvcore::reject(data)
#       
#       
#       ### initialize conditioning of covariates
#       if (is.something(self$options$covs)) {
#       span<-ifelse(self$options$simpleScale=="mean_sd",self$options$cvalue,self$options$percvalue)
#       private$.cov_condition<-conditioning$new(self$options$covs,self$options$simpleScale,span)
#       }
#       #####################
#       #### info table #####
#       
#       modelFormula<-lf.constructFormula(dep,modelTerms,self$options$fixedIntercept)
#       infoTable<-self$results$info
#       info<-MINFO[[modelType]]
#       info$link<-LINFO[[afamily$link]]
#       info$distribution<-DINFO[[afamily$family]]
#       infoTable$addRow(rowKey="mod",list(info="Model Type",value=info$name[[1]],comm=info$name[[2]]))
#       infoTable$addRow(rowKey="call",list(info="Call",comm=n64$translate(modelFormula),value=info$call))
#       infoTable$addRow(rowKey="link",list(info="Link function",value=info$link[[1]],comm=info$link[[2]]))
#       ep<-mi.explainPrediction(modelType,data,dep)
#       if (!is.null(ep))
#         infoTable$addRow(rowKey="dir",list(info="Direction",value=ep[1],comm=""))
#       infoTable$addRow(rowKey="family",list(info="Distribution",value=info$distribution[[1]],comm=info$distribution[[2]]))
#       infoTable$addRow(rowKey="log",list(info="LogLikel.",comm="Unconditional Log-Likelihood"))
#       infoTable$addRow(rowKey="2log",list(info="-2*LogLikel.",comm="Unconditional absolute deviance"))
#       infoTable$addRow(rowKey="dev",list(info="Deviance",comm="Conditional relative deviance"))
#       infoTable$addRow(rowKey="r2m",list(info="R-squared",comm="Marginal"))
#       infoTable$addRow(rowKey="r2c",list(info="R-squared",comm= "Conditional"))
#       infoTable$addRow(rowKey="aic",list(info="AIC",comm="Less is better"))
#       infoTable$addRow(rowKey="bic",list(info="BIC",comm="Less is better"))
#       infoTable$addRow(rowKey="resdf",list(info="Residual DF",comm=""))
#       infoTable$addRow(rowKey="valdf",list(info="Chi-squared/DF",comm="Overdispersion indicator"))
#       infoTable$addRow(rowKey="conv",list(info="Converged",comm=""))
#       infoTable$addRow(rowKey="opt",list(info="Optimizer",comm=""))
#       
# 
#       if ("note" %in% names(info))
#         infoTable$addRow(rowKey="note",list(info="Note",value=info$note[[1]],comm=info$note[[2]]))
#       
#       
#       
#       
#       ## random table
#       aTable<-self$results$main$random
#       if (self$options$ciRE==TRUE) {
#         aTable$getColumn('cilow')$setSuperTitle(jmvcore::format('Variance {}% C.I.', ciWidth))
#         aTable$getColumn('cihig')$setSuperTitle(jmvcore::format('Variance {}% C.I.', ciWidth))
#       }
#       
#       
#       
#       ## anova Table 
#       if (length(modelTerms)>0) {
#         aTable<- self$results$main$anova
#         for (i in seq_along(modelTerms)) {
#            lab<-jmvcore::stringifyTerm(modelTerms[[i]],raise=T)
#            aTable$addRow(rowKey=i, list(name=lab))
#         }
#       }
#       
# 
#       ## fixed effects parameters
#       modelFormula<-lf.constructFormula(dep,modelTerms,self$options$fixedIntercept)
#       
#       aTable<-self$results$main$fixed
#       dep64<-jmvcore::toB64(dep)
#       modelTerms64<-lapply(modelTerms,jmvcore::toB64)
#       formula64<-as.formula(lf.constructFormula(dep64,modelTerms64,self$options$fixedIntercept))
#       mynames64<-colnames(model.matrix(formula64,data))
#       terms<-n64$nicenames(mynames64)  
#       labels<-n64$nicelabels(mynames64)
#       aTable$getColumn('cilow')$setSuperTitle(jmvcore::format('{}% Confidence Interval', ciWidth))
#       aTable$getColumn('cihig')$setSuperTitle(jmvcore::format('{}% Confidence Interval', ciWidth))
#       aTable$getColumn('ecilow')$setSuperTitle(jmvcore::format('{}% Exp(B) Confidence Interval', ciWidth))
#       aTable$getColumn('ecihig')$setSuperTitle(jmvcore::format('{}% Exp(B) Confidence Interval', ciWidth))
#       
#       for(i in seq_along(terms)) 
#           aTable$addRow(rowKey=i,list(source=lf.nicifyTerms(terms[[i]]),label=lf.nicifyLabels(labels[[i]])))
#       
#       if (!is.something(self$options$factors))
#            aTable$getColumn('label')$setVisible(FALSE)
#       
#         # other inits
# #        gplots.initPlots(self,data,private$.cov_condition)
#         gposthoc.init(data,self$options, self$results$postHocs)     
# #        gmeans.init(data,self$options,self$results$emeansTables,private$.cov_condition)
#         gsimple.init(data,self$options,self$results$simpleEffects)
#         mi.initContrastCode(data,self$options,self$results,n64)
# 
#         # some cleaning
#         note<-self$results$plotnotes
#         note$setVisible(FALSE)
        
        
        
    },
    .run=function() {
      # n64<-private$.names64
      # ginfo("run")
      # # collect some option
      # dep <- self$options$dep
      # modelType<-self$options$modeltype
      # if (is.null(dep))
      #   return()
      # 
      # factors <- self$options$factors
      # covs <- self$options$covs
      # clusters<-self$options$cluster
      # if (self$options$simpleScale=="mean_sd" && self$options$cvalue==0)
      #     return()
      # if (self$options$simpleScale=="percent" && self$options$percvalue==0)
      #    return()
      # 
      # ###############      
      # modelTerms<-self$options$modelTerms
      # modelFormula<-private$.modelFormula()
      # if (modelFormula==FALSE)  
      #     return()
      # 
      # 
      # ### collect the tables #######
      # infoTable<-self$results$info
      # estimatesTable <- self$results$main$fixed
      # randomTable <- self$results$main$random
      # randomCovTable<-self$results$main$randomCov
      # anovaTable<-self$results$main$anova
      # 
      # ##### clean the data ####
      # data<-private$.cleandata()
      # data<-mf.checkData(self$options,data,cluster=clusters[[1]],modelType=modelType)
      # if (!is.data.frame(data))
      #   jmvcore::reject(data)
      # 
      # if (is.something(covs)) {
      #   names(data)<-jmvcore::fromB64(names(data))
      #   private$.cov_condition$storeValues(data)
      #   names(data)<-jmvcore::toB64(names(data))
      #   private$.cov_condition$labels_type=self$options$simpleScaleLabels
      # }
      # 
      # ## saving the whole set of results proved to be too heavy for memory issues.
      # ## so we estimate the model every time. In case it is not needed, we just trick
      # ## the module to believe that the other results are saved, when in reality we
      # ## just leave them the way they are :-)
      # 
      #  ##### model ####
      #  ## we estimate it every time
      #  ginfo("the model is being estimated")
      #  
      #  model<- try(private$.estimate(modelFormula, data=data))
      #  mi.check_estimation(model,n64)
      #  model<-mi.model_check(model)
      #  private$.model <- model
      #  ginfo("...done")
      #  ###### variances ###########
      #  vc<-as.data.frame(lme4::VarCorr(model))
      #  params<-vc[is.na(vc[,3]),]
      #  params$var1[is.na(params$var1)]<-""
      #  .sigma<-sigma(model)
      #  .sigma2<-sigma(model)^2
      #   grp<-unlist(lapply(params$grp, function(a) gsub("\\.[0-9]$","",a)))
      #   realgroups<-n64$nicenames(grp)
      #   realnames<-n64$nicenames(params$var1)
      #   realnames<-lapply(realnames,lf.nicifyTerms)
      #   r2<-try(r.squared(model),silent = TRUE)
      #   
      #   ### RE confidence intervals ###
      #   if (self$options$ciRE==TRUE) {
      #     ginfo("Estimating CI for RE")
      #     test<-try({
      #       pp<-stats::profile(model,which="theta_",optimizer=model@optinfo$optimizer,prof.scale="varcov")
      #       ci<-confint(pp,parm = "theta_",level = self$options$paramCIWidth/100)
      #       colnames(ci)<-c("cilow","cihig")
      #       params<-cbind(params,ci)
      #     })
      #     if (jmvcore::isError(test)) {
      #       randomTable$setNote("reci","Random effects C.I. cannot be computed")
      #     }
      #     ginfo("done")
      #   }
      #   
      #   
      #   for (i in 1:dim(params)[1]) {
      #          icc<-""
      #        if (!is.null(realnames[[i]]) && realnames[[i]]=="(Intercept)") {
      #            if (!jmvcore::isError(r2)) {
      #              icc<-params$sdcor[i]^2/(params$sdcor[i]^2+r2$varDist)
      #            }
      #          }
      # 
      #     
      #          if (i<=randomTable$rowCount)
      #              randomTable$setRow(rowNo=i, list(groups=realgroups[[i]],
      #                                               name=realnames[[i]],
      #                                               std=params$sdcor[i],
      #                                               var=params$vcov[i],
      #                                               cilow=params$cilow[i],
      #                                               cihig=params$cihig[i],
      #                                               icc=icc))
      #            else
      #              randomTable$addRow(rowKey=i, list(groups=realgroups[[i]],
      #                                                name=realnames[[i]],
      #                                                std=params$sdcor[i],
      #                                                var=params$vcov[i],
      #                                                cilow=params$cilow[i],
      #                                                cihig=params$cihig[i],
      #                                                icc=icc))
      #   }
      #                                                
      #          if (!("Residuals" %in% grp))
      #            randomTable$addRow(rowKey=i+1, list(groups="Residuals",name="",std=.sigma,var=.sigma2))
      # 
      #   N<-as.numeric(model@devcomp$dims['n'])
      #   groups<-vapply(model@flist,nlevels,1)
      #   info<-paste("Number of Obs:", N,", groups:",paste(n64$nicenames(names(groups)),groups,collapse = ", "))
      #   randomTable$setState(list("warning"=info))
      #   
      #    ### Covariance among random effects ###
      #    vcv<-vc[!is.na(vc[,3]),]
      #    grp<-unlist(lapply(vcv$grp, function(a) gsub("\\.[0-9]$","",a)))
      #    realgroups<-n64$nicenames(grp)
      #    realnames1<-lapply(n64$nicenames(vcv$var1),lf.nicifyTerms)
      #    realnames2<-lapply(n64$nicenames(vcv$var2),lf.nicifyTerms)
      #    if (dim(vcv)[1]>0) {
      #            for (i in 1:dim(vcv)[1]) {
      #              randomCovTable$addRow(rowKey=realgroups[[i]], list(groups=realgroups[[i]],name1=realnames1[[i]],name2=realnames2[[i]],cov=vcv$sdcor[i]))
      #            }
      #            randomCovTable$setVisible(TRUE)
      #    }
      #          
      #          
      #   ### anova results ####
      # 
      #   if (is.null(anovaTable$state)) {
      #     ### anova results ####
      #     anova_res<-data.frame()
      #     if (length(modelTerms)==0) {
      #       attr(anova_res,"warning")<-"Omnisbus Tests are not computed without fixed effects"
      #     } else {
      #       suppressWarnings({anova_res <- try(mf.anova(model)) })
      #       err<-mi.warn_estimation(anova_res,n64)
      #       if (is.something(err))
      #         attr(anova_res,"warning")<-"Omnisbus Tests cannot be computed"
      #       else {
      #         out.fillTable(anovaTable,anova_res)
      #       }
      #     }
      #   
      #     anovaTable$setState(list(warning=attr(anova_res,"warning"))) 
      #     ####### end of anova table ###########
      #       
      #   } else ginfo("Anova results recycled")
      # 
      #    if (is.null(estimatesTable$state)) {
      #      self$results$.setModel(model)
      #      ginfo("Parameters have been estimated")
      #      ### coefficients summary results ####
      #      parameters<-try(mf.summary(model))
      #      mi.check_estimation(parameters,n64)
      #      
      #      #### confidence intervals ######
      #      ciWidth<-self$options$paramCIWidth/100
      #      parameters<-mf.confint(model,level=ciWidth,parameters,method=self$options$cimethod)
      #      out.fillTable(estimatesTable,parameters)        
      #      estimatesTable$setState(attributes(parameters))
      #    
      #    
      # 
      #          ### prepare info table #########       
      #      ginfo("updating the info table")
      #      ep<-mi.explainPrediction(modelType,data,dep)
      #      if (!is.null(ep))
      #        infoTable$setRow(rowKey="dir",list(comm=ep[2]))
      #      
      #      info.call<-n64$translate(as.character(model@call)[[2]])
      #      info.title<-paste("Generalized mixed model" )
      #      info.aic<-round(stats::extractAIC(model)[2],digits=2)
      #      info.loglik<-lme4::llikAIC(model)$AICtab['logLik']
      #      info.2log<--2*info.loglik
      #      info.bic<-stats::BIC(model)
      #      info.dev<-stats::deviance(model)
      #      if (jmvcore::isError(r2)){
      #              note<-"R-squared cannot be computed."
      #              info.r2m<-NaN        
      #              info.r2c<-NaN
      #              infoTable$setNote("r2",note)  
      #      } else {
      #              info.r2m<-r2[[4]]        
      #              info.r2c<-r2[[5]]     
      #      }
      #      infoTable$setRow(rowKey="call",list(comm=info.call))
      #      infoTable$setRow(rowKey="aic",list(value=info.aic))
      #      infoTable$setRow(rowKey="bic",list(value=info.bic))
      #      infoTable$setRow(rowKey="log",list(value=info.loglik))
      #      infoTable$setRow(rowKey="2log",list(value=info.2log))
      #      infoTable$setRow(rowKey="dev",list(value=info.dev))
      #      infoTable$setRow(rowKey="r2m",list(value=info.r2m))
      #      infoTable$setRow(rowKey="r2c",list(value=info.r2c))
      #      infoTable$setRow(rowKey="resdf",list(value=mi.getResDf(model)))
      #      infoTable$setRow(rowKey="valdf",list(value=mi.getValueDf(model)))
      #      
      #      modelInfo<-attr(model,"infoTable" )
      #      if (modelInfo$conv==FALSE) {
      #        opt<-paste(OPTIMIZERS,collapse=", ")
      #        conv="no"
      #      } else { 
      #        opt<-model@optinfo$optimizer
      #        conv="yes"
      #      }
      #      infoTable$setRow(rowKey="opt",list(value=opt))
      #      infoTable$setRow(rowKey="conv",list(value=conv))
      #      infoTable$setState(list(warning=attr(model,"warning")))
      #      ### end of info table ###
      #    } ## end of estimate and info calculation
      #    
      #   out.infotable_footnotes(infoTable,attr(model,"infoTable"))        
      #   out.table_notes(infoTable)
      #   out.table_notes(anovaTable)
      #   out.table_notes(estimatesTable)
      #   out.table_notes(randomTable)
      #    
      # 
      #   private$.preparePlots(private$.model)
      #   gposthoc.populate(model,self$options,self$results$postHocs)
      #   gsimple.populate(model,self$options,self$results$simpleEffects,private$.cov_condition)
      #   gmeans.populate(model,self$options,self$results$emeansTables,private$.cov_condition)
      #   
      #   mf.savePredRes(self$options,self$results,model) 
      #   

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
