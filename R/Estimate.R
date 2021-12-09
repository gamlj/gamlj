## This class takes care of estimating the model and return the results. It inherit from Syntax, and defines the same tables
## defined by Syntax, but it fill them with the results. It also adds a few tables not defined in Syntax

## Any function that produce a table goes here

Estimate <- R6::R6Class("Estimate",
                        inherit = Syntax,
                        cloneable=FALSE,
                        class=TRUE,
                        public=list(
                          subclass=NULL,
                          ciwidth=NULL,
                          model=NULL,
                          tab_simpleAnova=NULL,
                          tab_simpleCoefficients=NULL,
                          
                          initialize=function(options,dispatcher,datamatic) {
                            super$initialize(options,dispatcher,datamatic)
                            self$ciwidth <- options$ciWidth/100
                            self$subclass<-paste0("model_",options$modelSelection)
                          },
                          estimate = function(data) {
                            self$model<-private$.estimateModel(data)
                            ginfo("Initial estimation is done...")
                            
                          }, # end of publich function estimate

                          ##### fill the tables #####
                          
                          run_info=function() {
                               
                                tab<-self$init_info()
                                tab[["sample"]]$value<-self$datamatic$N
                            
                          ## TODO: generalize if other models need an optimizer other than lmer
                                if (isTRUE(self$infomatic$optimized))
                                    tab[["optim"]]$value<-self$model@optinfo$optimizer
                          
                                tab[["conv"]]$value<-ifelse(mf.converged(self$model),"yes","no")
                                tab
                          },                    

                          run_main_anova=function() {
                            
                            if (!self$isProper) 
                                self$dispatcher$warnings<-list(topic="main_anova",message=WARNS["glm.zeromodel"])
                            mf.anova(self$model,self)
                              
                          },
                          
                          run_main_r2=function() {

                              fit.R2(self$model)
                          },
                          
                          run_main_coefficients=function() {
                            
                            tab<-NULL
                            if (self$isProper) {
                              tab       <-  mf.parameters(self$model,self)
                              tab       <-  private$.fix_names(tab)
                              
                            }
                            tab
                            
                          },
                          ### anova effect sizes ####
                          
                          run_main_effectsizes=function()  {
                            
                            es.glm_variances(self$model,self$ciwidth)
                            
                          },
                          
                          run_main_intercept=function() {
                            
                              ss<-summary(self$model)
                              tt<-ss$coefficients[1,3]
                              f<-tt^2
                              df<-stats::df.residual(self$model)
                              p<-ss$coefficients[1,4]
                              etap<-effectsize::t_to_eta2(tt,df_error = df)
                              omegap<-effectsize::t_to_omega2(tt,df_error = df)
                              epsilonp<-effectsize::t_to_epsilon2(tt,df_error = df)
                              tab<-list(list(source="(Intercept)",
                                             df=df,
                                             f=f,
                                             etaSqP=etap[1,1],
                                             omegaSqP=omegap[1,1],
                                             epsilonSqP=epsilonp[1,1],
                                             p=p))
                               tab
                          },
                          run_posthoc=function() {
                              
                            procedure.posthoc(self)
                              
                          },
                          run_emmeans=function() {
                              procedure.emmeans(self)
                          },
                          
                          run_simpleEffects_anova=function() {
                            
                            if (is.null(self$tab_simpleAnova))
                                private$.estimateSimpleEffects()
                            warnings("this is direct to simple anova")
                            self$tab_simpleAnova
                          },
                          
                          run_simpleEffects_coefficients=function() {
                            
                            if (is.null(self$tab_simpleCoefficients))
                              private$.estimateSimpleEffects()
                            
                            self$dispatcher$warnings<-list(topic="simpleEffects_anova",message="indirect simple")
                            
                            self$tab_simpleCoefficients
                          },
                          
                          run_simpleInteractions=function() {
                            
                            procedure.simpleInteractions(self)
                          },
                          
                          run_assumptions_homotest=function() {

                                data<-self$model$model
                                data$res<-residuals(self$model)
                                factors <-   names(attr(stats::model.matrix(self$model),"contrasts"))
                                if (is.null(factors))
                                  return()
                                rhs <- paste0('`', factors, '`', collapse=':')
                                formula <- as.formula(paste0('`res`~', rhs))
                                result <- car::leveneTest(formula, data, center="mean")
                                table<-data.frame(name=c("Levene's Test","Breusch-Pagan Test"))
                                table$test[1]=result[1,'F value']
                                table$df1[1]=result[1,'Df']
                                table$df2[1]=result[2,'Df']
                                table$p[1]=result[1,'Pr(>F)']
                                ## Breusch-Pagan ##
                                test<-lmtest::bptest(self$model)
                                table$test[2]<-test$statistic
                                table$df1[2]<-test$parameter
                                table$df2[2]<-""
                                table$p[2]<-test$p.value
                               table
                          },
                          run_assumptions_normtest=function() {
                           
                            table<-data.frame(name=c("Kolmogorov-Smirnov","Shapiro-Wilk"),test=c(NaN,NaN),p=c(NaN,NaN))
                            rr<-residuals(self$model)
                            ks<-ks.test(rr,"pnorm",mean(rr),sd(rr))
                            table$test[1]=ks$statistic
                            table$p[1]=ks$p.value
                            
                            st<-try_hard(shapiro.test(rr))
                            if (!isFALSE(st$error)) {
                              warning("Shapiro-Wilk not available due to too large number of cases")
                            }
                            else {
                              table$test[2]=st$obj$statistic
                              table$p[2]=st$obj$p.value 
                            }
                            mark(table)
                            table
                          },
                          
                          savePredRes=function(results) {
                            
                            if (self$options$predicted && results$predicted$isNotFilled()) {
                                ginfo("Saving predicted")
                                p<-stats::predict(self$model,type=self$infomatic$predict)
                              # we need the rownames in case there are missing in the datasheet
                                pdf <- data.frame(predicted=p, row.names=rownames(mf.getModelData(self$model)))
                                results$predicted$setValues(p)
                            }
                            if (self$options$residuals && results$residuals$isNotFilled()) {
                                ginfo("Saving residuals")
                                p<-stats::resid(self$model)
                              # we need the rownames in case there are missing in the datasheet
                              pdf <- data.frame(residuals=p, row.names=rownames(mf.getModelData(self$model)))
                              results$residuals$setValues(pdf)
                            }
                          },
                          #### we need this here because emmeans needs a contrast that
                          ###  we can control in terms of variable type
                          
                          interaction_contrast=function(levels,datamatic=NULL) {
                            
                            nvar<-length(datamatic)
                            private$.contr_index<-private$.contr_index+1
                            i<-private$.contr_index
                            var <-datamatic[[i]]
                            if (var$type=="factor")
                                contrast<-var$contrast_values
                            else 
                                contrast<-c(-.5,.5)
                            contrast<-as.data.frame(MASS::ginv(t(contrast)))
                            if (private$.contr_index==nvar)
                                  private$.contr_index<-0

                            if (var$type=="factor")
                                      names(contrast)<-paste0("(",gsub(" ","",var$contrast_labels),")")

                            else
                              names(contrast)<-var$name

                            return(contrast)
                          }
                          
                          
                          ),# end of public
                        active=list(
                          
                          
                          storage=function(anobj) {
                            
                            if (missing(anobj)) {
                              ginfo("retreiving the model")
                              return(private$.storageTable$state)
                            }
                            else {
                              ginfo("setting the model")
                              
                              private$.storageTable$setState(anobj)
                              private$.hasStorage<-TRUE
                              ginfo("done")
                            }
                            
                            
                            
                          }

                        ), #end of active
                        privat=list(
                          .data64=NULL,
                          .contr_index=0,
                          .storageTable=NULL,
                          .hasStorage=FALSE,
                          .estimateModel=function(data) {
                              ### check the dependent variable ####
                              if (is.something(self$datamatic$errors))
                                  stop(unlist(self$datamatic$errors))
                            
                              if (!(self$datamatic$dep$type %in% self$infomatic$deptype)) {
                                    t2  <-  paste(self$infomatic$deptype,collapse = " or ")
                                    t1  <-  self$datamatic$dep$type
                                    m   <-   self$infomatic$model[1]
                                    msg<-paste("Dependent variable is of type",t1,".",m,"requires variable of type",t2)
                                    stop(msg)
                              }

                            ### when necessary, check the number of levels
                            if (is.something(self$infomatic$depnlevels)) {
                                 nvar  <-  self$datamatic$dep$nlevels
                                 nreq  <-  self$infomatic$depnlevels
                                 if ( nreq > 0 ) {
                                       if ( nreq !=  nvar)
                                            stop(paste(self$infomatic$model[1],"requires exactly",nreq,"levels"))
                                 } else {                               
                                        if (nvar < abs(nreq) )
                                               stop(paste(self$infomatic$model[1],"requires a minimum of",abs(nreq),"levels"))
                                 }
                            }
                            
                            ## check random effects for mixed
                            
                            if (self$infomatic$caller=="lmer") {
                              terms<-setdiff(unlist(unlist(self$options$randomTerms)),unlist(c("Intercept",self$options$cluster)))
                              for (t in terms) {
                                 if(self$datamatic$variables[[tob64(t)]]$isBetween)
                                     self$dispatcher$warnings<-list(topic="info",message=paste("Variable",t,"does not seem to vary across clusters but its effects are set random."))
                              }

                          }
                             ### end of checks ###
                            
                              opts    <-  opts<-list(str2lang(self$infomatic$rcall))

                              opts[["formula"]]<-self$formula64
                              
                              if (is.something(self$infomatic$family))
                                          opts[["family"]]<-self$infomatic$family    
                              
                              for (opt in names(self$infomatic$calloptions))
                                        opts[[opt]]<-self$infomatic$calloptions[[opt]]    
                              
                              opts[["data"]]<-quote(data)
                              acall<-as.call(opts)

                              results<-try_hard(eval(acall))
                              
                              self$dispatcher$warnings<-list(topic="info", message=results$warning)
                              if (!isFALSE(results$error))
                                 stop(results$error)
                              
                              if (mf.aliased(self$model))
                                   self$dispatcher$warnings<-list(topic="info",message=WARNS["aliased"])
                             
                              .model<-mf.fixModel(results$obj,self)
                              
                              self$dispatcher$warnings<-list(topic="info",message="indirect info")
                              return(.model)


                          },
                          .estimateTests=function() {
                            
                                ginfo("Estimating Info")

                                ### update info table
                                self$tab_info[["sample"]]$value<-self$datamatic$N
                                
                                ## TODO: generalize if other models need an optimizer other than lmer
                                if (isTRUE(self$infomatic$optimized))
                                     self$tab_info[["optim"]]$value<-self$model@optinfo$optimizer
                                
                                self$tab_info[["conv"]]$value<-ifelse(mf.converged(self$model),"yes","no")
                                
                                ########## fill basic tables #########
                                if (!self$hasIntercept & is.something(self$options$factors)) 
                                  self$dispatcher$warnings<-list(topic="tab_coefficients",message=WARNS["nointercept"])
                                
                                ### coefficients table ###
                                
                                
                                

                                ### other table ###

                                

                                
                          },
                          .estimateFitIndices=function() {
                            
                            for (name in names(self$tab_fit)) {
                              
                              if (name=="lik")
                                  self$tab_fit[["lik"]]$value<-as.numeric(stats::logLik(self$model))
                              if (name=="aic")
                                  self$tab_fit[["aic"]]$value<-stats::AIC(self$model)
                              if (name=="bic")
                                  self$tab_fit[["bic"]]$value<-stats::BIC(self$model)
                              if (name=="dev")
                                  self$tab_fit[["dev"]]$value<-stats::deviance(self$model)
                              if (name=="dfr")
                                  self$tab_fit[["dfr"]]$value<-stats::df.residual(self$model)
                              if (name=="over") {
                                  value <- sum(stats::residuals(self$model, type = "pearson")^2)
                                  result <- value/stats::df.residual(self$model)
                                  self$tab_fit[["over"]]$value<-result
                              }
                        
                            }
                            
                            
                          },
                          .estimateRandom=function() {
                            
                                ### random components data frame ######
                                  if (is.null(self$tab_random))
                                      return()
                                  vc<-as.data.frame(lme4::VarCorr(self$model))
                                  variances<-which(is.na(vc$var2))
                                  covariances<-which(!is.na(vc$var2))
                                  params<-vc[variances,]
                                  names(params)<-c("groups","name","nothing","var","std")
                                  params$groups <- fromb64(params$groups,self$vars)
                                  params$name <- fromb64(params$name,self$vars)
                                  params$icc<-NA
                                  int<-which("(Intercept)" %in% params$name)
                                  for (i in int)
                                        params$icc[i]<-params$var[i]/(params$var[i]+insight::get_variance_distribution(self$model))
                                  ### Variance partitioning coefficients
                                  vartot<-sum(params$var)
                                  params$vpc<-params$var/vartot
                                  
                                  
                                  ### confidence intervals
                                  ci_covariances<-NULL
                                  if (self$option("ciRE")) {
                                    
                                      method     <-  ifelse(self$options$cimethod=="wald","Wald",self$options$cimethod)
                                      results    <-  try_hard(stats::profile(self$model,which="theta_",optimizer=self$model@optinfo$optimizer,prof.scale=c("varcov")))

                                      self$dispatcher$warnings<-list(topic="tab_random",message=results$warning)

                                      ci         <-  as.data.frame(confint(results$obj,parm = "theta_",level = self$ciwidth, method=method))
                                      names(ci)  <-  c("ci.lower","ci.upper")
                                      ci_var_pos <-  unlist(lapply(names(lme4::getME(self$model,"theta")), function(n) length(strsplit(n,".",fixed = T)[[1]][-1])==1))
                                      ci_variances <- ci[ci_var_pos,]
                                      ci_covariances <- ci[!ci_var_pos,]
                                      params<-cbind(params,ci_variances)
                                      self$dispatcher$warnings<-list(topic="tab_random",message="C.I. are computed with the profile method")
                                  }

                                  self$tab_random<-params
                                  ngrp<-vapply(self$model@flist,nlevels,1)
                                  .names<-fromb64(names(ngrp))
                                  info<-paste("Number of Obs:", self$model@devcomp$dims[[1]],", groups:",paste(.names),ngrp,collapse = ", ")
                                  self$dispatcher$warnings<-list(topic="tab_random",message=info)


                                if (is.something(covariances)) {
                                  
                                  params<-vc[covariances,]
                                  params$grp<-fromb64(params$grp,self$vars)
                                  params$var1<-fromb64(params$var1,self$vars)
                                  params$var2<-fromb64(params$var2,self$vars)
                                  if (is.something(ci_covariances))
                                     params<-cbind(params,ci_covariances)
                                  
                                  self$tab_randomCov<-params

                                }
                          

                                #### LRT for random effects ####
                                if (self$option("lrtRandomEffects")) {
                                  ## this is required by lmerTest::ranova() which looks in the parent for "data"
                                  data<-self$model@frame
                                  results<-try_hard(as.data.frame((lmerTest::ranova(self$model))))
                                  self$dispatcher$warnings<-list(topic="tab_randomTests",message=results$warning)
                                  if (!isFALSE(results$error))
                                     self$errors<-list(topic="tab_randomTests",message=paste("LR tests cannot be computed",results$error))
                                  else {
                                    ranova_test<-results$obj[-1,]
                                    ranova_test$test<-fromb64(rownames(results$obj[-1,]),self$vars)
                                    self$tab_randomTests<-ranova_test
                                  }
                                }
                                

                            },

                          .estimateEffectSizes=function() {

                            ## relative risks
                            
                            if (is.something(self$tab_relativerisk)) {
                               tab<-es.relativerisk(self)
                               self$tab_relativerisk<-private$.fix_names(tab)
                            }
                            
                          },
                          .estimateSimpleEffects=function() {
                            
                            results<-try_hard(procedure.simpleEffects(self$model,self))
                            
                            if (!isFALSE(results$error)) {
                               stop(results$error)
                            }  else {
                                  self$tab_simpleAnova         <-  results$obj[[1]]
                                  self$tab_simpleCoefficients  <-  results$obj[[2]]
                            }
                            
                        },
                          
                          
                        
                         .estimateAssumptions=function() {
                           
                           if (self$option("homoTest")) {
                             
                             factors<-intersect(unique(unlist(self$options$modelTerms)),self$options$factors)
                             
                             if (length(factors)>0) {
                               factors64<-tob64(factors)
                               data<-mf.getModelData(self$model)
                               data$res<-stats::residuals(self$model)
                               rhs <- paste0('`', factors64, '`', collapse=':')
                               formula <- as.formula(paste0('`res`~', rhs))
                               result <- car::leveneTest(formula, data, center="mean")
                               self$tab_levene<-list(list(
                                       test=result[1,'F value'],
                                       df1=result[1,'Df'],
                                       df2=result[2,'Df'],
                                       p=result[1,'Pr(>F)']))
                             } else {
                               
                               self$dispatcher$warnings<-list(topic="tab_levene",message="Levene's test requires at least one factor in the model")
                               
                             }
                             
                             if (is.something(self$tab_normtest)) {
                               
                               self$tab_normtest<-list()
                               
                               resids<-stats::residuals(self$model)
                               ### komogorov-smirnov
                               test<-stats::ks.test(resids,"pnorm",mean(resids),sd(resids))
                               self$tab_normtest[[1]]<-list(test=test$statistic,p=test$p.value)
                               
                               ### shapiro-wilk
                               
                               test<-try_hard(stats::shapiro.test(resids))
                               
                               if (!isFALSE(test$error))
                                   self$dispatcher$warnings<-list(topic="tab_normtest",message="Shapiro-Wilk not available due to the very large number of cases")
                               else
                                   self$tab_normtest[[2]]<-list(test=test$obj$statistic,p=test$obj$p.value)
                               

                             }

                             
                           }
                           

                         },
                          
                          .fix_names=function(atable) {
                            
                            .terms           <-  jmvcore::decomposeTerms(atable$source)
                            .rownames        <-  unlist(lapply(fromb64(.terms,self$vars),jmvcore::stringifyTerm,raise=T))
                            atable$source    <-  .rownames
                            atable$label     <-  self$datamatic$get_params_labels(.terms)
                            if ("Response" %in% names(atable)) {
                                   atable$Response          <-  factor(atable$Response)
                                   levels(atable$Response)  <-  unlist(self$datamatic$dep$contrast_labels)
                                   atable$Response          <-  as.character(atable$Response)
                            }
                            if ("Component" %in% names(atable))
                                               atable$source[atable$Component=="alpha"]<-"Threshold"
                            
                            return(atable)

                          }
                          
                          
                        ) #end of private
)  # end of class


### additional functions usefull for estimation of some model ###

estimate_lmer<-function(...) {
  opts<-list(...)
  data<-opts$data
  reml<-opts$reml
  for (opt in opts$optimizers) {
      
            model = lmerTest::lmer(formula=as.formula(opts$formula), data=data,REML=reml,control=lme4::lmerControl(optimizer = eval(opt)))
            if (mf.converged(model))
            break()
  }
#  this is required for lmerTest::ranova to work
  model@call$control<-lme4::lmerControl(optimizer=opt)
  ### done
  model
}
