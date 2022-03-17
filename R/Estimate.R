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
                          boot_model=NULL,
                          nested_model=NULL,
                          tab_simpleAnova=NULL,
                          tab_simpleCoefficients=NULL,
                          boot_variances=NULL,
                          
                          initialize=function(options,dispatcher,datamatic) {
                            super$initialize(options,dispatcher,datamatic)
                            self$ciwidth <- options$ci_width/100
                            self$subclass<-paste0("model_",options$modeltype)
                          },
                          estimate = function(data) {
                            
                            self$model<-private$.estimateModel(data)
                            ginfo("ESTIMATE: initial estimation done")
                            if (is.something(self$nested_formula64)) {
                              obj<-try_hard(mf.update(self$model,formula=self$nested_formula64))
                              self$nested_model<-obj$obj
                              if (!isFALSE(obj$warning))
                                    self$dispatcher$warnings<-list(topic="main_r2",
                                                             message=paste("Nested model:",obj$warning))
                              if (!isFALSE(obj$error))
                                    self$dispatcher$warnings<-list(topic="main_r2",
                                                             message=paste("Nested model:",obj$error))
                              
                            }
                          }, # end of public function estimate

                          ##### fill the tables #####
                          
                          run_info=function() {
                               
                                tab<-self$init_info()
                                tab[["sample"]]$value<-self$datamatic$N
                            
                          ## TODO: generalize if other models need an optimizer other than lmer
                                if (isTRUE(self$infomatic$optimized))
                                    tab[["optim"]]$value<-self$model@optinfo$optimizer
                          
                                tab[["conv"]]$value<-ifelse(mf.converged(self$model),"yes","no")
                                
                                ### issue some notes ###
                                if (self$option("ci_method",c("quantile","bcai")) & self$option("posthoc") & self$option("posthoces") & self$option("d_ci")) 
                                  self$dispatcher$warnings<-list(topic="posthocEffectSize",message="Bootstrap confidence intervals not available. They are computed based on t-distribution")
                                tab
                          },                    
                          run_main_fit=function() {
                            
                            tab<-self$init_main_fit()
                            for (name in names(tab)) {
                              
                              if (name=="lik")
                                tab[["lik"]]$value<-as.numeric(stats::logLik(self$model))
                              if (name=="aic")
                                tab[["aic"]]$value<-stats::AIC(self$model)
                              if (name=="bic")
                                tab[["bic"]]$value<-stats::BIC(self$model)
                              if (name=="dev")
                                tab[["dev"]]$value<-stats::deviance(self$model)
                              if (name=="dfr")
                                tab[["dfr"]]$value<-stats::df.residual(self$model)
                              if (name=="over") {
                                value <- sum(stats::residuals(self$model, type = "pearson")^2)
                                result <- value/stats::df.residual(self$model)
                                tab[["over"]]$value<-result
                              }
                            }
                            
                            #### model comparison #####
                            if (!is.null(self$nested_model)) {
                            
                             for (name in names(tab)) {
                              if (name=="lik") {
                                tab[["lik"]]$nested<-as.numeric(stats::logLik(self$nested_model))
                                tab[["lik"]]$diff<-tab[["lik"]]$value-tab[["lik"]]$nested
                              }
                              if (name=="aic") {
                                tab[["aic"]]$nested<-stats::AIC(self$nested_model)
                                tab[["aic"]]$diff<-tab[["aic"]]$value-tab[["aic"]]$nested
                              }
                              if (name=="bic") {
                                tab[["bic"]]$nested<-stats::BIC(self$nested_model)
                                tab[["bic"]]$diff<-tab[["bic"]]$value-tab[["bic"]]$nested
                                
                              }
                              if (name=="dev") {
                                tab[["dev"]]$nested<-stats::deviance(self$nested_model)
                                tab[["dev"]]$diff<-tab[["dev"]]$value-tab[["dev"]]$nested
                              }
                              if (name=="dfr") {
                                tab[["dfr"]]$nested<-stats::df.residual(self$nested_model)
                                tab[["dfr"]]$diff<-tab[["dfr"]]$nested-tab[["dfr"]]$value
                                
                              }
                              if (name=="over") {
                                value <- sum(stats::residuals(self$nested_model, type = "pearson")^2)
                                result <- value/stats::df.residual(self$nested_model)
                                tab[["over"]]$nested<-result
                                tab[["over"]]$diff<-""
                                
                              }
                            }
                            }
                            
                            
                            tab
                          },
                          run_main_anova=function() {

                            if (!self$isProper) {

                                if (self$infomatic$caller=="lm") {
                                    self$dispatcher$warnings<-list(topic="main_anova",message=WARNS["lm.zeromodel"])
                                    return(mf.anova(self$model,self))
                                }   else { 
                                    self$dispatcher$warnings<-list(topic="main_anova",message=WARNS["error.zeromodel"])
                                    return(NULL)
                                }
                            }
                            mf.anova(self$model,self)
                          },
                          
                          run_main_r2=function() {

                              tab<-fit.R2(self$model,self)
                              tab
                          },
                          
                          run_main_coefficients=function() {
                            
                            if (is.null(self$boot_model) & self$option("ci_method",c("quantile","bcai"))) 
                              private$.bootstrap_model()

                            tab<-NULL
                            if (self$isProper) {
                              tab       <-  mf.parameters(self$model,self)
                              tab       <-  private$.fix_names(tab)
                            }
                            tab
                            
                          },
                          ### anova effect sizes ####
                          
                          run_main_effectsizes=function()  {
                            
                            if (!self$option("ci_method","wald")) {
                              gstart("ESTIMATE: bootstrapping variances")
                              self$boot_variances<-boot::boot(self$model$model,
                                                              statistic = es.var_boot_fun,
                                                              R = self$options$boot_r,
                                                              model=self$model)
                              gend()
                            }
                            es.glm_variances(self$model,self)
                            
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
                          run_main_vcov=function() {
                            
                            tab<-as.data.frame(stats::vcov(self$model))
                            names(tab)<-paste0("c",1:dim(tab)[2])
                            tab$source<-rownames(tab)
                            tab<-private$.fix_names(tab)
                            tab
                          },
                          
                          run_main_relativerisk=function() {
                            tab<-NULL
                            if (self$isProper) {
                              tab       <-  es.relativerisk(self)
                              tab       <-  private$.fix_names(tab)
                            }
                            if (self$hasIntercept)
                                 tab$label[tab$source=="(Intercept)"]<-"Probability"
                            
                            tab
                          },
                          run_main_paralleltest=function() {
                            tab<-NULL
                            if (self$isProper) {
                              obj<-try_hard({
                                form<-formula(paste(self$formula64,collapse = ""))
                                mod<-do.call(ordinal::clm,list(formula=form,data=self$model$model))
                                ordinal::nominal_test(mod)
                                
                                })
                              if (!isFALSE(obj$error))
                                    warnings(obj$error)
                              
                              if (!isFALSE(obj$warning)) {
                                  if (length(grep("deprecated",obj$warning,fixed=T ))==0)
                                        warnings(obj$warning)
                              }
                              
                              tab<-as.data.frame(obj$obj)
                              tab<-tab[rownames(tab)!="<none>",]
                              names(tab)<-c("df","loglik","aic", "test","p")
                              
                            }
                            tab
                          },
                          
                          run_posthoc=function() {

                            if (is.null(self$boot_model) & !self$option("ci_method","wald")) 
                              private$.bootstrap_model()
                            
                            tab<-procedure.posthoc(self)
                            tab
                          },
                          
                          run_posthocEffectSize=function() {
                            procedure.posthoc_effsize(self)
                          },
                          
                          run_emmeans=function() {
                            
                            if (is.null(self$boot_model) & !self$option("ci_method","wald")) 
                              private$.bootstrap_model()
                            if (self$option("model_type","ordinal")) {
                                 msg<-paste(1:length(self$datamatic$dep$levels_labels),self$datamatic$dep$levels_labels,sep="=",collapse = ", ")
                                 self$dispatcher$warnings<-list(topic="emmeans",message=paste("Classes are:",msg),id="emclasses")
                            }
                            procedure.emmeans(self)
                          },
                          
                          run_simpleEffects_anova=function() {
                            
                            if (is.null(self$boot_model) & !self$option("ci_method","wald")) 
                              private$.bootstrap_model()
                            
                            if (is.null(self$tab_simpleAnova))
                                private$.estimateSimpleEffects()
                            self$tab_simpleAnova
                          },
                          
                          run_simpleEffects_coefficients=function() {
                            
                            if (is.null(self$boot_model) & !self$option("ci_method","wald")) 
                              private$.bootstrap_model()
                            
                            if (is.null(self$tab_simpleCoefficients))
                               private$.estimateSimpleEffects()
                            
                            self$tab_simpleCoefficients
                          },
                          
                          run_simpleInteractions=function() {
                            
                            if (is.null(self$boot_model) & !self$option("ci_method","wald")) 
                              private$.bootstrap_model()
                            
                            
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
                            


                              opts    <-  opts<-list(str2lang(self$infomatic$rcall))

                              opts[["formula"]]<-self$formula64
                              
                              if (is.something(self$infomatic$family))
                                          opts[["family"]]<-str2lang(self$infomatic$family)    
                              
                              for (opt in names(self$infomatic$calloptions))
                                        opts[[opt]]<-self$infomatic$calloptions[[opt]]    
                              
                              opts[["data"]]<-quote(data)
                              acall<-as.call(opts)
                              results<-try_hard(eval(acall))
                              self$dispatcher$warnings<-list(topic="info", message=results$warning)
                              
                              if (!isFALSE(results$error)) {
                                if (self$option("modeltype","custom"))
                                  stop("No solution has been found for the combination of link function and distribution")
                                else
                                  stop(results$error)
                              }
                              
                              if (!isFALSE(results$warning))
                                warning(results$warning)
                              
                              if (mf.aliased(results$obj))
                                   self$dispatcher$warnings<-list(topic="info",message=WARNS["aliased"])

                              .model<-mf.fixModel(results$obj,self)
                              
                              return(.model)


                          },
                          .bootstrap_model=function() {
                                
                                opts_list<-list(model=self$model,iterations=self$options$boot_r)
                                ### check if we can go in paraller ###
                                test<-try_hard(find.package("parallel"))
                                if (isFALSE(test$error)) {
                                    opts_list[["n_cpus"]]<-parallel::detectCores()
                                    opts_list[["parallel"]]<-"multicore"
                                }
                            
                                gstart("ESTIMATE: estimating bootstrap model")
                                bmodel<-try_hard(do.call(parameters::bootstrap_model,opts_list))
                                if (isFALSE(bmodel$error)) {
                                  self$boot_model<-bmodel$obj
                                }
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
                                  if (self$option("re_ci")) {
                                    
                                      method     <-  ifelse(self$options$ci_method=="wald","Wald",self$options$ci_method)
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
                                if (self$option("re_lrt")) {
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
                           
                           if (self$option("homo_test")) {
                             
                             factors<-intersect(unique(unlist(self$options$model_terms)),self$options$factors)
                             
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
                            
                            .terms64         <-  jmvcore::decomposeTerms(atable$source)
                            .rownames        <-  unlist(lapply(fromb64(.terms64,self$vars),jmvcore::stringifyTerm,raise=T))
                            atable$source    <-  .rownames
                            atable$label     <-  self$datamatic$get_params_labels(.terms64)
                            
                            if ("response" %in% names(atable)) {
                                   atable$response          <-  factor(atable$response)
                                   levels(atable$response)  <-  unlist(self$datamatic$dep$contrast_labels)
                                   atable$response          <-  as.character(atable$response)
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
