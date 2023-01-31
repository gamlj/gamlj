## This class takes care of estimating the model and returns the results. It inherits from Syntax, and defines the same tables
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
                          tab_randomcov=NULL,
                          boot_variances=NULL,
                          
                          initialize=function(options,dispatcher,datamatic) {
                            super$initialize(options,dispatcher,datamatic)
                            self$ciwidth <- options$ci_width/100
                            self$subclass<-paste0("model_",options$model_type)
                          },
                          estimate = function(data) {
                            
                            self$model<-private$.estimateModel(data)
                            ginfo("ESTIMATE: initial estimation done")
                            if (self$options$comparison) {
                              
                              obj<-try_hard(mf.update(self$model,formula=self$nestedformulaobj$formula64()))
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
                                if (self$option("ci_method",c("quantile","bcai")) & self$option("posthoc") & self$option("posthoc_es") & self$option("d_ci")) 
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
                                tab[["dev"]]$value<-insight::get_deviance(self$model)
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
                                tab[["dev"]]$nested<-insight::get_deviance(self$nested_model)
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

                            if (!self$formulaobj$isProper) {

                                if (self$infomatic$caller=="lm") {
                                    self$dispatcher$warnings<-list(topic="main_anova",message=WARNS["lm.zeromodel"])
                                    return(ganova(self$model,self))
                                }   else { 
                                    self$dispatcher$warnings<-list(topic="main_anova",message=WARNS["error.zeromodel"])
                                    return(NULL)
                                }
                            }
                            ganova(self$model,self)
                          },
                          
                          run_main_r2=function() {
                             
                              obj<-gFit$new(self)
                              obj$r2table()

                          },
                          
                          run_main_coefficients=function() {
                            
                            if (is.null(self$boot_model) & self$option("ci_method",c("quantile","bcai"))) 
                              private$.bootstrap_model()

                            tab<-NULL
                            if (self$formulaobj$isProper) {
                              tab       <-  gparameters(self$model,self)
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
                          run_main_marginals=function() {
                            tab<-NULL
                            if (self$formulaobj$isProper) {
                              tab       <-  es.marginals(self)
                            }
                            tab
                          },
                          
                          run_main_relativerisk=function() {
                            tab<-NULL
                            if (self$formulaobj$isProper) {
                              tab       <-  es.relativerisk(self)
                              tab       <-  private$.fix_names(tab)
                            }
                            if (self$hasIntercept)
                                 tab$label[tab$source=="(Intercept)"]<-"Probability"
                            
                            tab
                          },
                          run_main_paralleltest=function() {
                            
                            tab<-NULL
                            if (self$hasTerms) {
                              obj<-try_hard({
                                form<-formula(paste(self$formulaobj$formula64(),collapse = ""))
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
                              
                            } else
                               warning("Parallel test cannot be computed")
                            tab
                          },
                          run_main_random=function() {
                            
                              vc<-gVarCorr(self$model,self)
                              grp<-unlist(lapply(vc$grp, function(a) gsub("\\.[0-9]$","",a)))
                              ### if the model has no intercept or intercept and 
                              ### effects of factors are set as uncorrelated
                              ### lmer() estimate random means, and name them
                              ### factornamefactorlevel. We fix this make the 
                              ### labels more intuitive
                              vc$groups <- fromb64(grp)
                              var1<-gsub(LEVEL_SYMBOL," = ",vc$var1,fixed = T)
                              vc$name   <- fromb64(var1)
                              var2<-gsub(LEVEL_SYMBOL," = ",vc$var2,fixed = T)
                              vc$name2  <- fromb64(var2)
                              
                              variances<-which(is.na(vc$var2))
                              params<-vc[variances,]
                              params$icc<-NA
                              
                              int<-which(params$name %in% "(Intercept)")
                              for (i in int)
                                  params$icc[i]<-params$var[i]/(params$var[i]+insight::get_variance_distribution(self$model,verbose = FALSE))

                              nr<-nrow(params)+1
                              if (params$grp[nrow(params)]!="Residual"){
                                  params[nr,"groups"] <- "Residual"
                                  params[nr,"name"]  <- ""
                                  params[nr,"var"]   <- insight::get_variance_residual(self$model,verbose = FALSE)
                                  params[nr,"std"]   <- sqrt(params[nr,"var"]) 
                              }
                                
                              
                              self$dispatcher$warnings<-list(topic="main_random",message=attr(vc,"info"))
                              
                              covariances<-which(!is.na(vc$var2))
                            if (nrow(vc[covariances,])>0) {
                                self$tab_randomcov<-vc[covariances,]
                              }
                              params
                          },
                          run_main_randomcov=function() {

                            if (is.something(self$tab_randomcov))
                                return(self$tab_randomcov)
                          },
                          run_main_multirandom=function() {

                            
                            tab<-self$model$VarCov
                            tab<-lapply(tab,function(x) {
                               q<-as.data.frame(x)
                               q$name<-fromb64(rownames(q))
                               q})
                            tab
                            
                          },
                          run_main_ranova=function() {
                            
                            procedure.ranova(self$model,self)
                            
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
                            
                            if (self$options$model_type=="multinomial" & self$options$.caller=="glmer") 
                              return(NULL)
                            
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

                                
                                pdf <- predicted(self$model,self)
                                
                                results$predicted$set(1:ncol(pdf),
                                                      names(pdf),
                                                      rep("Predicted",ncol(pdf)),
                                                      rep("continuous",ncol(pdf)))
                                results$predicted$setValues(pdf)
                            }
                            if (self$options$residuals && results$residuals$isNotFilled()) {
                                ginfo("Saving residuals")
                                p<-stats::resid(self$model)
                              # we need the rownames in case there are missing in the datasheet
                              pdf <- data.frame(residuals=p, row.names=rownames(insight::get_data(self$model)))
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
                          .estimateModel=function(data) {
                            
                              ginfo("MODULE: Estimating the model: checking")
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
                             ### Some functions, such as lme4::glmer.nb 
                             ### require a package to be loaded 
                             ### in the function parent environment.
                             ### we give the required functions to them
                             glmer<-lme4::glmer
                             ###

                             ginfo("MODULE: Estimating the model: making options")
                             
                              opts    <-  opts<-list(str2lang(self$infomatic$rcall))
                              
                              if (!("formula" %in% names(opts)))
                                          opts[["formula"]]<-self$formulaobj$formula64()
                              
                              if (is.something(self$infomatic$family))
                                          opts[["family"]]<-str2lang(self$infomatic$family)    
                              
                              for (opt in names(self$infomatic$calloptions))
                                        opts[[opt]]<-self$infomatic$calloptions[[opt]]   

                              ## for some reason, mclogit::mblogit requires the list of random terms
 
                              
                              if (self$option("offset"))
                                opts[["formula"]]<-paste(opts[["formula"]],"+offset(",tob64(self$options$offset),")")
                              opts[["data"]]<-quote(data)

                              acall<-as.call(opts)
                              
                              ginfo("MODULE: Estimating the model: running")
                              results<-try_hard(eval(acall))
                              
                              self$dispatcher$warnings<-list(topic="info", message=results$warning)
                              
                              if (!isFALSE(results$error)) {
                                if (self$option("model_type","custom"))
                                  stop("No solution has been found for the combination of link function and distribution")
                                else
                                  stop(results$error)
                              }
                              
                              if (!isFALSE(results$warning))
                                warning(results$warning)
                              
                              if (mf.aliased(results$obj))
                                   self$dispatcher$warnings<-list(topic="info",message=WARNS["aliased"])

                              .model<-mf.fixModel(results$obj,self,data)
                              
                              return(.model)


                          },
                          .bootstrap_model=function() {
                                
                                opts_list<-list(model=self$model,iterations=self$options$boot_r)
                                ### check if we can go in paraller ###
                                test<-try_hard(find.package("parallel"))
                                if (isFALSE(test$error)) {
                                    ginfo("we go in parallel")
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
                                  self$dispatcher$warnings<-list(topic="main_coefficients",message=WARNS["nointercept"])

                                
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
                                  self$tab_fit[["dev"]]$value<-insight::get_deviance(self$model)
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
                            

                                covariances<-which(!is.na(vc$var2))
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

                          .estimateSimpleEffects=function() {
                            
                            results<-try_hard(procedure.simpleEffects(self$model,self))
                            
                            if (!isFALSE(results$error)) {
                               stop(results$error)
                            }  else {
                                  self$tab_simpleAnova         <-  results$obj[[1]]
                                  self$tab_simpleCoefficients  <-  results$obj[[2]]
                            }
                            
                        },

                          .fix_names=function(atable) {
                            
                            .terms64         <-  jmvcore::decomposeTerms(atable$source)
                            .rownames        <-  unlist(lapply(fromb64(.terms64),jmvcore::stringifyTerm,raise=T))
                            atable$source    <-  .rownames
                            
                            if (!("label" %in% names(atable)))
                                atable$label     <-  self$datamatic$get_params_labels(.terms64)
                            else 
                              atable$label     <-  self$datamatic$get_params_labels(atable$label)
                            
                            if ("response" %in% names(atable)) {
                                   atable$response          <-  factor(atable$response)
                                   levels(atable$response)  <-  unlist(self$datamatic$dep$contrast_labels)
                                   atable$response          <-  as.character(atable$response)
                            }
                            if ("level" %in% names(atable)) {
                              atable$level          <-  factor(atable$level)
                              levels(atable$level)  <-  unlist(self$datamatic$dep$levels_labels)
                              atable$level          <-  as.character(atable$level)
                            }
                            
                            if ("Component" %in% names(atable))
                                               atable$source[atable$Component=="alpha"]<-"Threshold"

                            if ("contrast" %in% names(atable)) {
                              atable$contrast<-fromb64(atable$contrast)
                              atable$contrast[atable$contrast==""]<-atable$source[atable$contrast==""]
                            }
                            return(atable)

                          }
                          
                          
                        ) #end of private
)  # end of class


### additional functions useful for estimation of some model ###

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


