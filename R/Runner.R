## This class takes care of estimating the model and returns the results. It inherits from Syntax, and defines the same tables
## defined by Syntax, but it fill them with the results. It also adds a few tables not defined in Syntax

## Any function that produce a table goes here

Runner <- R6::R6Class("Runner",
                        inherit = Initier,
                        cloneable=FALSE,
                        class=TRUE,
                        public=list(
                          model=NULL,
                          boot_model=NULL,
                          nested_model=NULL,
                          tab_simpleAnova=NULL,
                          tab_simpleCoefficients=NULL,
                          tab_randomcov=NULL,
                          boot_variances=NULL,
                          weights_exist=FALSE,
                          etime=0,
                          estimate = function(data) {
                            
                            
                            t<-Sys.time()
                            ## if datamatic is not ok, we do not do anything
                            if (!self$datamatic$ok) {
                              self$ok<-FALSE
                              return()
                            }
                            self$model<-private$.estimateModel(data)
                            self$etime<-as.numeric(Sys.time()-t)
                            
                            jinfo("RUNNER: initial estimation done, ",self$etime," secs")
                            
                            if (self$options$comparison) {
                              obj<-try_hard(mf.update(self$model,formula=as.formula(self$nestedformulaobj$formula64())))
                              self$nested_model<-obj$obj
                              if (!isFALSE(obj$warning))
                                    self$warning<-list(topic="main_r2",
                                                             message=paste("Nested model:",obj$warning))
                              if (!isFALSE(obj$error))
                                    self$warning<-list(topic="main_r2",
                                                             message=paste("Nested model:",obj$error))
                              
                            }
                          }, # end of public function estimate

                          ##### fill the tables #####
                          
                          run_info=function() {
                               
                                tab<-self$init_info()
                                Ns<-mf.sample_size(self$model, self)
                                tab[["sample"]]$value<-Ns[["N"]]
                                if ("wN" %in% names(Ns))  tab[["sample"]]$specs<-paste("Weighted N=",Ns[["wN"]])
                            
                          ## TODO: generalize if other models need an optimizer other than lmer
                                if (isTRUE(self$infomatic$optimized))
                                    tab[["optim"]]$value<-self$model@optinfo$optimizer
                          
                                tab[["conv"]]$value<-ifelse(mf.converged(self$model),"yes","no")
                                ### issue some notes ###
                                if (self$option("ci_method",c("quantile","bcai")) & self$option("posthoc") & self$option("posthoc_es") & self$option("d_ci")) 
                                  self$warning<-list(topic="posthocEffectSize",message="Bootstrap confidence intervals not available for effect sizes. They are computed based on t-distribution")
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
                            return(tab)
                          },
                          run_main_crosstab= function() {
                              
                            
                              y<-stats::model.response(stats::model.frame(self$model))
                              mark(table(y))
                              fitted<-stats::predict(self$model)
                              mark(table(fitted))
                              if (is.numeric(fitted)) fitted<-(exp(fitted)/(1+exp(fitted))) > .5
                              tab  <- table( y , fitted)
                              if (self$weights_exist) {
                                tab<-round(self$datamatic$wN*tab/self$datamatic$N,digits=0)
                              }
                           
                              marg <- round(100*diag(tab)/apply(tab,1,sum))
                              tab  <- lapply(1:nrow(tab), function(i) {
                                          t<-as.list(c(tab[i,],marg[i]))
                                          names(t)<-(c(paste0("pred",1:(length(t)-1)), "pcorrect"))
                                          t
                                          })
                           
                              return(tab)
                          },
                          run_main_anova = function() {

                            if (!self$formulaobj$isProper) {

                                if (self$infomatic$caller=="lm") {
                                    self$warning<-list(topic="main_anova",message=WARNS["lm.zeromodel"])
                                    return(ganova(self$model,self))
                                }   else { 
                                    self$warning<-list(topic="main_anova",message=WARNS["error.zeromodel"])
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
                            private$.coefficientstab<-tab
                            tab
                            
                          },
                          # focus on custom contrasts
                          
                          run_main_contrasts=function() {

                             tab<-private$.coefficientstab
                             if (is.null(tab)) return()
                             labels<-unlist(lapply(self$datamatic$variables, function(x) if (x$method == "custom") x$contrast_labels[[1]] else NULL))
                             w<-which(tab$label %in% labels)
                             if (length(w)==0) {
                               warning("No custom contrast defined. Custom contrasts can be defined for variables defined as factors")
                               return()
                             }
                             tab<-tab[w,]
#                            tab$source<-NULL
                      
                             if (self$infomatic$caller=="lm")
                                       tab$d<-2*tab$test/sqrt(tab$df)
                             return(tab)
                          },

                         run_main_customEffectsizes=function()  {
                            
                            if (!self$option("ci_method","wald")) 
                              warning("Bootstrap confidence intervals not available for custom contrasts. Standard method is used.")
                          
                            if (!self$option(".caller","lm") || !self$option("contrast_custom_es"))
                              return()
      


                            es.custom_variances(self$model,self)
                            
                          },
                          ### this is for beta regression
                          run_main_phi=function() {
                            

                            ss  <- summary(self$model)
                            tab <- ss$coefficients$precision
                            tab<-as.data.frame(parameters::parameters(self$model,component="precision"))
                            names(tab)<-c("source","estimate","se","none","est.ci.lower","est.ci.upper","test","df_error","p")
                            tab       <-  private$.fix_names(tab)
                            tab
                            
                          },
                          
                          ### anova effect sizes ####
                          
                          run_main_effectsizes=function()  {
                            
                            if (!self$option("ci_method","wald")) {
                              jinfo("RUNNER: bootstrapping variances")
                              self$boot_variances<-boot::boot(self$model$model,
                                                              statistic = es.var_boot_fun,
                                                              R = self$options$boot_r,
                                                              model=self$model)
                              jinfo("RUNNER: bootstrapping done")
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

                            if (self$formulaobj$hasTerms) {
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
                            if (self$formulaobj$hasTerms) {
        
                              ## for reasons beyond my comprehension, the clm model must be 
                              ## estimate there, otherwise nominal test cannot be computed
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
                              tab$source<-fromb64(rownames(tab))
                            } else
                               warning("Parallel test cannot be computed")
                            tab
                          },
                          run_main_random=function() {
                            
                              jinfo("RUNNER: estimating variance components")
                              results<-gVarCorr(self$model,self)
                              self$tab_randomcov<-results[[2]]
                              ## this is a fix for aligning the column correctly. Do not remove yet
                              results[[1]]$var1[[nrow(results[[1]])]]<-" "

                              return(results[[1]])
                              
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
                          run_main_res_corr=function() {
                            
                            tab<-as.matrix(self$model$modelStruct$corStruct)[[1]]
                            tab[!lower.tri(tab)]<-NA
                            tab[col(tab)==row(tab)]<-1
                            tab<-as.data.frame(tab)
                            tab$index<-names(tab)
                            tab

                          },
                          run_main_ranova=function() {
                            jinfo("RUNNER: ranova")
                            anovas.ranova(self$model,self)
                          },
                          
                          run_posthoc=function() {
                            
                            if (length(self$options$posthoc)==0)
                               return()

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
                            
#                            if (self$option("model_type","ordinal")) {
#                                 msg<-paste(1:length(self$datamatic$dep$levels_labels),self$datamatic$dep$levels_labels,sep="=",collapse = ", ")
#                                 self$warning<-list(topic="emmeans",message=paste("Classes are:",msg),id="emclasses")
#                            }
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
                                ## Breusch-Pagan ##
                                bptable<-list(name=c("Breusch-Pagan Test"))
                                test<-lmtest::bptest(self$model)
                                bptable[["test"]] <- test$statistic
                                bptable[["df1"]]  <- test$parameter
                                bptable[["df2"]]  <- ""
                                bptable[["p"]]    <- test$p.value
                                
                                factors <-   names(attr(stats::model.matrix(self$model),"contrasts"))
                                if (is.null(factors))
                                  return(list(bptable))
                                data$res<-residuals(self$model)
                                rhs <- paste0('`', factors, '`', collapse=':')
                                formula <- stats::as.formula(paste0('`res`~', rhs))
                                result <- car::leveneTest(formula, data, center="mean")
                                ltable<-list(name=c("Levene's Test"))
                                ltable[["test"]] <- result[1,'F value']
                                ltable[["df1"]]  <- result[1,'Df']
                                ltable[["df2"]]  <- result[2,'Df']
                                ltable[["p"]]    <- result[1,'Pr(>F)']
                                warning("Levene's test is done only for factors.")
                                return(list(bptable,ltable))
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
                          run_assumptions_collitest=function() {
                            
                            obj<-try_hard(tab <- as.data.frame(car::vif(self$model, type = "terms")))
                            if (!isFALSE(obj$error)) {
                             warning(obj$error)
                             return()
                            }
                            tab<-obj$obj   
                            names(tab)[1]<-"vif"
                            tab$tol=1/tab$vif
                            tab
                            
                          },

                          savePredRes=function(results) {
                            
                            if (self$options$predicted && results$predicted$isNotFilled()) {
                                jinfo("Saving predicted")

                                
                                pdf <- predicted(self$model,self)

                                results$predicted$set(1:ncol(pdf),
                                                      names(pdf),
                                                      rep("Predicted",ncol(pdf)),
                                                      rep("continuous",ncol(pdf)))
                                results$predicted$setValues(pdf)
                            }
                            if (self$options$residuals && results$residuals$isNotFilled()) {
                                jinfo("Saving residuals")
                                p<-stats::residuals(self$model,type="response")
                              # we need the rownames in case there are missing in the datasheet
                              pdf <- data.frame(residuals=p, row.names=rownames(insight::get_data(self$model, source="frame")))
                              results$residuals$setValues(pdf)
                            }
                          },
                          #### we need this here because emmeans needs a contrast that
                          ###  we can control in terms of variable type
                          
                          interaction_contrast=function(levels,datamatic=NULL,...) {
                            
                            nvar<-length(datamatic)
                            private$.contr_index<-private$.contr_index+1
                            i<-private$.contr_index
                            var <-datamatic[[i]]
                            if (var$type == "factor")
                                contrast<-var$contrast_values
                            else 
                                contrast<-c(-.5,.5)
                            
                            if (private$.contr_index==nvar)
                                  private$.contr_index<-0
                            
                            if (var$requireFocus()) {
                              contrast<-data.frame(contrast[,1])
                              names(contrast)<-var$contrast_labels[[1]]
                              return(contrast)
                            }
                            contrast<-as.data.frame(.ginv(t(contrast)))

                            if (var$type == "factor")
                                      names(contrast)<-paste0("(",gsub(" ","",var$contrast_labels),")")
                            else
                                     names(contrast)<-var$name

                            return(contrast)
                          }
                          
                          
                          ),# end of public

                        private=list(
                          .data64=NULL,
                          .contr_index=0,
                          .coefficientstab=NULL,
                          .estimateModel=function(data) {
                            
                              jinfo("MODULE: Estimating the model: checking")
                              ### check the dependent variable ####

                            
                              if (!(self$datamatic$dep$type %in% self$infomatic$deptype)) {

                                    t2  <-  paste(self$infomatic$deptype,collapse = " or ")
                                    t1  <-  self$datamatic$dep$type
                                    m   <-   self$infomatic$model[1]
                                    if (self$options$.interface=="jamovi") {
                                      t2<-gsub("numeric","Continuous",t2,fixed = TRUE)
                                      t2<-gsub("factor","Nominal",t2,fixed=TRUE)
                                      t2<-gsub("integer","Measurement type=`Continuous`, Data type=`Integer`",t2,fixed=TRUE)
                                      
                                    }
                                    msg<-paste("Dependent variable is of type",t1,".",m,"requires variable of type: ",t2)
                                    self$stop(msg)
                                    
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

                             jinfo("MODULE: Estimating the model: making options")
                             
                              opts    <-  opts<-list(str2lang(self$infomatic$rcall))
                              
                              if (is.something(self$infomatic$formula))
                                          opts[["formula"]]<-self$infomatic$formula

                              
                              if (is.something(self$infomatic$family))
                                          opts[["family"]]<-str2lang(self$infomatic$family)    
                              
                              for (opt in names(self$infomatic$calloptions))
                                        opts[[opt]]<-self$infomatic$calloptions[[opt]]   

                              ## for some reason, mclogit::mblogit requires the list of random terms
 
                              
                              if (self$option("offset"))
                                 opts[["formula"]]<-paste(opts[["formula"]],"+offset(",tob64(self$options$offset),")")

                              ### check if there are weights and can be used
                              ### weights can come from jamovi or from a call in infomatic
                              ### they should be passed as numeric, not a variable name
                              
                              ## if they arrive from the call, make it numeric
                              if (is.something(opts$weights) && !is.numeric(opts$weights)) {
                                 opts$weights<-data[[opts$weights]]
                              }
                              
                              ## if they arrive from jamovi, plug them in
                              if (is.something(attr(data, "jmv.weights")) ) {
                                   if (self$infomatic$has_weights) {
                                        self$weights_exist<-TRUE
                                        opts[["weights"]]<-as.numeric(attr(data, "jmv.weights"))      
                                   } else 
                                      self$warning<-list(topic="weightsnotes",message=paste("However,",self$infomatic$model[[2]]," does not accept weights. This analysis used the data unweighted."))
                                
                              }
                              opts[["data"]]<-quote(data)
                              acall<-as.call(opts)
                              jinfo("MODULE: Estimating the model: running")
                          
                              results<-try_hard(eval(acall))

                              self$warning<-list(topic="modelnotes", message=results$warning, head="warning")
                              self$warning<-list(topic="info", message=results$message, head="info")

                              if (!isFALSE(results$error)) {
                                if (self$option("model_type","custom")) {
                                 msg1<-"No solution has been found for the combination of link function and distribution. \n\n"
                                 msg2<-results$error
                                 if (length(grep("valid starting values",msg2))>0) msg2<-""
                                 stop(msg1,msg2)
                                }
                                else
                                  stop(fromb64(results$error))
                              }
                             
                              
                              if (mf.aliased(results$obj))
                                   self$warning<-list(topic="info",message=WARNS["aliased"], head="info")

                              .model<-mf.fixModel(results$obj,self,data)
                              
                              ### add custom info to the model
                              if (self$option("model_type","ordinal")) {
                                      msg<-paste(1:length(self$datamatic$dep$levels_labels),self$datamatic$dep$levels_labels,sep="=",collapse = ", ")
                                      self$warning<-list(topic="modelnotes",message=paste("Classes are:",msg), head="info")
                                     }                              
                              return(.model)


                          },
                          .bootstrap_model=function() {
                            
                            if (self$weights_exist) {
                              self$warning<-list(topic="info",message="Bootstrap method not available with weighted data.")
                              return()
                            }
                            
                            ### Here is how the storage mechanism works:
                            ### In the .b.R file we assign a table to be the runner storage.
                            ### When the model is estimated we save it as a Rdata file named with a random sequence and
                            ### save in the storage table name the file name. When results update, if self$storage$state
                            ### is not null, the Rdata file is load retrieving its name from the storage$state
                            ### if storage$state is null, the model is bootstrapped

                            if (is.something(self$storage) && is.something(self$storage$state)) {
                              
                              id<-self$storage$state
                              load(id)
                              self$boot_model<-boot_model
                              
                            } else {
                            
                                   opts_list<-list(model=self$model,iterations=self$options$boot_r)
                                   jinfo(paste("Boot repetitions: ",opts_list$iterations))

                                   ### check if we can go in paraller ###
                                     test<-try_hard(find.package("parallel"))
                                     if (isFALSE(test$error)) {
                                         jinfo("We go in parallel")
                                         if (Sys.info()['sysname']!="Windows") {
                                           opts_list[["n_cpus"]]<-parallel::detectCores()
                                           opts_list[["parallel"]]<-"multicore"
                                         }
                                         else
                                            opts_list[["parallel"]]<-"no"
                                     }
                                      
                                jinfo("RUNNER: estimating bootstrap model")
                                t<-Sys.time()
                              
                                bmodel<-try_hard(do.call(parameters::bootstrap_model,opts_list))
                                etime<-as.numeric(Sys.time()-t)
                                jinfo("RUNNER: done ",etime," secs")
                                if (isFALSE(bmodel$error)) {
                                  self$boot_model<-bmodel$obj
                                  jinfo("RUNNER: storing results")
                                  id<-tempfile()
                                  self$storage$setState(id)
                                  boot_model<-self$boot_model
                                  save(boot_model,file = id)
                                  jinfo("RUNNER: done")
                                }
                            }
                          },
                          .estimateTests=function() {
                            
                                jinfo("Estimating Info")

                                ### update info table
                                self$tab_info[["sample"]]$value<-self$datamatic$N
                                
                                ## TODO: generalize if other models need an optimizer other than lmer
                                if (isTRUE(self$infomatic$optimized))
                                     self$tab_info[["optim"]]$value<-self$model@optinfo$optimizer
                                
                                self$tab_info[["conv"]]$value<-ifelse(mf.converged(self$model),"yes","no")
                                
                                ########## fill basic tables #########
                                if (!self$hasIntercept & is.something(self$options$factors)) 
                                  self$warning<-list(topic="main_coefficients",message=WARNS["nointercept"])

                                
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
                                   atable$response          <-  fromb64(atable$response)
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
  good<- NULL
  tried<-list()
  for (opt in opts$optimizers) {
            model = lmerTest::lmer(formula=stats::as.formula(opts$formula),
                                   data=data,
                                   REML=reml,
                                   control=lme4::lmerControl(optimizer = eval(opt),calc.derivs=FALSE))
            ladd(tried)<-opt
            jinfo("MODULE: trying optimizer", opt)
            if (mf.converged(model)) {
              good<-ifelse(is.null(good),opt,good)
              if (!lme4::isSingular(model))
                         break()
           }
  }
  if (lme4::isSingular(model) && !is.null(good))
        model = lmerTest::lmer(formula=stats::as.formula(opts$formula), 
                               data=data,
                               REML=reml,
                               control=lme4::lmerControl(optimizer = eval(good),calc.derivs=FALSE))
  if (length(tried)>1)
     message(paste("Optimizer ",paste(tried,collapse=", "), "have been tried to find a solution."))    
  
#  this is required for lmerTest::ranova to work
  model@call$control<-lme4::lmerControl(optimizer=good)
  ### done
  return(model)
}

estimate_lme<-function(...) {
  
  opts<-list(...)
  data<-opts$data
  coropts<-list(form=stats::formula(opts$form))
  if (utils::hasName(opts,"coropts")) coropts<-c(coropts,opts$coropts)
  cor <- do.call(opts$cor,coropts)
  model = nlme::lme(fixed=opts$fixed, 
                      random=opts$random,
                      data=data,
                      method=opts$method,
                      correlation=cor
                      )
   model$call$correlation<-cor  
   model$call$fixed<-opts$fixed
   model$call$random<-opts$random
   model$call$method<-opts$method
   model$call[[1]]<-quote(nlme::lme.formula)
  return(model)
}




