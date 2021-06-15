## This class takes care of estimating the model and return the results. It inherit from Syntax, and defines the same tables
## defined by Syntax, but it fill them with the results. It also adds a few tables not defined in Syntax

## Any function that produce a table goes here

Estimate <- R6::R6Class("Estimate",
                        inherit = Syntax,
                        cloneable=FALSE,
                        class=FALSE,
                        public=list(
                          model=NULL,
                          summary=NULL,
                          anova=NULL,
                          tab_anova=NULL,
                          tab_coefficients=NULL,
                          ciwidth=NULL,
                          subclass=NULL,
                          initialize=function(options,datamatic) {
                            super$initialize(options=options,datamatic=datamatic)
                            self$ciwidth <- options$ciWidth/100
                            self$subclass<-paste0("model_",options$modelSelection)
                          },
                          estimate = function(data) {
                            private$.estimateModel(data)
                            private$.estimateTests()
                            private$.estimateFit()
                            private$.estimatePostHoc()
                            private$.estimateEffectSizes()
                            private$.estimateIntercept()
                            private$.estimateSimpleEffects()
                            private$.estimateEmmeans()
                            
                            ginfo("Estimation is done...")
                          }, # end of publich function estimate
                          
                          savePredRes=function(results) {
                            
                            if (self$options$predicted && results$predicted$isNotFilled()) {
                              ginfo("Saving predicted")
                              if ("multinom" %in% class(self$model))  type="probs" else type="response"
                              p<-stats::predict(self$model,type=type)
                              # we need the rownames in case there are missing in the datasheet
                              pdf <- data.frame(predicted=p, row.names=rownames(mf.getModelData(model)))
                              results$predicted$setValues(p)
                            }
                            if (self$options$residuals && results$residuals$isNotFilled()) {
                              ginfo("Saving residuals")
                              p<-stats::resid(self$model)
                              # we need the rownames in case there are missing in the datasheet
                              pdf <- data.frame(residuals=p, row.names=rownames(mf.getModelData(model)))
                              results$residuals$setValues(pdf)
                            }
                          },
                          
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
                        privat=list(
                          .data64=NULL,
                          .contr_index=0,
                          .estimateModel=function(data) {
                            
                            form<-self$formula64
                            if (self$options$modelSelection=="lm") {
                              ### estimte the model ###
                                  results<-try_hard(stats::lm(form, data=data))
                                  model<-results$obj
                                  self$warnings<-list(topic="info", message=results$warning)
                                  self$errors<-results$error
                                  attr(model,"refit")<-list(command="lm",
                                                      coptions=list(formula=self$formula),
                                                      eoptions=list(formula=self$formula))
                                  if (!self$hasIntercept & is.something(self$options$factors)) {
                                    self$warnings<-list(topic="tab_coefficients",message=WARNS["nointercept"])
                                  }
                            
                            
                                  self$summary<-summary(model)
                                  if (length(model$coefficients)>0) {
                                        results<-try_hard(parameters::parameters(model))
                                        if (is.something(results$warning))
                                            self$warnings<-list(topic="tab_coefficients",message=results$warning)
                                        coefficients<-as.data.frame(parameters::parameters(model))
                                        coefficients$CI<-NULL
                                        names(coefficients)<-c("source","estimate","se","ci.lower","ci.upper","t","df","p")
                                        coefficients$beta<-procedure.beta(model)
                                        self$tab_coefficients<-private$.fix_names(coefficients)
                                  }
                            }
                            self$model<-model
                          },
                          .estimateTests=function() {
                            if (self$options$modelSelection=="lm") {
                            
                                      if (length(coef(self$model))==0) {
                                        results<-try_hard({
                                          self$anova<-stats::anova(self$model)
                                          self$warnings<-list(topic="tab_anova",message=WARNS["glm.zeromodel"])
                                        })

                                      } else {
                                          results<-try_hard({
                                          self$anova<-car::Anova(self$model,type=3)
                                          })
                                      }
                                      self$warnings<-list(topic="tab_anova",message=results$error)
                                      if (results$error!=FALSE)
                                              return()
                                      self$warnings<-list(topic="tab_anova",results$warning)
                                      class(self$anova)<-c(self$subclass,class(self$anova))
                                      self$tab_anova<-mf.anova(self$anova)
                                      
                            }

                            },
                          .estimateFit=function() {
                            
                            if (self$options$modelSelection=="lm") {
                              
                                  ss<-self$summary
                                  results<-list()
                                  results$df1<-ss$fstatistic[["numdf"]]
                                  results$df2<-ss$fstatistic[["dendf"]]
                                  results$r1<-ss$r.squared
                                  results$r2<-ss$adj.r.squared
                                  if (hasName(ss,"fstatistic")) {
                                      results$f<-ss$fstatistic[["value"]]
                                      results$p<-stats::pf(results$f,results$df1,results$df1, lower.tail = FALSE)
                                  } else 
                                      self$warnings<-list(topic="tab_r2",message="Inferential tests cannot be computed")
                                  
                                  self$tab_r2<-list(results)
                                  
                            }
                            
                          },
                          
                          .estimateIntercept=function() {
                            
                             if (!self$options$interceptInfo || ! self$options$fixedIntercept) 
                                return()
                            
                                ss<-self$summary
                                tt<-ss$coefficients[1,3]
                                f<-tt^2
                                df<-stats::df.residual(self$model)
                                p<-ss$coefficients[1,4]
                                peta<-effectsize::t_to_eta2(tt,df_error = df)
                                omega<-effectsize::t_to_omega2(tt,df_error = df)
                                epsilon<-effectsize::t_to_epsilon2(tt,df_error = df)
                                self$tab_intercept<-list(list(source="(Intercept)",
                                                              df=df,
                                                              f=f,
                                                              etaSqP=peta[1,1],
                                                              omegaSq=omega[1,1],
                                                              epsilonSq=epsilon[1,1],
                                                              p=p))
                                
                            
                            
                          },
                          .estimateEffectSizes=function() {
                            
                            if (!is.something(self$tab_effectsizes))
                              return()
                            
                            eta<-effectsize::eta_squared(self$anova,partial = F,ci=self$ciwidth,verbose=F)
                            peta<-effectsize::eta_squared(self$anova,partial = T,ci=self$ciwidth,verbose=F)
                            omega<-  effectsize::omega_squared(self$anova,partial = T,ci=self$ciwidth,verbose=F)
                            epsilon<-  effectsize::epsilon_squared(self$anova,partial = T,ci=self$ciwidth,verbose=F)
                            alist<-list()
                            for (i in seq_along(eta$Parameter)) {
                              alist[[length(alist)+1]]<-list(..space..=eta[i,1],estimate=eta[i,2],ci.lower=eta[i,4],ci.upper=eta[i,5])
                              alist[[length(alist)+1]]<-list(..space..=eta[i,1],estimate=peta[i,2],ci.lower=peta[i,4],ci.upper=peta[i,5])
                              alist[[length(alist)+1]]<-list(..space..=eta[i,1],estimate=omega[i,2],ci.lower=omega[i,4],ci.upper=omega[i,5])
                              alist[[length(alist)+1]]<-list(..space..=eta[i,1],estimate=omega[i,2],ci.lower=epsilon[i,4],ci.upper=epsilon[i,5])
                            }
                            self$tab_effectsizes<-alist
                          },
                          
                          
                          .estimatePostHoc=function() {
                            
                            if (!is.something(self$tab_posthoc))
                               return()
                            
                            self$tab_posthoc<-procedure.posthoc(self)

                          },
                          .estimateEmmeans=function() {
                            
                            if (!is.something(self$tab_emmeans))
                              return()
                            
                            self$tab_emmeans<-procedure.emmeans(self)
                            
                            
                          },
                          .estimateSimpleEffects=function() {
                            
                            if (!is.something(self$tab_simpleAnova))
                              return()
                            
                            tables<-procedure.simpleEffects(self)
                            self$tab_simpleAnova<-tables[[1]]
                            self$tab_simpleCoefficients<-tables[[2]]
                            adds<-procedure.simpleInteractions(self)
                            self$tab_simpleInteractionCoefficients<-adds[[1]]
                            self$tab_simpleInteractionAnova<-adds[[2]]
                            
                          },
                          

                          .fix_names=function(atable) {
                            
                            .terms<-jmvcore::decomposeTerms(atable$source)
                            .rownames<-unlist(lapply(fromb64(.terms,self$vars),jmvcore::stringifyTerm,raise=T))
                            atable$source<-.rownames
                            atable$label<-self$datamatic$get_params_labels(.terms)
                            atable

                          }
                        ) #end of private
)  # end of class
