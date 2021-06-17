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
                            private$.estimateSimpleInteractions()
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
                        privat=list(
                          .data64=NULL,
                          .contr_index=0,
                          .estimateModel=function(data) {
                            
                              results<-try_hard(eval(parse(text=private$.syntax())))
                              self$model<-results$obj
                              self$warnings<-list(topic="info", message=results$warning)
                              self$errors<-list(topic="info", message=results$warning)
                            
                              if (is.something(self$errors))
                                       stop(self$errors)
                              
                              if (!self$hasIntercept & is.something(self$options$factors)) 
                                       self$warnings<-list(topic="tab_coefficients",message=WARNS["nointercept"])
                            
                              if (length(self$model$coefficients)>0) {
                                    results<-try_hard(parameters::parameters(self$model,exponentiate=FALSE))
                                    if (is.something(results$warning))
                                        self$warnings<-list(topic="tab_coefficients",message=results$warning)
                                        coefficients<-as.data.frame(results$obj)
                                    coefficients$CI<-NULL
                                    names(coefficients)<-c("source","estimate","se","ci.lower","ci.upper","t","df","p")
                                    
                                    if (self$option("effectSize","expb")) {
                                        ex<-as.data.frame(parameters::parameters(self$model,exponentiate=TRUE))
                                        ex<-ex[,c("Coefficient","CI_low" ,"CI_high")]
                                        names(ex)<-c("expb","expb.ci.lower","expb.ci.upper")
                                        coefficients<-cbind(coefficients,ex)
                                    }
                                    if (self$option("effectSize","beta"))
                                          coefficients$beta<-procedure.beta(self$model)
                                    
                                    self$tab_coefficients<-private$.fix_names(coefficients)
                                  }
                            
                          },
                          .estimateTests=function() {

                                if (!self$isProper) 
                                              self$warnings<-list(topic="tab_anova",message=WARNS["glm.zeromodel"])
                                
                                self$tab_anova<-mf.anova(self$model,self)
                            
                            },
                          .estimateFit=function() {
                              
                                self$tab_r2<-mf.R2(self$model,self)

                          },
                          
                          .estimateIntercept=function() {
                            
                             if (is.null(self$tab_intercept)) 
                                return()
                            
                                ss<-summary(self$model)
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
                            anova<-car::Anova(self$model,type=3)
                            eta<-effectsize::eta_squared(anova,partial = F,ci=self$ciwidth,verbose=F)
                            peta<-effectsize::eta_squared(anova,partial = T,ci=self$ciwidth,verbose=F)
                            omega<-  effectsize::omega_squared(anova,partial = T,ci=self$ciwidth,verbose=F)
                            epsilon<-  effectsize::epsilon_squared(anova,partial = T,ci=self$ciwidth,verbose=F)
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

                          },
                          
                          .estimateSimpleInteractions=function() {
                            
                            if (!is.something(self$tab_simpleInteractionAnova))
                              return()
                            
                            tables<-procedure.simpleInteractions(self)
                            self$tab_simpleInteractionCoefficients<-tables[[1]]
                            self$tab_simpleInteractionAnova<-tables[[2]]
                            
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
