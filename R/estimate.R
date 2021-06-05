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
                          tab_coefficients=NULL,
                          ciwidth=NULL,
                          initialize=function(options,datamatic) {
                            super$initialize(options=options,datamatic=datamatic)
                            self$ciwidth <- options$ciWidth/100
                          },
                          estimate = function(data) {
                            private$.estimateModel(data)
                            private$.estimateTests()
                            ginfo("Estimation is done...")
                          } # end of publich function estimate

                          ),# end of public
                        privat=list(
                          .data64=NULL,
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
                            
                            
                            self$summary<-summary(model)
                            coefficients<-as.data.frame(parameters::parameters(model))
                            mark(coefficients)
                            coefficients$CI<-NULL
                            names(coefficients)<-c("source","estimate","se","ci.lower","ci.upper","t","df","p")
                            coefficients$beta<-procedure.beta(model)
                            self$tab_coefficients<-private$.fix_names(coefficients)
                            }                            
                            
                          },
                          .estimateTests=function() {
                            
                            self$anova<-NULL
                            
                          },
                          .fix_names=function(atable) {
                            
                            .rownames<-jmvcore::decomposeTerms(fromb64(atable$source,self$vars))
                            .rownames<-unlist(lapply(.rownames,jmvcore::stringifyTerm,raise=T))
                            atable$source<-.rownames
                            atable

                          }
                        ) #end of private
)  # end of class
