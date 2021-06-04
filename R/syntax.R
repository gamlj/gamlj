

Syntax <- R6::R6Class(
         "Syntax",
          class=TRUE, ## this and the next 
          cloneable=FALSE, ## should improve performance https://r6.r-lib.org/articles/Performance.html ###
          inherit = Dispatch,
          public=list(
              formula=NULL,
              formula64=NULL,
              factorinfo=NULL,
              tab_info=NULL,
              tab_anova=NULL,
              tab_coefficients=NULL,
              tab_intercept=NULL,
              tab_emm=NULL,
              tab_effectsizes=NULL,
              tab_emmeans=NULL,
              tab_simpleAnova=NULL,
              tab_simpleCoefficients=NULL,
              tab_posthoc=NULL,
              tab_contrastcodes=NULL,
              initialize=function(options,datamatic) {
                super$initialize(options=options,vars=datamatic$vars)
                private$.datamatic<-datamatic
                #### we prepare the model syntax
                private$.constructFormula()
                
                ## prepare tables for init
                private$.make_structure(datamatic)



                # here we prepare the variables. Factors are expanded to dummies and all variables are B64 named
                # this produce two lists of terms, in plain names self$lav_terms and in B64 private$.lav_terms


                } # here initialize ends
              
              
          ),   # End public
          active=list(
           ### inherited warnings and errors are overriden to check specific message to change, and then passed to super 
           warnings=function(obj) {
             if (missing(obj))
               return(private$.warnings)
             if (is.null(obj))
               return()
             if (is.null(obj$message))
                 return()
             
             ### custom checks here ###

             super$warnings<-obj
           },
           errors=function(obj) {
             
             if (missing(obj))
               return(private$.errors)
             if (is.null(obj))
               return()
             
##             check<-length(grep("infinite or missing",obj,fixed = T)>0) 
##             if (check) 
##               super$errors<-ERRS[["noluck"]]
             
             super$errors<-obj
           }
           
          ),
          private=list(
            .datamatic=NULL,
            .constructFormula=function() {
              
              
              # this allows intercept only model to be passed by syntax interface
              intercept<-self$options$fixedIntercept
              modelTerms<-self$options$modelTerms
              aOne<-which(unlist(self$options$modelTerms)=="1")
              if (is.something(aOne)) {
                modelTerms[[aOne]]<-NULL
                intercept=TRUE
              }
              
                sep="+"
                if (length(modelTerms)==0) sep=""
                rform<-jmvcore::composeFormula(NULL,modelTerms)
                rform<-gsub("~",paste(self$options$dep,"~",as.numeric(intercept),sep),rform)
                self$formula<-rform

                rform<-jmvcore::composeFormula(NULL,tob64(modelTerms))
                rform<-gsub("~",paste(tob64(self$options$dep),"~",as.numeric(intercept),sep),rform)
                self$formula64<-rform
                
              
            },
  
            .make_structure=function(datamatic) {
              
              #### info table ####
              self$tab_info[[1]]<-list(info="Estimate",value="Linear model fit by OLS")
              self$tab_info[[2]]<-list(info="Call",value=self$formula)
              self$tab_info[[3]]<-list(info="R-squared")
              self$tab_info[[4]]<-list(info="Adj. R-squared")

              ### anova table ###
              
              if (is.something(self$options$modelTerms))
                  self$tab_anova<-lapply(self$options$modelTerms, function(x) list(name=jmvcore::stringifyTerm(x,raise=T)))
              
              self$tab_anova[[length(self$tab_anova)+1]]<-list(name="Residuals",f="",p="",etaSq="",etaSqP="",omegaSq="",epsilonSq="")
              self$tab_anova[[length(self$tab_anova)+1]]<-list(name="Total",f="",p="",etaSq="",etaSqP="",omegaSq="",epsilonSq="")
              
              ### parameter estimates ####
              .terms<-colnames(model.matrix(as.formula(self$formula64),private$.datamatic$data_structure64))
              .terms<-jmvcore::decomposeTerms(.terms)
              terms<-fromb64(.terms,self$vars)
              labels<-private$.datamatic$get_params_labels(.terms)
              
              self$tab_coefficients<-lapply(seq_along(terms), function(x) list(source=jmvcore::stringifyTerm(terms[x],raise=T),
                                                              label=jmvcore::stringifyTerm(labels[[x]],raise=T)))
              
                        

            ### intercept table ###
            if (self$options$interceptInfo & self$options$fixedIntercept) {
                self$tab_intercept<-list(list(name="(Intercept)"))
            }
            
            ### effect sizes table ###
              
            if (self$options$effectSizeInfo) {
                alist<-list()
                for (term in self$options$modelTerms) {
                     alist[[length(alist)+1]]<-list(effect=jmvcore::stringifyTerm(term),name=letter_eta2)
                     alist[[length(alist)+1]]<-list(effect=jmvcore::stringifyTerm(term),name=letter_peta2)
                     alist[[length(alist)+1]]<-list(effect=jmvcore::stringifyTerm(term),name=letter_pomega2)
                     alist[[length(alist)+1]]<-list(effect=jmvcore::stringifyTerm(term),name=letter_pepsilon2)
                }
                self$tab_effectsizes<-alist
            }
              
              
            ### estimated marginal means ###
            if (is.something(self$options$emmeans)) {
              
               .terms<-tob64(self$options$emmeans)
               alist<-lapply(.terms, function(.term) {
                     p<-prod(unlist(lapply(.term,function(t) private$.datamatic$variables[[t]]$nlevels)))
                     rep(list(emmeans=""),p)
               })
               self$tab_emmeans<-alist
            } 

              ### estimated marginal means ###
              if (is.something(self$options$posthoc)) {
                 
                self$tab_posthoc<-lapply(self$options$posthoc, function(.term) {
                  p<-prod(unlist(lapply(.term,function(t) private$.datamatic$variables[[tob64(t)]]$nlevels)))
                  nrow<-p*(p-1)/2
                  ncol<-(length(.term)*2)+1
                  df<-as.data.frame(matrix("",ncol=ncol,nrow=nrow))
                  names(df)<-c(.term,"op",.term)
                  df$op="-"
                  df
                })
                

              } 
              
            ### simple effects ########

              if (is.something(self$options$simpleVariable) & is.something(self$options$simpleModerators)) {

                .term<-tob64(self$options$simpleVariable)
                focal<-private$.datamatic$variables[[.term]]
                if (focal$type=="numeric")
                    neffects<-1
                else
                    neffects<-focal$nlevels-1
                .terms<-tob64(self$options$simpleModerators)
                anovap<-prod(unlist(lapply(.terms,function(t) private$.datamatic$variables[[t]]$nlevels)))
                self$tab_simpleAnova<-data.frame(F.ratio=rep("",anovap))
                coefsp<-anovap*neffects
                self$tab_simpleCoefficients<-data.frame(contrast=rep("",coefsp))

              }
              
              ### Contrast coding explanation ####
              
              if (self$options$showContrastCode) {
                self$tab_contrastcodes<-lapply(self$options$factors, function(factor) {
                    focal<-private$.datamatic$variables[[tob64(factor)]]
                    values<-focal$contrast_values
                    values<-as.data.frame(t(values))
                    names(values)<-paste("Level",focal$levels,sep="=")
                    values$cname<-focal$paramsnames
                    values$clab<-unlist(focal$contrast_labels)
                    
                    values
                    })

                }
                
  

            },

            .check_models=function() {
            }
            

          ) # end of private
) # End Rclass

