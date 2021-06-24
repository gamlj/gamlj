

Syntax <- R6::R6Class(
         "Syntax",
          class=TRUE, ## this and the next 
          cloneable=FALSE, ## should improve performance https://r6.r-lib.org/articles/Performance.html ###
          inherit = Dispatch,
          public=list(
              formula=NULL,
              formula64=NULL,
              factorinfo=NULL,
              hasIntercept=TRUE,
              hasTerms=FALSE,
              isProper=NULL,
              tab_info=NULL,
              tab_anova=NULL,
              tab_coefficients=NULL,
              tab_intercept=NULL,
              tab_emm=NULL,
              tab_effectsizes=NULL,
              tab_emmeans=NULL,
              tab_simpleAnova=NULL,
              tab_simpleCoefficients=NULL,
              tab_simpleInteractionCoefficients=NULL,
              tab_simpleInteractionAnova=NULL,
              tab_posthoc=NULL,
              tab_contrastcodes=NULL,
              tab_r2=NULL,
              tab_fit=NULL,
              tab_relativerisk=NULL,
              datamatic=NULL,
              infomatic=NULL,
              initialize=function(options,datamatic) {
                
                super$initialize(options=options,vars=datamatic$vars)
                self$datamatic<-datamatic
                #### we prepare the model syntax
                private$.constructFormula()
                
                ### infomatic class takes care of all info about different model
                self$infomatic<-Infomatic$new(options,datamatic)
                
                
                ## prepare tables for init
                private$.make_structure()





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
            .constructFormula=function() {
              # this allows intercept only model to be passed by syntax interface
              self$hasIntercept<-self$options$fixedIntercept
              modelTerms<-self$options$modelTerms
              aOne<-which(unlist(self$options$modelTerms)=="1")
              if (is.something(aOne)) {
                modelTerms[[aOne]]<-NULL
                self$hasIntercept=TRUE
              }
              self$hasTerms <-(length(modelTerms)>0)
              self$isProper <-(self$hasIntercept | self$hasTerms)
              
              sep<-"+"
                if (!self$hasTerms) sep=""
                rform<-jmvcore::composeFormula(NULL,modelTerms)
                rform<-gsub("~",paste(self$options$dep,"~",as.numeric(self$hasIntercept),sep),rform)
                self$formula<-rform

                rform<-jmvcore::composeFormula(NULL,tob64(modelTerms))
                rform<-gsub("~",paste(tob64(self$options$dep),"~",as.numeric(self$hasIntercept),sep),rform)
                self$formula64<-rform
                
              
            },
  
            .make_structure=function() {
              
              #### info table ####
              self$tab_info                   <-  self$infomatic$info_table()
              self$tab_info[["call"]]$specs   <-  self$formula
              
              if (self$option("dep_scale"))
                    self$tab_info[["dep"]]<-list(info="Y transform",value=self$options$dep_scale)

              ### anova table ###
              
              if (self$hasTerms)
                  self$tab_anova<-lapply(self$options$modelTerms, function(x) list(name=.stringifyTerm(x)))
              
              if (self$options$modelSelection=="lm") {
                    self$tab_anova[[length(self$tab_anova)+1]]<-list(name="Residuals",f="",p="",etaSq="",etaSqP="",omegaSq="",epsilonSq="")
                    self$tab_anova[[length(self$tab_anova)+1]]<-list(name="Total",f="",p="",etaSq="",etaSqP="",omegaSq="",epsilonSq="")
              }  
              ### we need at least a row otherwise we cannot add notes to the table
              if (is.null(self$tab_anova))
                    self$tab_anova[[1]]<-list(test="")

              #### additional fit indeces ####

              if (is.something(self$infomatic$fit))
                  self$tab_fit<-self$infomatic$info_fit()

              
              ### parameter estimates ####
              .terms<-colnames(model.matrix(as.formula(self$formula64),self$datamatic$data_structure64))
              .len<-length(.terms)
              if (self$options$modelSelection=="multinomial") {
                .len  <- .len * (self$datamatic$dep$nlevels-1)
              }
              self$tab_coefficients<-lapply(1:.len, function(t) list(source=""))

              
            ### intercept info table ###
              
              if (self$option("interceptInfo")) {
               self$tab_intercept<-list(source="(Intercept)") 
                
              }
                
            ### effect sizes table ###

            if (self$option("effectSizeInfo")) {
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
            if (self$option("emmeans")) {
              
               .terms<-tob64(self$options$emmeans)
               alist<-lapply(.terms, function(.term) {
                     nrow<-prod(unlist(lapply(.term,function(t) self$datamatic$variables[[t]]$nlevels)))
                     if (self$options$modelSelection=="multinomial") {
                        nrow<-nrow*(self$datamatic$dep$nlevels)
                     }
                     
                     rep(list(emmeans=""),nrow)
               })
               self$tab_emmeans<-alist
               emm<-self$infomatic$emmeans
               if (!is.null(emm))
                   self$warnings<-list(topic="tab_emmeans",message=paste("Expected means are expressed as",emm))
            }

              ### posthoc means ###
              if (self$option("posthoc")) {
                 
                self$tab_posthoc<-lapply(self$options$posthoc, function(.term) {
                  p<-prod(unlist(lapply(.term,function(t) self$datamatic$variables[[tob64(t)]]$nlevels)))
                  nrow<-p*(p-1)/2
                  ncol<-(length(.term)*2)+1
                  if (self$options$modelSelection=="multinomial") {
                      nrow<-nrow*(self$datamatic$dep$nlevels)
                  }
                  df<-as.data.frame(matrix("",ncol=ncol,nrow=nrow))
                  .names<-c(.term,"vs",.term)
                  names(df)<-.names
                  df$vs="-"
                  df
                })
                

              } 
              
            ### simple effects ########

              if (is.something(self$options$simpleVariable) & is.something(self$options$simpleModerators)) {

                .term<-tob64(self$options$simpleVariable)
                focal<-self$datamatic$variables[[.term]]
                if (focal$type=="numeric")
                    neffects<-1
                else
                    neffects<-focal$nlevels-1
                .terms<-tob64(self$options$simpleModerators)
                anovap<-prod(unlist(lapply(.terms,function(t) self$datamatic$variables[[t]]$nlevels)))
                self$tab_simpleAnova<-data.frame(F.ratio=rep("",anovap))
                coefsp<-anovap*neffects
                if (self$options$modelSelection=="multinomial")
                       coefsp <- coefsp * (self$datamatic$dep$nlevels-1)
                
                self$tab_simpleCoefficients<-data.frame(contrast=rep("",coefsp))

              }
              
              ##### simple interactions ######
              
              if (self$options$simpleInteractions)
                  if (is.something(self$options$simpleVariable) & is.something(self$options$simpleModerators)) {
                      params<-list()
                ### moderators should be reverted in order to match emmeans 
                    .term<-rev(self$options$simpleModerators)
                    n<-length(.term)
                    j<-n
                    params<-list()
                    anovas<-list()
                    while(j>1) {
                          mods<-.term[j:n]
                         .names<-setdiff(.term,mods)
                         params[[length(params)+1]]<-.names
                         j<-j-1
                    }
                ### and the results of the definitions should be revered as well
                   self$tab_simpleInteractionCoefficients<-rev(params)
                   self$tab_simpleInteractionAnova<-rev(params)
              }
                
              
                
              
                            
              ### Contrast coding explanation ####
              
              if (self$options$showContrastCode) {
                self$tab_contrastcodes<-lapply(self$options$factors, function(factor) {
                    focal<-self$datamatic$variables[[tob64(factor)]]
                    values<-focal$contrast_values
                    values<-as.data.frame(t(values))
                    names(values)<-paste("Level",focal$levels,sep="=")
                    values$cname<-focal$paramsnames
                    values$clab<-unlist(focal$contrast_labels)
                    
                    values
                    })

                }
                
              ### here are specific calls ########
            
              ### relative risk
              if (self$option("effectSize","RR")) {

                    simtab  <-  self$tab_coefficients
                    
                    if (self$hasIntercept)
                          simtab  <-  simtab[-1]
                  
                    self$tab_relativerisk<-simtab
              }
              

            }



          ) # end of private
) # End Rclass


.nicifyLabels<-function(labels) {
  
  flabs<-grep(DUMMY_TAIL,names(labels),fixed=T)
  
  for (i in flabs)
    labels[[i]]<-paste0(labels[[i]],paste0(rep(IMPROBABLE_SEQ,i),collapse = ""))
  news<-jmvcore::stringifyTerm(labels,raise=T)
  for (i in seq_along(news))
    news[[i]]<-gsub(IMPROBABLE_SEQ,"",news[[i]],fixed=T)
  news
}

.stringifyTerm<-function(term) {
  jmvcore::stringifyTerm(term,raise=T)
}

