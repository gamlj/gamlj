

Syntax <- R6::R6Class(
         "Syntax",
          class=TRUE, ## this and the next 
          cloneable=FALSE, ## should improve performance https://r6.r-lib.org/articles/Performance.html ###
          inherit = Dispatch,
          public=list(
              formula=NULL,
              formula64=NULL,
              hasIntercept=TRUE,
              hasTerms=FALSE,
              clusters=NULL,
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
              tab_random=NULL,
              tab_randomCov=NULL,
              tab_randomTests=NULL,
              tab_levene=NULL,
              tab_normtest=NULL,
              
              datamatic=NULL,
              infomatic=NULL,
              initialize=function(options,datamatic) {
                
                super$initialize(options=options,vars=datamatic$vars)
                self$datamatic<-datamatic
                
                #### we prepare the model syntax
                private$.constructFormula()
                
                ### infomatic class takes care of all info about different models
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
              if (self$options$modelSelection=="ordinal") {
                self$hasIntercept=TRUE
                if (self$options$fixedIntercept==FALSE)
                   self$warnings<-list(topic="tab_info",message="Ordinal regression requires the intercept. It has been added to the model")
              }
              
              self$hasTerms <-(length(modelTerms)>0)
              self$isProper <-(self$hasIntercept | self$hasTerms)
              
              rands<-NULL
              if (self$option("randomTerms")) 
                     rands<-private$.buildreffects()

              sep<-"+"
                if (!self$hasTerms) sep=""

                fixed<-jmvcore::composeFormula(NULL,tob64(modelTerms))
                fixed<-gsub("~",paste(tob64(self$options$dep),"~",as.numeric(self$hasIntercept),sep),fixed)
                self$formula64<-trimws(paste(fixed,rands,sep =  ""))
                self$formula<-fromb64(self$formula64,self$vars)
              
            },
  
            .make_structure=function() {
              
              #### info table ####
              self$tab_info                   <-  self$infomatic$info_table()
              self$tab_info[["call"]]$specs   <-  self$formula
              
              if (self$option("dep_scale"))
                    self$tab_info[["dep"]]<-list(info="Y transform",value=self$options$dep_scale)
              
              if (is.something(self$infomatic$r2)) {
                
                 self$tab_r2<-lapply(self$infomatic$r2, function(x) list(type=x))
                 
              }
              
              ## some warnings ###
              if (self$option("cimethod","boot"))
                  self$warnings<-list(topic="tab_info",message="Bootstrap C.I. are being computed, this may take a while")

              ### anova table ###
              
              if (self$hasTerms)
                  self$tab_anova<-lapply(self$options$modelTerms, function(x) list(name=.stringifyTerm(x)))
              
              if (self$options$modelSelection=="lm") {
                if (self$hasTerms)
                        self$tab_anova<-prepend_list(self$tab_anova,list(name="Model",f="",p="",etaSq="",etaSqP="",omegaSq="",epsilonSq=""))
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
              .terms<-colnames(model.matrix(lme4::nobars(as.formula(self$formula64)),self$datamatic$data_structure64))
              .len<-length(.terms)
              if (self$options$modelSelection=="multinomial") {
                .len  <- .len * (self$datamatic$dep$nlevels-1)
              }
              if (self$options$modelSelection=="ordinal") {
                .len  <- .len + (self$datamatic$dep$nlevels-2)
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
                     alist[[length(alist)+1]]<-list(effect=jmvcore::stringifyTerm(term),name=letter_omega2)
                     alist[[length(alist)+1]]<-list(effect=jmvcore::stringifyTerm(term),name=letter_pomega2)
                     alist[[length(alist)+1]]<-list(effect=jmvcore::stringifyTerm(term),name=letter_epsilon2)
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
              
              if (self$options$simpleInteractions) {
                  if (is.something(self$options$simpleVariable) & length(self$options$simpleModerators)>1 ) {
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
              
              if (self$option("randomTerms")) {
                count<-sum(unlist(sapply(self$options$randomTerms, length)))
                self$tab_random<-rep(list(groups=""),count+1)
              }
              
              if (self$option("normTest")) {
                self$tab_normtest<-list(list(name="Kolmogorov-Smirnov"),
                                        list(name="Shapiro-Wilk"))

              }
              

              

            },
            .buildreffects=function() {
              
              terms  <-  self$options$randomTerms
              ## this is for R. It overrides the correlatedEffect option 
              correl<-TRUE
              if (length(terms)>1)
                   correl  <-  "block"
              
              # remove empty sublists
              terms <- terms[sapply(terms, function(a) !is.null(unlist(a)))]
              
              # split in sublists if option=nocorr
              if (self$options$correlatedEffects=="nocorr") {
                termslist<-terms[[1]]
                terms<-lapply(termslist,list)
              }
              rterms<-""    
              for(i in seq_along(terms)) {
                one<-terms[[i]]
                one64<-lapply(one,jmvcore::toB64)
                flatterms<-lapply(one64,function(x) c(jmvcore::composeTerm(head(x,-1)),tail(x,1)))
                res<-do.call("rbind",flatterms)
                ### check blocks coherence
                if (length(unique(res[,2]))>1 && correl=="block")
                    stop("Correlated random effects by block should have the same cluster variable within each block. Please specify different blocks for random coefficients with different clusters.")
                
                self$clusters<-c(self$clusters,unique(res[,2]))
                
                res<-tapply(res[,1],res[,2],paste)
                res<-sapply(res, function(x) paste(x,collapse = " + "))
                ### delat with intercept ###
                for (i in seq_along(res)) {
                      test<-grep(jmvcore::toB64("Intercept"),res[[i]],fixed=TRUE)
                      if (is.something(test))
                        res[[i]]<-gsub(jmvcore::toB64("Intercept"),1,res[[i]])
                      else 
                        res[[i]]<-paste("0 + ",res[[i]])
                }
#                if (is.something(test))
#                  res<-gsub(jmvcore::toB64("Intercept"),1,res)
#                else
#                  res[[1]]<-paste(0,res[[1]],sep = "+")
                
                ### compose ####
                form<-paste(res,names(res),sep=" | ")
                form<-paste("(",form,")")
                rterms<-paste(rterms,form,sep = "+")
              }
              ## paste and return ``
              rterms<-paste(rterms,collapse = "")
              rterms
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

