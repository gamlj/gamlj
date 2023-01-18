

Syntax <- R6::R6Class(
  "Syntax",
  class=TRUE, ## this and the next 
  cloneable=FALSE, ## should improve performance https://r6.r-lib.org/articles/Performance.html ###
  inherit = Scaffold,
  public=list(
    vars=NULL,
    formulaobj=NULL,
    hasIntercept=TRUE,
    hasTerms=FALSE,
    isProper=NULL,
    tab_anova=NULL,
    tab_coefficients=NULL,
    tab_intercept=NULL,
    tab_emm=NULL,
    tab_effectsizes=NULL,
    tab_emmeans=NULL,
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
    initialize=function(options,dispatcher,datamatic) {
      
      super$initialize(options,dispatcher)
      self$datamatic<-datamatic
      
      #### we prepare the model syntax
      self$formulaobj<-gFormula$new()
      self$formulaobj$fixed_intercept<-self$optionValue("fixed_intercept")
      self$formulaobj$nested_intercept<-self$optionValue("nested_intercept")
      self$formulaobj$dep<-self$options$dep
      self$formulaobj$fixed<-self$options$model_terms
      self$formulaobj$random<-self$optionValue("re")
      self$formulaobj$nested_fixed<-self$optionValue("nested_terms")
      self$formulaobj$nested_random<-self$optionValue("nested_re")
      self$formulaobj$offset<-self$optionValue("offset")
      self$formulaobj$update_terms(self$datamatic$data_structure64)

      ### infomatic class takes care of all info about different models
      self$infomatic<-Infomatic$new(options,datamatic)
      
    }, # here initialize ends
    #### init functions #####
    
    init_info=function() {
      tab                   <-   self$infomatic$info_table()
      tab[["call"]]$specs   <-    self$formulaobj$formula()
      
      if (self$option("dep_scale"))
          tab[["dep"]]     <-  list(info="Y transform",value=self$options$dep_scale,specs="")

      ### confidence intervals

      method<-switch(self$options$ci_method,
                     wald="Wald",
                     profile="Profile",
                     quantile="Bootstrap percent",
                     bcai="Bootstrap BCa"
      
                                    )
      info<-switch(self$options$ci_method,
                     wald="",
                     profile="",
                     quantile=paste(self$options$boot_r, "bootstrap samples"),
                     bcai=paste(self$options$boot_r, "bootstrap samples")
      )
      
      tab[["ci"]]<-list(info="C.I. method",value=method,specs=info)
      
      
      if (self$options$comparison) {
        
         tab[["mc"]]<-list(info="Comparison",
                           value="Nested model",
                           specs=self$formulaobj$nested_formula())
         
         tab[["mctest"]]<-list(info="Comparison",
                               value="Tested terms",
                               specs=self$formulaobj$nested_tested_fixed())
         
         if (self$option("nested_re")) {
             tab[["mctest1"]]<-list(info="Comparison",
                                    value="Tested random",
                                    specs=self$formulaobj$nested_tested_random())
         }
      }
        
      if (self$option("offset"))
         tab[["offset"]]<-list(info="Offset",value=self$options$offset,specs="Coefficient set to 1")
      
      
      if (self$option("se_method","robust"))
        warning("All SE are heteroskedasticity-consistent robust SE")
      
      tab
      
    },
    init_main_r2=function() {
      
        if (self$option(".caller",c("lmer","glmer"))) {
          tab<-list(list(type="Marginal"),list(type="Conditional"))
          
          if (self$options$comparison) {
            
            tab<-list(list(type="Conditional",model="Full"),
                      list(type="Marginal",model="Full"),
                      list(type="Conditional",model="Nested"),
                      list(type="Marginal",model="Nested"),
                      list(type="Comparison",model=paste0(greek_vector[["Delta"]],"R\u00B2"))
                      )
          }
  
        } else {
        tab<-list(model="")
        
        if (self$options$comparison)
           tab<-list(list(model="Full"),
                     list(model="Nested"),
                     list(model=paste0(greek_vector[["Delta"]],"R\u00B2")))
        }
      tab
      
    },
    
    init_main_fit=function() {
      
            tab<-self$infomatic$info_fit()
            if (is.null(tab))
              tab[[1]]<-list(info="")
            tab
            
    },
    init_main_anova=function() {
      
      tab<-list()
      if (self$formulaobj$hasTerms) {
        tab<-lapply(self$formulaobj$anova_terms, function(x) list(source=.stringifyTerm(x)))
      }
      
      if (self$options$model_type=="lm") {
        if (self$formulaobj$hasTerms)
          tab<-prepend_list(tab,list(source="Model",f="."))
        tab<-append_list(tab,list(source="Residuals",f="",p="",etaSq="",etaSqP="",omegaSq="",omegaSqP="",epsilonSq="",epsilonSqP=""))
        tab<-append_list(tab,list(source="Total",f="",p="",etaSq="",etaSqP="",omegaSq="",omegaSqP="",epsilonSq="",epsilonSqP=""))
      }       
      ### we need at least a row otherwise we cannot add notes to the table
      if (!is.something(tab))
        tab[[1]]<-list(test="")
      tab
    },
    
    ### parameter estimates ####
    init_main_coefficients=function() {
      
      .terms<-colnames(model.matrix(as.formula(self$formulaobj$fixed_formula64()),self$datamatic$data_structure64))
      .len<-length(.terms)
      if (self$options$model_type=="multinomial") 
        .len  <- .len * (self$datamatic$dep$nlevels-1)
      
      if (self$options$model_type=="ordinal") 
        .len  <- .len + (self$datamatic$dep$nlevels-2)
      
      lapply(1:.len, function(t) list(source=""))
      
    },
    init_main_contrastCodeTables=function() {
      
        tab<-NULL
        
        if (self$options$show_contrastcodes) {
          
          tab <-lapply(self$options$factors, function(factor) {
            focal<-self$datamatic$variables[[tob64(factor)]]
            values<-focal$contrast_values
            values<-as.data.frame(t(values))
            names(values)<-paste("Level",focal$levels,sep="=")
            values$cname<-focal$paramsnames
            values$clab<-unlist(focal$contrast_labels)
            values
          })
        }
    },
    init_main_effectsizes=function() {

        alist<-NULL  
        if (self$option("es_info")) {
          alist<-list()
          for (term in self$options$model_terms) {
            ladd(alist) <-  list(effect=jmvcore::stringifyTerm(term),name=letter_eta2)
            ladd(alist) <-  list(effect=jmvcore::stringifyTerm(term),name=letter_peta2)
            ladd(alist) <-  list(effect=jmvcore::stringifyTerm(term),name=letter_omega2)
            ladd(alist) <-  list(effect=jmvcore::stringifyTerm(term),name=letter_pomega2)
            ladd(alist) <-  list(effect=jmvcore::stringifyTerm(term),name=letter_epsilon2)
            ladd(alist) <-  list(effect=jmvcore::stringifyTerm(term),name=letter_pepsilon2)
          }
        }
        alist
    },
    ### intercept more info ###
    
    init_main_intercept=function() {
      
      self$tab_intercept<-list(source="(Intercept)")
      
    },
### vcov more info ###

    init_main_vcov=function() {
  
       .terms<-self$formulaobj$params_terms
       .len <- length(.terms)
       .titles<-fromb64(.terms)
      
       if (self$options$model_type=="multinomial") {
         .len  <- .len * (self$datamatic$dep$nlevels-1)
         .titles<-c(paste("1",.titles,sep=":"),paste("2",.titles,sep=":"))
         
       }
       if (self$options$model_type=="ordinal") {
         .len  <- .len + (self$datamatic$dep$nlevels-2)
         .titles<-c(.titles[-1],paste0("int",1:(self$datamatic$dep$nlevels-1)))
       }
       mat<-as.data.frame(matrix(".",nrow=.len,ncol=.len+1))
       names(mat)<-c("source",paste0("c",1:.len))
       attr(mat,"titles")<-.titles
       mat
      },

    init_main_relativerisk=function() {
      
      alist<-NULL
      if (self$option("es","RR") ) {
        alist  <-  self$init_main_coefficients()
#        if (self$hasIntercept)
#          alist  <-  alist[-1]
      }
      alist
    },
    init_main_paralleltest=function() {
      
      self$init_main_anova()
    },

    # random effect variances for lmer
    init_main_random=function() {
      
      if (self$option("re_ci")) 
            self$dispatcher$warnings<-list(topic="main_random",
                                           message="Computation of C.I. may take a while. Please be patient.",
                                           init=TRUE)
      
      rows<-sum(length(unlist(self$options$re)))
      data.frame(group=rep("",(rows/2)+1))

    },
    init_main_randomcov=function() {
      return()
},


    ### posthoc means ###
    
    init_posthoc=function() {
      

        lapply(self$options$posthoc, function(.term) {
          p<-prod(unlist(lapply(.term,function(t) self$datamatic$variables[[tob64(t)]]$nlevels)))
          nrow<-p*(p-1)/2
          ncol<-(length(.term)*2)+1
          if (self$options$model_type=="multinomial") {
            nrow<-nrow*(self$datamatic$dep$nlevels)
          }
          df<-as.data.frame(matrix("",ncol=ncol,nrow=nrow))
          .vars<-make.names(.term,unique = T)
          .names <- c(paste0(.vars,"_lev1"),".vs.",paste0(.vars,"_lev2"))
          .titles<-c(.term,"vs",.term)
          names(df)<-.names
          df$.vs.="-"
          attr(df,"titles")<-.titles
          df
        })

    },
    init_posthocEffectSize=function() {
      self$init_posthoc()
    },
    
    ### estimated marginal means ###
    
    init_emmeans=function() {
      
        alist=NULL
        
        if (self$option("emmeans")) {
          
          .terms<-tob64(self$options$emmeans)
          alist<-lapply(.terms, function(.term) {
            ncol<-length(.term)
            nrow<-prod(unlist(lapply(.term,function(t) self$datamatic$variables[[t]]$nlevels)))
            if (self$options$model_type=="multinomial") {
              nrow<-nrow*(self$datamatic$dep$nlevels)
            }
            one<-data.frame(matrix("",ncol=ncol,nrow = nrow))
            names(one)<-fromb64(.term)
            one
          })
          
          emm<-self$infomatic$emmeans
          if (!is.null(emm))
            self$dispatcher$warnings<-list(topic="emmeans",message=paste("Expected means are expressed as",emm))
        }
        alist
    },
    
    init_simpleEffects_anova=function() {
      
        .var64<-tob64(self$options$simple_effects)
        .mods<-rev(self$options$simple_moderators)
        .mods64<-tob64(.mods)
        nrow<-prod(unlist(lapply(.mods64,function(m) self$datamatic$variables[[m]]$nlevels)))
        ncol<-length(.mods64)
        df<-data.frame(matrix("",nrow = nrow,ncol=ncol))
        names(df)<-paste0("mod_",make.names(.mods,unique = T))
        attr(df,"titles")<-.mods
        df
        
    },
    
    init_simpleEffects_coefficients=function() {
      
        .var64<-tob64(self$options$simple_effects)
        focal<-self$datamatic$variables[[.var64]]
        neffects<-focal$neffects
        .mods<-rev(self$options$simple_moderators)
        .mods64<-tob64(.mods)
        
        nrow<-neffects*prod(unlist(lapply(.mods64,function(m) self$datamatic$variables[[m]]$nlevels)))
        ncol<-length(.mods64)
        
        if (self$options$model_type=="multinomial")
          nrow <- nrow * (self$datamatic$dep$nlevels-1)
        
        df<-data.frame(matrix("",nrow = nrow,ncol=ncol))
        names(df)<-paste0("mod_",make.names(.mods,unique = T))
        attr(df,"titles")<-.mods
        df

    },
    init_simpleInteractions=function() {
      
    
        ### moderators should be reverted in order to match emmeans 
        .term<-rev(self$options$simple_moderators)
        .simple<-self$options$simple_effects
        n<-length(.term)
        j<-n
        resultsList<-list()
        inter_term<-list()
        while(j>1) {
          ## mods are the variables that go in the interaction with simple
          .mods<-.term[j:n]
          ## inters are the variables in the interaction
          .inters<-c(.simple,.mods)
          ## params are selected moderators
          .params<-setdiff(.term,.mods)

          inter_term <- append_list(inter_term,.inters)
          .names<-make.names(paste0("mod_",.params))
          
          .params64<-tob64(.params)
          ntests<-prod(unlist(lapply(.params64,function(m) self$datamatic$variables[[m]]$nlevels)))
          df1<-data.frame(matrix(".",ncol=length(.names),nrow=ntests))
          names(df1)<-.names
          attr(df1,"titles")<-.params
          
          ## for coefficients
          .inters64<-tob64(.inters)
          neffects<-ntests*prod(unlist(lapply(.inters64,function(m) self$datamatic$variables[[m]]$neffects)))
          
          df2<-data.frame(matrix(".",ncol=length(.names),nrow=neffects))
          names(df2)<-.names
          attr(df2,"titles")<-.params
          resultsList[[length(resultsList)+1]]<-list(df1,df2)
          j<-j-1
        }
        ### the order should be reverted to fit the results
        inter_term<-rev(inter_term)
        resultsList<-rev(resultsList)
        attr(resultsList,"keys")<-inter_term
        resultsList
      
    }
    
    
    
    
  ),   # End public
  
  private=list(
    
    .make_structure=function() {
      
      ## some warnings ###
      if (self$option("ci_method","boot"))
        self$dispatcher$warnings<-list(topic="info",message="Bootstrap C.I. are being computed, this may take a while")
      
      ### anova table ###
      
      
      #### additional fit indeces ####
      
      if (is.something(self$infomatic$fit))
        self$tab_fit<-self$infomatic$info_fit()
      
      
      
      ### intercept info table ###
      
      #              if (self$option("interceptInfo")) {
      #               self$tab_intercept<-list(source="(Intercept)") 
      
      #              }
      
      ### effect sizes table ###
      
      
      
      
      ### simple effects ########
      
      
      ##### simple interactions ######
      
      if (self$options$simple_interactions) {
        if (is.something(self$options$simple_effects) & length(self$options$simple_moderators)>1 ) {
          params<-list()
          ### moderators should be reverted in order to match emmeans 
          .term<-rev(self$options$simple_moderators)
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
      
      
      ### here are specific calls ########
      
      ### relative risk
      if (self$option("es","RR")) {
        
        simtab  <-  self$tab_coefficients
        
        if (self$hasIntercept)
          simtab  <-  simtab[-1]
        
        self$tab_relativerisk<-simtab
      }
      
      if (self$option("re")) {
        count<-sum(unlist(sapply(self$options$re, length)))
        self$tab_random<-rep(list(groups=""),count+1)
      }
      
      if (self$option("norm_test")) {
        self$tab_normtest<-list(list(name="Kolmogorov-Smirnov"),
                                list(name="Shapiro-Wilk"))
        
      }
      
      
      
      
    }

  ) # end of private
) # End Rclass



.stringifyTerm<-function(term) {
  jmvcore::stringifyTerm(term,raise=T)
}

