

Initier <- R6::R6Class(
  "Operator",
  class=TRUE, ## this and the next 
  cloneable=FALSE, ## should improve performance https://r6.r-lib.org/articles/Performance.html ###
  inherit = Scaffold,
  public=list(
    vars=NULL,
    formulaobj=NULL,
    nestedformulaobj=NULL,
    hasIntercept=TRUE,
    hasTerms=FALSE,
    isProper=NULL,
    datamatic=NULL,
    infomatic=NULL,
    ciwidth=NULL,
    subclass=NULL,
    initialize=function(jmvobj,datamatic) {
      
      super$initialize(jmvobj)
      self$datamatic<-datamatic
      self$ciwidth <- self$options$ci_width/100
      self$subclass<-paste0("model_",self$options$model_type)
      x<-self$datamatic$data_structure64
      names(x)<-fromb64(names(x))
      #### we prepare the model syntax
      self$formulaobj<-gFormula$new()
      self$formulaobj$fixed_intercept<-self$optionValue("fixed_intercept")
      self$formulaobj$random_corr<-self$optionValue("re_corr")
      self$formulaobj$dep<-self$options$dep
      self$formulaobj$fixed<-self$options$model_terms
      self$formulaobj$random<-self$optionValue("re")
      self$formulaobj$offset<-self$optionValue("offset")
      self$formulaobj$update_terms(self$datamatic$data_structure64)

      #### we prepare the nested model syntax, if necessary
      
      if (self$option("comparison")) {
            self$nestedformulaobj<-gFormula$new()
            self$nestedformulaobj$fixed_intercept<-self$optionValue("nested_intercept")
            self$nestedformulaobj$random_corr<-"block"
            self$nestedformulaobj$dep<-self$options$dep
            self$nestedformulaobj$fixed<-self$optionValue("nested_terms")
            self$nestedformulaobj$random<-self$optionValue("nested_re")
            self$nestedformulaobj$offset<-self$optionValue("offset")
            self$nestedformulaobj$update_terms(self$datamatic$data_structure64)
            
      }      
      
      
      ### infomatic class takes care of all info about different models
      self$infomatic<-Infomatic$new(self$options,datamatic,self$formulaobj)
      
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
      if (method != "Wald") 
          self$warning=list(topic="info",message=paste(method," method for C.I. may take a while, please be patient."),initOnly=TRUE)
      
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
                           specs=self$nestedformulaobj$formula())
         
                 tab[["mctest"]]<-list(info="Comparison",
                               value="Tested terms",
                               specs=self$formulaobj$nested_tested_fixed(self$nestedformulaobj))
         
                 if (self$option("nested_re")) {
                     tab[["mctest1"]]<-list(info="Comparison",
                                    value="Tested random",
                                    specs=self$formulaobj$nested_tested_random(self$nestedformulaobj))
                  }
      }
        
      if (self$option("offset"))
         tab[["offset"]]<-list(info="Offset",value=self$options$offset,specs="Coefficient set to 1")
      
      
      if (self$option("se_method","robust"))
         tab[["se_method"]]<-list(info="SE method",value="Robust")
      
      tab
      
    },
    init_main_r2=function() {
      
          tab<-self$infomatic$r2
          if (self$options$comparison) {
             tab<-c(tab,tab)
             models<-rep(c("Full","Nested"),each=length(tab)/2)
             for (i in seq_along(tab)) tab[[i]]$model<-models[[i]]
             ladd(tab)<-list(type="Comparison",model=paste0(greek_vector[["Delta"]],"R\u00B2"))
          }
      tab
      
    },
    
    init_main_fit=function() {
      
            tab<-self$infomatic$info_fit()
            if (is.null(tab))
              tab[[1]]<-list(info="")
            tab
            
    },
     init_main_crosstab=function() {

            nl <- self$datamatic$dep$nlevels
            tab<-as.data.frame(matrix(".",ncol=nl+2,nrow=nl))
            
            names(tab)<-c("obs",paste0("pred",1:nl),"pcorrect") 
            tab$obs<-self$datamatic$dep$levels_labels
            attr(tab,"titles")<-c(self$datamatic$dep$levels_labels,"% Correct")
            attr(tab,"types")<-c(rep("integer",length(nl)+2))
            tab
            
    },

    init_main_anova=function() {
      
      if (self$options$model_type=="multinomial" & self$options$.caller=="glmer") {
        self$warning<-list(topic="main_anova",
                                       message="Fixed Effects Omnibus Tests not available for this type of model.")
        
        return(NULL)
        
      }
      
      tab<-list()
      if (self$formulaobj$hasTerms) {
        tab<-lapply(self$formulaobj$anova_terms, function(x) list(source=.stringifyTerm(x)))
      }
      
      if (self$options$model_type=="lm") {
        if (self$formulaobj$hasTerms)
          padd(tab)<-list(source="Model",f=".")
        ladd(tab)<-list(source="Residuals",f="",p="",etaSq="",etaSqP="",omegaSq="",omegaSqP="",epsilonSq="",epsilonSqP="")
        ladd(tab)<-list(source="Total",f="",p="",etaSq="",etaSqP="",omegaSq="",omegaSqP="",epsilonSq="",epsilonSqP="")
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
            ladd(alist) <-  list(effect=jmvcore::stringifyTerm(term,raise = TRUE),name=letter_eta2)
            ladd(alist) <-  list(effect=jmvcore::stringifyTerm(term,raise = TRUE),name=letter_peta2)
            ladd(alist) <-  list(effect=jmvcore::stringifyTerm(term,raise = TRUE),name=letter_omega2)
            ladd(alist) <-  list(effect=jmvcore::stringifyTerm(term,raise = TRUE),name=letter_pomega2)
            ladd(alist) <-  list(effect=jmvcore::stringifyTerm(term,raise = TRUE),name=letter_epsilon2)
            ladd(alist) <-  list(effect=jmvcore::stringifyTerm(term,raise = TRUE),name=letter_pepsilon2)
          }
        }
        alist
    },
    ### intercept more info ###
    
    init_main_intercept=function() {
      
      list(source="(Intercept)")
      
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
            self$warning<-list(topic="main_random",
                                           message="Computation of C.I. may take a while. Please be patient.",
                                           initOnly=TRUE)
      
      rows<-sum(length(unlist(self$options$re)))
      data.frame(group=rep("",(rows/2)+1))

    },
    init_main_randomcov=function() {
      return()
},

init_main_multirandom=function() {
  
  random<-self$formulaobj$listify_random()
  tabs<-lapply(random,function(x) list(name=""))
  attr(tabs,"keys")<-random
  tabs
},

init_main_res_corr=function() {
   list(list(var="."))
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
            self$warning<-list(topic="emmeans",message=paste("Expected means are expressed as",emm))
        }
        alist
    },
    
    init_simpleEffects_anova=function() {
      
      if (self$options$model_type=="multinomial" & self$options$.caller=="glmer") 
        return(NULL)
      
        .var64<-tob64(self$options$simple_x)
        .mods<-rev(self$options$simple_mods)
        .mods64<-tob64(.mods)
        nrow<-prod(unlist(lapply(.mods64,function(m) self$datamatic$variables[[m]]$nlevels)))
        ncol<-length(.mods64)
        df<-data.frame(matrix("",nrow = nrow,ncol=ncol))
        names(df)<-paste0("mod_",make.names(.mods,unique = T))
        attr(df,"titles")<-.mods
        df
        
    },
    
    init_simpleEffects_coefficients=function() {
      
        .var64<-tob64(self$options$simple_x)
        focal<-self$datamatic$variables[[.var64]]
        neffects<-focal$neffects
        .mods<-rev(self$options$simple_mods)
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
        .term<-rev(self$options$simple_mods)
        .simple<-self$options$simple_x
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

          ladd(inter_term) <- .inters
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
      
    },
    init_assumptions_collitest=function() {
      
      tab<-list(source="")
      if (self$formulaobj$hasTerms) {
        tab<-lapply(self$formulaobj$anova_terms, function(x) list(source=.stringifyTerm(x)))
      }
      tab
    },

   
   run_assumptions_homotest=function() {
     alist<-list(list(name="Breusch-Pagan Test"))
     if (is.something(self$options$factors))
       ladd(alist)<-list(name="Levene's Test")
     return(alist)
     
   }
    
    
    
    
  ),   # End public
  
  private=list(

  ) # end of private
) # End Rclass



.stringifyTerm<-function(term) {
  jmvcore::stringifyTerm(term,raise=T)
}

