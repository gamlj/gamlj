
gamljgmixedClass <- R6::R6Class(
  "gamljgmixedClass",
  inherit=gamljgmixedBase,
  private=list(
      .dispatcher=NULL,
      .data_machine=NULL,
      .runner_machine=NULL,
      .plotter_machine=NULL,
      .ready=NULL,
      .time=NULL,
      .smartObjs=list(),
      .init=function() {
        
        ginfo(paste("MODULE:",self$options$.caller,self$options$model_type,"  #### phase init  ####"))
        class(private$.results) <- c('gamlj', class(private$.results))
        private$.time<-Sys.time()
        private$.ready<-readiness(self$options)
        if (!private$.ready$ready) {
          if(private$.ready$report)
            self$results$info$addRow("info",list(info="Setup",specs=private$.ready$reason))
          return()
        }
        emmeans::emm_options(lmerTest.limit = 25000)  
        
        ### set up the R6 workhorse class
        dispatcher<-Dispatch$new(self$results)
        data_machine<-Datamatic$new(self$options,dispatcher,self$data)
        runner_machine<-Runner$new(self$options,dispatcher,data_machine)
        
        
        ### info table ###
        aSmartObj<-SmartTable$new(self$results$info,runner_machine)
        ladd(private$.smartObjs)<- aSmartObj        
        
        
        ## R2 table ###
        aSmartObj<-SmartTable$new(self$results$main$r2,runner_machine)
        aSmartObj$spaceBy<-"model"
        ladd(private$.smartObjs)<- aSmartObj        
        
        ### additional fit table ###
        aSmartObj<-SmartTable$new(self$results$main$fit,runner_machine)
        ladd(private$.smartObjs)<- aSmartObj        
        
        ### anova table ###
        aSmartObj<-SmartTable$new(self$results$main$anova,runner_machine)
        aSmartObj$activated<-(self$options$model_type!="multinomial")
        ladd(private$.smartObjs)<- aSmartObj        
        
        ### estimates table ###
        aSmartObj<-SmartTable$new(self$results$main$coefficients,runner_machine)
        aSmartObj$ci("est",self$options$ci_width)
        aSmartObj$ci("expb",width=self$options$ci_width,label="Exp(B)")
        aSmartObj$spaceBy<-"response"
        if (is.something(self$options$factors))
          aSmartObj$setColumnVisible<-"label"
        ladd(private$.smartObjs)<- aSmartObj        
        
        ### contrasts code tables
        aSmartObj<-SmartArray$new(self$results$main$contrastCodeTables,runner_machine)
        aSmartObj$expandOnInit<-TRUE
        ladd(private$.smartObjs)<- aSmartObj        
        
        ### relativerisk tables
        aSmartObj<-SmartTable$new(self$results$main$relativerisk,runner_machine)
        aSmartObj$ci("est",width=self$options$ci_width)
        ladd(private$.smartObjs)<- aSmartObj        
        
        ### marginal effects tables
        aSmartObj<-SmartTable$new(self$results$main$marginals,runner_machine)
        aSmartObj$ci("est",width=self$options$ci_width)
        ladd(private$.smartObjs)<- aSmartObj        
        
        
        ### random variances table
        aSmartObj<-SmartTable$new(self$results$main$random,runner_machine)
        aSmartObj$ci("var",self$options$ci_width)
        aSmartObj$activated<-(self$options$model_type!="multinomial" | self$options$.caller!="glmer")
        ladd(private$.smartObjs)<- aSmartObj        
        
        ### multinomial random variances table
        aSmartObj<-SmartArray$new(self$results$main$multirandom,runner_machine)
        aSmartObj$activated<-(self$options$model_type=="multinomial" & self$options$.caller=="glmer")
        aSmartObj$expandOnRun<-TRUE
        aSmartObj$expandFrom<-3
        ladd(private$.smartObjs)<-aSmartObj
        
        
        
        ### random covariances table
        aSmartObj<-SmartTable$new(self$results$main$randomcov,runner_machine)
        aSmartObj$activateOnData<-TRUE
        ladd(private$.smartObjs)<- aSmartObj        
        
        ### random variances lrt table
        aSmartObj<-SmartTable$new(self$results$main$ranova,runner_machine)
        ladd(private$.smartObjs)<- aSmartObj        
        
        
        ### estimate marginal means
        
        aSmartObj<-SmartArray$new(self$results$emmeans,runner_machine)
        aSmartObj$activated<-is.something(self$options$emmeans)
        aSmartObj$expandOnInit<-TRUE
        aSmartObj$combineBelow="new!"
        aSmartObj$spaceBy="new!"
        aSmartObj$ci("est",self$options$ci_width)
        ladd(private$.smartObjs)<- aSmartObj        
        
        ### simple effects
        ##### anova
        aSmartObj<-SmartTable$new(self$results$simpleEffects$anova,runner_machine)
        aSmartObj$activated<-(is.something(self$options$simple_x) & is.something(self$options$simple_mods))
        aSmartObj$expandOnInit<-TRUE
        aSmartObj$expandSuperTitle<-"Moderator"
        aSmartObj$key<-self$options$simple_x
        aSmartObj$combineBelow<-1:(length(self$options$simple_mods)-1)
        aSmartObj$spaceBy<-(length(self$options$simple_mods)-1)
        
        ladd(private$.smartObjs)<- aSmartObj        
        
        ##### coefficients
        aSmartObj<-SmartTable$new(self$results$simpleEffects$coefficients,runner_machine)
        aSmartObj$activated<-(is.something(self$options$simple_x) & is.something(self$options$simple_mods))
        aSmartObj$expandOnInit<-TRUE
        aSmartObj$expandFrom<-2
        aSmartObj$expandSuperTitle<-"Moderator"
        aSmartObj$key<-self$options$simple_x
        aSmartObj$ci("est",self$options$ci_width)
        aSmartObj$ci("expb",width=self$options$ci_width,format="Exp(B) {}% Confidence Intervals")
        aSmartObj$combineBelow<-1:(length(self$options$simple_mods)-1)
        aSmartObj$spaceBy<-(length(self$options$simple_mods)-1)
        ladd(private$.smartObjs)<- aSmartObj        
        
        ### simple interaction
        aSmartObj<-SmartArray$new(self$results$simpleInteractions,runner_machine)
        aSmartObj$activated<-(self$options$simple_interactions & is.something(self$options$simple_x) & length(self$options$simple_mods)>1)
        aSmartObj$expandOnInit<-TRUE
        aSmartObj$expandSuperTitle<-"Moderator"
        aSmartObj$ci("est",self$options$ci_width)
        aSmartObj$combineBelow<-"new!"
        aSmartObj$spaceBy<-"new!"
        ladd(private$.smartObjs)<- aSmartObj        
        

        ## post hoc #####
        
        aSmartObj<-SmartArray$new(self$results$posthoc,runner_machine)
        aSmartObj$expandOnInit<-TRUE
        aSmartObj$expandSuperTitle<-"Comparison"
        aSmartObj$ci("est",self$options$ci_width)
        aSmartObj$combineBelow<-"response"
        aSmartObj$expandFrom<-2
        aSmartObj$setColumnTitle("estimate",runner_machine$infomatic$comparison)
        ladd(private$.smartObjs)<- aSmartObj        
        
        
        ### init all ####
        
        
        
        for (tab in private$.smartObjs) {
          tab$initTable()
        }
        
        
        private$.data_machine<-data_machine
        private$.runner_machine<-runner_machine
        
        ######## plotting class #######
        plotter_machine<-Plotter$new(self$options,runner_machine,self$results)
        plotter_machine$initPlots()
        private$.plotter_machine<-plotter_machine
        self$results$plotnotes$setContent("")
        
      },
      .run=function() {
        ginfo("MODULE:  #### phase run ####")
        
        private$.ready<-readiness(self$options)
        if (!private$.ready$ready) {
          return()
        }

        data<-private$.data_machine$cleandata(self$data)
        private$.runner_machine$estimate(data)
        
        ### run tables ###
        for (smarttab in private$.smartObjs) {
          smarttab$runTable()
        }
        
        
        private$.checkpoint()
        
        #save model preds and resids            
        private$.runner_machine$savePredRes(self$results) 
        
        ### do plots 
        private$.plotter_machine$preparePlots()
        
        
        private$.checkpoint()
        
        ### save the model if we are in R ###
        if (self$options$.interface=="r")
          self$results$.setModel(private$.runner_machine$model)
        
        ginfo("MODULE:  #### phase end ####")
        now<-Sys.time()
        ginfo("TIME:",now-private$.time," secs")
        
        return()
        
      },
      
      .mainPlot=function(image, ggtheme, theme, ...) {
        
        plot<-private$.plotter_machine$scatterPlot(image)
        plot<-plot + ggtheme
        
        return(plot)
        
      },

      

      .marshalFormula= function(formula, data, name) {
        
        fixed<-lme4::nobars(formula)
        bars<-lme4::findbars(formula)
        rterms<-lapply(bars,all.vars)
        rvars<-unlist(sapply(rterms,function(a) if (length(a)>1) a[[length(a)-1]]))
        if (name=="dep")
          return(jmvcore::marshalFormula(fixed,data,from = "lhs"))  
        if (name=="factors") {
          ffactors<-jmvcore::marshalFormula(fixed,data,from='rhs',type='vars',permitted='factor')
          rfactors<-unlist(lapply(rvars, function(a) {if (is.factor(data[[a]])) a}))
          return(c(ffactors,rfactors))
        }
        if (name=="covs") {
          fcovs<-jmvcore::marshalFormula(fixed,data,from='rhs',type='vars',permitted='numeric')
          rcovs<-unlist(lapply(rvars, function(a) {if (is.numeric(data[[a]])) a}))
          return(c(fcovs,rcovs))
        }
        if (name=="cluster") {
          return(sapply(rterms,function(a) a[[length(a)]] ))
        }
        if (name=="randomTerms") {
          bars<-lme4::findbars(formula)
          fullist<-list()
          for (b in seq_along(bars)) {
            cluster=bars[[b]][[3]]
            bar<-strsplit(as.character(bars[[b]])[[2]],"+",fixed=T)[[1]]
            barlist<-list()
            j<-0
            for (term in bar) {
              term<-trimws(jmvcore::decomposeTerm(term))
              if (length(term)==1 && term=="1")
                term="Intercept"
              if (length(term)==1 && term=="0")
                next()              
              alist<-c(term,as.character(cluster))
              j<-j+1
              barlist[[j]]<-alist
            }
            fullist[[b]]<-barlist
          }
          return(fullist)
        }
        
        if (name=="modelTerms") {
          return(jmvcore::marshalFormula(fixed,data,from='rhs',type='terms'))
        }
        
      },
      
      
      .formula=function() {
        
        private$.runner_machine$formula
        
      },
      
      .sourcifyOption = function(option) {
        
        
        if (option$name=="nested_re") 
          return('')
        
        if (option$name=="nested_terms") 
          return('')
        
        if (option$name=="comparison") 
          if (self$options$comparison) {
            form<-stats::as.formula(fromb64(private$.runner_machine$nested_formula64))  
            return(paste0("nested_model = ",deparse(form)))
          }
        
        
        defaults<-c(covs_scale="centered",contrasts="simple",scale_missing="complete")
        if (option$name %in% NO_R_OPTS)
          return('')
        sourcifyOption(option,defaults)
        
      }
    )
  )
  
  
  
