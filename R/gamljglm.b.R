gamljglmClass <- R6::R6Class(
  "gamljglmClass",
  inherit = gamljglmBase,
  private=list(
    .data_machine=NULL,
    .runner_machine=NULL,
    .plotter_machine=NULL,
    .ready=NULL,
    .time=NULL,
    .smartObjs=list(),
    .init=function() {
      
      jinfo(paste("MODULE:",self$options$.caller,self$options$model_type,"  #### phase init  ####"))
      class(private$.results) <- c('gamlj', class(private$.results))
      private$.time<-Sys.time()

      private$.ready<-readiness(self$options)
      if (!private$.ready$ready) {
        if(private$.ready$report)
          self$results$info$addRow("info",list(info="Setup",specs=private$.ready$reason))
        return()
      }

      ### set up the R6 workhorse class
      data_machine            <-  Datamatic$new(self)
      runner_machine          <-  Runner$new(self,data_machine)
      runner_machine$storage  <-  self$results$main$coefficients      
      
      
      ### info table ###
      aSmartObj<-SmartTable$new(self$results$info,runner_machine)
      ladd(private$.smartObjs)<-aSmartObj
      
      
      ## R2 table ###
      aSmartObj<-SmartTable$new(self$results$main$r2,runner_machine)
      aSmartObj$hideOn<-list(ar2=NA)
      ladd(private$.smartObjs)<-aSmartObj
      
      ### some models do not have adjusted r2 ###
      if (self$options$model_type %in% c("multinomial","ordinal", "custom"))
             self$results$main$r2$getColumn("ar2")$setVisible(FALSE)
      
      
      ### additional fit table ###
      aSmartObj<-SmartTable$new(self$results$main$fit,runner_machine)
      ladd(private$.smartObjs)<-aSmartObj
      
      ### anova table ###
      aSmartObj<-SmartTable$new(self$results$main$anova,runner_machine)
      ladd(private$.smartObjs)<-aSmartObj
      
      ### estimates table ###
      aSmartObj<-SmartTable$new(self$results$main$coefficients,runner_machine)
      aSmartObj$ci("est",self$options$ci_width)
      aSmartObj$ci("expb",width=self$options$ci_width,label="Exp(B)")
      if (is.something(self$options$factors) || self$options$model_type=="ordinal")
         aSmartObj$setColumnVisible<-"label"
      aSmartObj$spaceBy<-"response"
      ladd(private$.smartObjs)<-aSmartObj

      ### predictors for phi in beta regression ###
      aSmartObj<-SmartTable$new(self$results$main$phi,runner_machine)
      aSmartObj$ci("est",self$options$ci_width)
      if (is.something(self$options$factors))
        aSmartObj$setColumnVisible<-"label"
      ladd(private$.smartObjs)<-aSmartObj
      
      ### contrasts code tables
      aSmartObj<-SmartArray$new(self$results$main$contrastCodeTables,runner_machine)
      aSmartObj$expandOnInit<-TRUE
      ladd(private$.smartObjs)<-aSmartObj
      
      
      ### relativerisk tables
      aSmartObj<-SmartTable$new(self$results$main$relativerisk,runner_machine)
      aSmartObj$ci("est",width=self$options$ci_width)
      ladd(private$.smartObjs)<-aSmartObj

      ### marginal effects tables
      aSmartObj<-SmartTable$new(self$results$main$marginals,runner_machine)
      aSmartObj$ci("est",width=self$options$ci_width)
      ladd(private$.smartObjs)<-aSmartObj
      
      ### paralleltest tables
      aSmartObj<-SmartTable$new(self$results$main$paralleltest,runner_machine)
      ladd(private$.smartObjs)<-aSmartObj

      ### vcov table ###
      
      aSmartObj<-SmartTable$new(self$results$main$vcov,runner_machine)
      aSmartObj$expandOnInit<-TRUE
      aSmartObj$expandFrom<-2
      ladd(private$.smartObjs)<-aSmartObj
      
      ## post hoc #####
      
      aSmartObj<-SmartArray$new(self$results$posthoc,runner_machine)
      aSmartObj$expandOnInit<-TRUE
      aSmartObj$expandSuperTitle<-"Comparison"
      aSmartObj$ci("est",self$options$ci_width)
      aSmartObj$combineBelow<-"response"
      aSmartObj$expandFrom<-2
      aSmartObj$setColumnTitle("estimate",runner_machine$infomatic$comparison)
      ladd(private$.smartObjs)<-aSmartObj
      

      ### estimate marginal means
      
      aSmartObj<-SmartArray$new(self$results$emmeans,runner_machine)
      aSmartObj$activated<-is.something(self$options$emmeans)
      aSmartObj$expandOnInit<-TRUE
      aSmartObj$expandFrom<-2
      aSmartObj$combineBelow="new!"
      aSmartObj$spaceBy="new!"
      aSmartObj$ci("est",self$options$ci_width)
      ladd(private$.smartObjs)<-aSmartObj
      
      ### simple effects
      ##### anova
      aSmartObj<-SmartTable$new(self$results$simpleEffects$anova,runner_machine)
      aSmartObj$activated<-(is.something(self$options$simple_x) & is.something(self$options$simple_mods))
      aSmartObj$expandOnInit<-TRUE
      aSmartObj$expandSuperTitle<-"Moderator"
      aSmartObj$key<-self$options$simple_x
      aSmartObj$combineBelow<-1:(length(self$options$simple_mods)-1)
      aSmartObj$spaceBy<-(length(self$options$simple_mods)-1)
      
      ladd(private$.smartObjs)<-aSmartObj
      
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
      ladd(private$.smartObjs)<-aSmartObj
      
      ### simple interaction
      aSmartObj<-SmartArray$new(self$results$simpleInteractions,runner_machine)
      aSmartObj$activated<-(self$options$simple_interactions & is.something(self$options$simple_x) & length(self$options$simple_mods)>1)
      aSmartObj$expandOnInit<-TRUE
      aSmartObj$expandSuperTitle<-"Moderator"
      aSmartObj$ci("est",self$options$ci_width)
      aSmartObj$combineBelow<-"new!"
      aSmartObj$spaceBy<-"new!"
      ladd(private$.smartObjs)<-aSmartObj
      
      ### init all ####
      for (tab in private$.smartObjs) {
        tab$initTable()
        tab$mutenotes<-self$options$mute
        
      }
      
      
      private$.data_machine<-data_machine
      private$.runner_machine<-runner_machine
      
      ######## plotting class #######
      plotter_machine<-Plotter$new(self,runner_machine)
      plotter_machine$initPlots()
      private$.plotter_machine<-plotter_machine
      self$results$plotnotes$setContent("")
      
      now<-Sys.time()
      jinfo("INIT TIME:",now-private$.time," secs")
      
    },
    .run=function() {
      
      runnow<-Sys.time()
      
      jinfo("MODULE:  #### phase run ####")
      
      if (self$options$donotrun) return()
        
      private$.ready<-readiness(self$options)
      if (!private$.ready$ready) {
        return()
      }
      
      data<-private$.data_machine$cleandata(self$data)
      private$.runner_machine$estimate(data)
      
      ### run tables ###
      for (smarttab in private$.smartObjs)
        smarttab$runTable()
      

      private$.checkpoint()
      
      ### do plots 
      private$.plotter_machine$preparePlots()
      
      
      #save model preds and resids            
      private$.runner_machine$savePredRes(self$results) 
      
      private$.checkpoint()
      
      if (self$options$.interface=="R") 
        self$results$.setModel(private$.runner_machine$model)
      
      
      jinfo("MODULE:  #### phase end ####")
      
      jinfo("RUN TIME:",Sys.time()-runnow," secs")
      
      jinfo("TIME:",Sys.time()-private$.time," secs")
      

    },

    .mainPlot=function(image, ggtheme, theme, ...) {
      
      if (is.something(private$.runner_machine$errors))
          return()
      if (!is.something(image$key))
        return()

        plot<-private$.plotter_machine$scatterPlot(image,ggtheme,theme)
        

      return(plot)
      
    },
    
    


.formula=function() {
  jmvcore:::composeFormula(self$options$dep, self$options$model_terms)
},
.sourcifyOption = function(option) {
  
  skip<-c("model_terms","factors","covs","dep")
  defaults<-c(covs_scale="centered",contrasts="simple")
  if (option$name %in% skip)
    return('')
  sourcifyOption(option,defaults)
  
}

  )


)


