gamljlmClass <- R6::R6Class(
  "gamlj_lm_Class",
  inherit = gamljlmBase,
  private=list(
    .dispatcher=NULL,
    .data_machine=NULL,
    .runner_machine=NULL,
    .plotter_machine=NULL,
    .ready=NULL,
    .time=NULL,
    .smartObjs=list(),
    .init=function() {
      ginfo(paste("MODULE:",self$options$.caller,"  #### phase init  ####"))
      class(private$.results) <- c('gamlj', class(private$.results))

      private$.time<-Sys.time()
      private$.ready<-readiness(self$options)
      if (!private$.ready$ready) {
        if(private$.ready$report)
          self$results$info$addRow("info",list(info="Setup",specs=private$.ready$reason))
        return()
      }
      
      ### set up the R6 workhorse class
      dispatcher<-Dispatch$new(self$results)
      data_machine<-Datamatic$new(self$options,dispatcher,self$data)
      runner_machine<-Runner$new(self$options,dispatcher,data_machine)
     

      ### info table ###
      aSmartObj<-SmartTable$new(self$results$info,runner_machine)
      ladd(private$.smartObjs)<-aSmartObj

     
      ## R2 table ###
      aSmartObj<-SmartTable$new(self$results$main$r2,runner_machine)
      ladd(private$.smartObjs)<-aSmartObj
      

      ### anova table ###
      aSmartObj<-SmartTable$new(self$results$main$anova,runner_machine)
      aSmartObj$spaceAt<-c(1,-2)
      ladd(private$.smartObjs)<-aSmartObj
      
      ### coefficents table ###
      aSmartObj<-SmartTable$new(self$results$main$coefficients,runner_machine)
      aSmartObj$ci("est",self$options$ci_width)
      aSmartObj$ci("beta",self$options$ci_width,label=greek_vector[["beta"]])
      if (is.something(self$options$factors))
            aSmartObj$setColumnVisible<-"label"
      
      ladd(private$.smartObjs)<-aSmartObj
      
      ### contrasts code tables
      aSmartObj<-SmartArray$new(self$results$main$contrastCodeTables,runner_machine)
      aSmartObj$expandOnInit<-TRUE
      ladd(private$.smartObjs)<-aSmartObj

      ### intercept more info table ###
      
      aSmartObj<-SmartTable$new(self$results$main$intercept,runner_machine)
      ladd(private$.smartObjs)<-aSmartObj
      
      ### effectsizes table ###

      aSmartObj<-SmartTable$new(self$results$main$effectsizes,runner_machine)
      aSmartObj$ci("est",self$options$ci_width)
      aSmartObj$spaceBy="effect"
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
      ladd(private$.smartObjs)<-aSmartObj
      
      aSmartObj<-SmartArray$new(self$results$posthocEffectSize,runner_machine)
      aSmartObj$activated<-(is.something(self$options$posthoc) & is.something(self$options$posthoc_es))
      aSmartObj$expandOnInit<-TRUE
      aSmartObj$expandSuperTitle<-"Comparison"
      aSmartObj$ci("dm",self$options$ci_width)
      aSmartObj$ci("ds",self$options$ci_width)
      aSmartObj$ci("g",self$options$ci_width)
#      aSmartObj$restNotes(self$options$dci==FALSE)
      ladd(private$.smartObjs)<-aSmartObj
      
      ### estimate marginal means
      
      aSmartObj<-SmartArray$new(self$results$emmeans,runner_machine)
      aSmartObj$activated<-is.something(self$options$emmeans)
      aSmartObj$expandOnInit<-TRUE
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
      aSmartObj$expandSuperTitle<-"Moderator"
      aSmartObj$key<-self$options$simple_x
      aSmartObj$ci("est",self$options$ci_width)
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
      
      ### assumptions hometest
      aSmartObj<-SmartTable$new(self$results$assumptions$homotest,runner_machine)
      ladd(private$.smartObjs)<-aSmartObj
      ### assumptions nromtest
      aSmartObj<-SmartTable$new(self$results$assumptions$normtest,runner_machine)
      ladd(private$.smartObjs)<-aSmartObj
      
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
      
      now<-Sys.time()
      ginfo("INIT TIME:",now-private$.time," secs")
      
    },
    .run=function() {
      ginfo("MODULE:  #### phase run ####")
      
      if (self$options$donotrun) return()
      
      private$.ready<-readiness(self$options)
      if (!private$.ready$ready) {
        return()
      }
      runnow<-Sys.time()
      data<-private$.data_machine$cleandata(self$data)
      private$.runner_machine$estimate(data)
      
      ### run tables ###
      
      for (smarttab in private$.smartObjs)
           smarttab$runTable()

      private$.checkpoint()
      
      #save model preds and resids            
      private$.runner_machine$savePredRes(self$results) 
      # 
      private$.plotter_machine$preparePlots()
      # 
      # if ("plot" %in% private$.plotter_machine$dispatcher$warnings_topics) {
      #     self$results$plotnotes$setContent(paste(private$.plotter_machine$dispatcher$warnings[["plot"]],collapse = "; "))
      #     self$results$plotnotes$setVisible(TRUE)
      # }  
      

     if (self$options$.interface=="R") 
       self$results$.setModel(private$.runner_machine$model)
      
      ginfo("MODULE:  #### phase end ####")
      
      ginfo("RUN TIME:",Sys.time()-runnow," secs")
      
      ginfo("TIME:",Sys.time()-private$.time," secs")
      
      return()
          
    },


.mainPlot=function(image, ggtheme, theme, ...) {

  plot<-private$.plotter_machine$scatterPlot(image)
  plot<-plot + ggtheme
  
  return(plot)
  
},



.qqPlot=function(image, ggtheme, theme, ...) {

  if (!private$.ready$ready) 
    return()
  
  plot<-private$.plotter_machine$qqplot(theme,ggtheme)
  return(plot)
  
},
.normPlot=function(image, ggtheme, theme, ...) {

  if (!private$.ready$ready) 
    return()
  
  plot<-private$.plotter_machine$normplot(theme,ggtheme)
  return(plot)
},

.residPlot=function(image, ggtheme, theme, ...) {

  if (!private$.ready$ready) 
    return()
  
  plot<-private$.plotter_machine$residPlot(theme,ggtheme)
  
  return(plot)
},

.formula=function() {
  
  private$.runner_machine$formula
  
},


.sourcifyOption = function(option) {

  skip<-c("modelTerms","factors","covs","dep")
  defaults<-c(scaling="centered",contrasts="simple")
  
  if (option$name %in% skip)
     return('')
 sourcifyOption(option,defaults)

}
)
)

