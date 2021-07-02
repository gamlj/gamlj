
gamljMixedClass <- R6::R6Class(
  "gamljMixedClass",
  inherit=gamljMixedBase,
  private=list(
    .ready=NULL,
    .data_machine=NULL,
    .estimate_machine=NULL,
    .plotter_machine=NULL,
    .init=function() {
      
      ginfo("init")
      
      class(private$.results) <- c('gamlj', class(private$.results))
      
      private$.ready<-readiness(self$options)
      if (!private$.ready$ready) {
        if(private$.ready$report)
          self$results$info$addRow("info",list(info="Setup",specs=private$.ready$reason))
        return()
      }
      
      ### set up the R6 workhorse class
      
      data_machine<-Datamatic$new(self$options,self$data)
      estimate_machine<-Estimate$new(self$options,data_machine)
      plotter_machine<-Plotter$new(estimate_machine,self$results)
      
      ### info table ###
      j.init_table(self$results$info,estimate_machine$tab_info) 
      j.add_warnings(self$results$info,estimate_machine,"tab_info")
      
      ### r2 table  ###
      j.init_table(self$results$main$r2,estimate_machine$tab_r2)
      
      ### additional indices table ###
      j.init_table(self$results$main$fit,estimate_machine$tab_fit)
      
      ### anova table ###
      j.init_table(self$results$main$anova,estimate_machine$tab_anova) 
      
      ### estimates table ###
      j.init_table(self$results$main$coefficients,estimate_machine$tab_coefficients, ci=T,ciwidth=self$options$ciWidth)
      
      if (!is.something(self$options$factors))
        self$results$main$coefficients$getColumn('label')$setVisible(FALSE)

      ### random component table ###
      j.init_table(self$results$main$random,estimate_machine$tab_random, ci=T,ciwidth=self$options$ciWidth)
      
      

      ### estimate marginal means
      
      if (is.something(self$options$emmeans)) {
        
        self$results$emmeans$setVisible(TRUE)
        for (i in seq_along(self$options$emmeans)) {
          term<-self$options$emmeans[[i]]
          spaceby=NULL
          if (length(term)>1) spaceby<-1
          
          aTable <- self$results$emmeans$addItem(key = jmvcore::stringifyTerm(term))
          j.expand_table(aTable,term)
          j.init_table(aTable,estimate_machine$tab_emmeans[[i]], ci=T,ciwidth=self$options$ciWidth,spaceby = spaceby)
        } 
        
      }
      
      ### simple effects ####
      if (is.something(estimate_machine$tab_simpleAnova)) {
        
        j.expand_table(self$results$simpleEffects$anova,self$options$simpleModerators) 
        title<-paste("Omnibus test for variable",self$options$simpleVariable)
        j.init_table(self$results$simpleEffects$anova,estimate_machine$tab_simpleAnova,title=title)
        title<-paste("Parameter estimates for variable",self$options$simpleVariable)
        j.expand_table(self$results$simpleEffects$coefficients,self$options$simpleModerators,superTitle = "Moderators") 
        j.init_table(self$results$simpleEffects$coefficients,estimate_machine$tab_simpleCoefficients, ci=T,ciwidth=self$options$ciWidth,title=title)
        
      }
      
      ### simple interactions ######
      
      if (is.something(estimate_machine$tab_simpleInteractionCoefficients)) {
        
        self$results$simpleInteractions$setVisible(TRUE)
        ### moderators should be reverted in order so they make sense
        terms<-c(self$options$simpleVariable,self$options$simpleModerators)
        for (i in seq_along(estimate_machine$tab_simpleInteractionCoefficients)) {
          aGroup <- self$results$simpleInteractions$addItem(key = i)
          aTable<-aGroup$interactionCoefficients
          term<-setdiff(terms,estimate_machine$tab_simpleInteractionCoefficients[[i]])
          j.expand_table(aTable,estimate_machine$tab_simpleInteractionCoefficients[[i]],superTitle="Moderator")
          title<-paste("Parameter Estimates for simple interaction",  jmvcore::stringifyTerm(term))
          j.init_table(aTable,FALSE, ci=T,ciwidth=self$options$ciWidth,title=title)
          
          aTable<-aGroup$interactionAnova
          j.expand_table(aTable,estimate_machine$tab_simpleInteractionAnova[[i]],superTitle="Moderator")
          title<-paste("ANOVA test for simple interaction",  jmvcore::stringifyTerm(term))
          j.init_table(aTable,FALSE, ci=F,title=title)
          
          title<-paste("Simple interaction:",  jmvcore::stringifyTerm(term))
          aGroup$setTitle(title)
          
        } 
        
        
      }
      
      ### posthoc ####
      
      if (is.something(self$options$posthoc)) {
        
        for (i in seq_along(self$options$posthoc)) {
          term<-self$options$posthoc[[i]]
          aTable<-self$results$posthoc$get(key = term)
          aTable$setTitle(paste0("Post Hoc Comparisons - ", jmvcore::stringifyTerm(term)))
          j.expand_table(aTable,names(estimate_machine$tab_posthoc[[i]]),superTitle="Comparison",names64=FALSE)
          j.init_table(aTable,estimate_machine$tab_posthoc[[i]],ci=T,ciwidth = self$options$ciWidth)
        } 
        
      }
      
      if (is.something(estimate_machine$tab_contrastcodes)) {
        
        for (i in seq_along(self$options$factors)) {
          term<-self$options$factors[[i]]
          aTable<-self$results$main$contrastCodeTables$get(key = term)
          eTable<-estimate_machine$tab_contrastcodes[[i]]
          levels_pos<-seq_len(length(names(eTable))-2)
          j.expand_table(aTable,names(eTable)[levels_pos],append=T,type="number",names64=FALSE) 
          names(eTable)[levels_pos]<-make.names(names(eTable)[levels_pos])
          j.init_table(aTable,eTable)
        }
      }
      plotter_machine$initPlots()
      
      private$.data_machine<-data_machine
      private$.estimate_machine<-estimate_machine
      private$.plotter_machine<-plotter_machine
      
      
      
      
      
      emmeans::emm_options(lmerTest.limit = 25000)  
      #### info table #####
      # infoTable<-self$results$info
      # infoTable$addRow(rowKey="est",list(info="Estimate"))
      # infoTable$addRow(rowKey="call",list(info="Call"))
      # infoTable$addRow(rowKey="aic",list(info="AIC"))
      # infoTable$addRow(rowKey="bic",list(info="BIC"))
      # infoTable$addRow(rowKey="log",list(info="LogLikel."))
      # infoTable$addRow(rowKey="r2m",list(info="R-squared Marginal"))
      # infoTable$addRow(rowKey="r2c",list(info="R-squared Conditional"))
      # infoTable$addRow(rowKey="conv",list(info="Converged"))
      # infoTable$addRow(rowKey="opt",list(info="Optimizer"))
      # 
      # 
      # ## random table
      # aTable<-self$results$main$random
      # aTable$addRow(rowKey="res",list(groups="Residuals",name=""))


    },
    .run=function() {
      ginfo("run")
      
      private$.ready<-readiness(self$options)
      if (!private$.ready$ready) {
        return()
      }
      
      
      data<-private$.data_machine$cleandata(self$data)
      private$.estimate_machine$estimate(data)
      

      
      
      ## info table and posssible main problems to report
      j.fill_table(self$results$info,private$.estimate_machine$tab_info) 
      
      j.add_warnings(self$results$info,private$.data_machine,"data")
      j.add_warnings(self$results$info,private$.estimate_machine,"info",reset=TRUE)
      
      ## R2 table ###
      j.fill_table(self$results$main$r2,private$.estimate_machine$tab_r2)
      j.add_warnings(self$results$main$r2,private$.estimate_machine,"tab_r2")
      
      ## additional fit indices table ###
      
      j.fill_table(self$results$main$fit,private$.estimate_machine$tab_fit)
      j.add_warnings(self$results$main$fit,private$.estimate_machine,"tab_fit")
      
      ## anova table ###
      j.fill_table(self$results$main$anova,private$.estimate_machine$tab_anova)
      j.add_warnings(self$results$main$anova,private$.estimate_machine,"tab_anova")


      ### parameter estimates
      j.fill_table(self$results$main$coefficients,private$.estimate_machine$tab_coefficients)
      j.add_warnings(self$results$main$coefficients,private$.estimate_machine,"tab_coefficients")
      
      ## random table ###
      j.fill_table(self$results$main$random,private$.estimate_machine$tab_random)
      j.add_warnings(self$results$main$random,private$.estimate_machine,"tab_random")
      j.fill_table(self$results$main$randomCov,private$.estimate_machine$tab_randomCov,append = T)
      
      j.fill_table(self$results$main$randomTests,private$.estimate_machine$tab_randomTests,append = T)
      j.add_warnings(self$results$main$randomTests,private$.estimate_machine,"tab_randomTests")
      
            
            
      ### post hoc
      
      if (is.something(private$.estimate_machine$tab_posthoc)) {
        
        for (i in seq_along(self$options$posthoc)) {
          term<-self$options$posthoc[[i]]
          aTable<-self$results$posthoc$get(key = term)
          j.fill_table(aTable,private$.estimate_machine$tab_posthoc[[i]])
        } 
      }
      
      ### simple effects
      j.fill_table(self$results$simpleEffects$anova,private$.estimate_machine$tab_simpleAnova)
      j.add_warnings(self$results$simpleEffects$anova,private$.estimate_machine,"tab_simpleAnova")
      
      j.fill_table(self$results$simpleEffects$coefficients,private$.estimate_machine$tab_simpleCoefficients)
      j.add_warnings(self$results$simpleEffects$coefficients,private$.estimate_machine,"tab_simpleCoefficients")
      
      ### simple interactions
      if (is.something(private$.estimate_machine$tab_simpleInteractionCoefficients)) {
        
        for (i in seq_along(private$.estimate_machine$tab_simpleInteractionCoefficients)) {
          aGroup <- self$results$simpleInteractions$get(key = i)
          aTable<-aGroup$coefficients
          j.fill_table(aTable,private$.estimate_machine$tab_simpleInteractionCoefficients[[i]],append = T)
          aTable<-aGroup$anova
          j.fill_table(aTable,private$.estimate_machine$tab_simpleInteractionAnova[[i]],append = T)
          
          
        } 
      }
      
      ###  emmeans 
      
      if (is.something(private$.estimate_machine$tab_emmeans)) {
        
        for (i in seq_along(self$options$emmeans)) {
          term<-self$options$emmeans[[i]]
          aTable<-self$results$emmeans$get(key = jmvcore::stringifyTerm(term))
          j.fill_table(aTable,private$.estimate_machine$tab_emmeans[[i]])
          j.add_warnings(aTable,private$.estimate_machine,"tab_emmeans")
        } 
      }
      
      #save model preds and resids            
      private$.estimate_machine$savePredRes(self$results) 
      
      private$.plotter_machine$preparePlots()
      j.add_warnings(self$results$plotnotes,private$.plotter_machine,"plot")
      
        
        
    },
    
    .mainPlot=function(image, ggtheme, theme, ...) {
      
      if (is.something(private$.estimate_machine$errors))
        return()
      if (!is.something(image$key))
        return()
      
      plot<-private$.plotter_machine$scatterPlot(image)
      plot<-plot + ggtheme
      
      return(plot)
      
    },
    
    

.qqPlot=function(image, ggtheme, theme, ...) {

  if (!self$options$qq)
    return()
  
  model<-private$.model      
  if (!is.something(model) )
    return(FALSE)
  
  residuals <- as.numeric(scale(residuals(model)))
  df <- as.data.frame(qqnorm(residuals, plot.it=FALSE))
  plot<-ggplot2::ggplot(data=df, aes(y=y, x=x)) +
          geom_abline(slope=1, intercept=0, colour=theme$color[1]) +
          geom_point(aes(x=x,y=y), size=2, colour=theme$color[1]) +
          xlab("Theoretical Quantiles") +
          ylab("Standardized Residuals") +ggtheme
  
  plot
},
.normPlot=function(image, ggtheme, theme, ...) {
  if (!self$options$normPlot)
    return()
  model<-private$.model      
  if (!is.something(model) )
    return(FALSE)
  plot<-gplots.normPlot(model,ggtheme,theme)
  return(plot)
},

.residPlot=function(image, ggtheme, theme, ...) {

  if (!self$options$residPlot)
    return()
  model<-private$.model      
  if (!is.something(model) )
    return(FALSE)
  plot<-gplots.residPlot(model,ggtheme,theme)
  
  return(plot)
},

.prepareClusterBoxplot=function() {
  
  if (!self$options$clusterBoxplot)
    return()
  
  model<-private$.model
  if (!is.something(model))
    return()
  n64<-private$.names64
  clusters64<-names(model@cnms)
  image<-self$results$assumptions$clusterBoxplot
  for (cluster in clusters64) {
    label<-n64$nicenames(cluster)
    title<-paste("Clustering variable:",jmvcore::fromB64(cluster))
    id<-cluster
    image$addItem(id)
    image$get(key=id)$setTitle(title)
    image$get(key=id)$setState(list(cluster=cluster,label=label))
    }
},


.clusterBoxplot=function(image, ggtheme, theme, ...) {
  
  if (!self$options$clusterBoxplot)
    return()
  
  model<-private$.model      
  if (!is.something(model) )
    return(FALSE)
  label<-image$state$label
  cluster<-image$state$cluster
  fmodel<-lme4::fortify.merMod(model)
  plot<-ggplot(fmodel, aes_string(cluster,".resid")) + geom_boxplot() + coord_flip()
  plot<-plot+xlab(jmvcore::fromB64(cluster))+ylab("Residuals")
  plot<-plot+ ggtheme 
  note<-self$results$plotnotes
  note$setContent(paste('<i>Note</i>: Residuals plotted by',jmvcore::fromB64(cluster)))
  note$setVisible(TRUE)
  
  return(plot)
},

.prepareRandHist=function() {
  
  if (!self$options$randHist)
    return()
  
  model<-private$.model
  if ((!self$options$randHist) | !is.something(model))
     return()
  n64<-private$.names64
  res<-lme4::ranef(model)
  clusters64<-names(res)
  image<-self$results$assumptions$randHist
  for (cluster in clusters64) {
       clusterres<-res[[cluster]]
       vars<-names(clusterres)
       for (v in vars) {
           data<-data.frame(clusterres[,v])
           names(data)<-"x"
           label<-n64$nicenames(v)
           title<-paste("Coefficient",label," random across",jmvcore::fromB64(cluster))
           id<-paste0(v,cluster)
           image$addItem(id)
           image$get(key=id)$setTitle(title)
           image$get(key=id)$setState(list(data=data,label=label))
       }

  }


},

.randHist=function(image, ggtheme, theme, ...) {

  if (!self$options$randHist)
    return()
  
  label<-image$state$label
  data<-image$state$data
  fill <- theme$fill[2]
  color <- theme$color[1]
  alpha <- 0.4
  plot <- ggplot(data=data, aes(x=x)) +
    labs(x="Coefficients", y='density')
  
  plot <- plot + geom_histogram(aes(y=..density..), position="identity",
                                stat="bin", color=color, fill=fill)
  plot <- plot + stat_function(fun = dnorm, args = list(mean = mean(data$x), sd = sd(data$x)))  
  
  themeSpec <- theme(axis.text.y=element_blank(),
                     axis.ticks.y=element_blank())
  plot <- plot + ggtheme + themeSpec
  
  
  return(plot)
},



.populateNormTest=function(model) {
  
  if ( ! self$options$normTest)
    return()
  table <- self$results$get('assumptions')$get('normTest')
  
  rr<-residuals(model)
  ks<-ks.test(rr,"pnorm",mean(rr),sd(rr))
  table$setRow(rowNo=1, values=list(test="Kolmogorov-Smirnov",stat=ks$statistic,p=ks$p.value))
  
  st<-try(shapiro.test(rr))
  if (jmvcore::isError(st)) {
    table$setNote("noshapiro","Shapiro-Wilk not available due to too large number of cases")
    table$setRow(rowNo=2, values=list(test="Shapiro-Wilk",stat="",p=""))
  }
  else
    table$setRow(rowNo=2, values=list(test="Shapiro-Wilk",stat=st$statistic,p=st$p.value))
  
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


.formula = function() {

  private$.estimate_machine$formula
  
},
.sourcifyOption = function(option) {
  
  name <- option$name
  value <- option$value
  
  if (!is.something(value))
    return('')

  
  if (option$name %in% c('factors', 'dep', 'covs', 'cluster', 'modelTerms','randomTerms'))
    return('')
  
  if (name =='scaling') {
    vec<-sourcifyList(option,"centered")
    return(vec)
  }
  if (name =='contrasts') {
    vec<-sourcifyList(option,"simple")
    return(vec)
  }
  if (name == 'postHoc') {
    if (length(value) == 0)
      return('')
  }
  
    
  super$.sourcifyOption(option)
}
))

jmvcore::sourcify
