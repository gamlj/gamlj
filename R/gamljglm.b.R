library(ggplot2)

#' @import ggplot2
gamljGLMClass <- R6::R6Class(
  "gamljGLM",
  inherit = gamljGLMBase,
  private=list(
    .model=NA,
    .postHocRows=NA,
    .cleanData=function() {

        print("cleandata")      
        dep <- self$options$dep

        covs <- NULL
        if ('covs' %in% names(self$options))
            covs <- self$options$covs
      
        factors <- NULL
        if ('factors' %in% names(self$options))
           factors <- self$options$factors
      
        data <- self$data
 
        if ( ! is.null(dep))
           data[[dep]] <- jmvcore::toNumeric(data[[dep]])

        for (factor in factors) {
            data[[factor]] <- as.factor(data[[factor]])
        }
      
        for (contrast in self$options$contrasts) {
            levels <- base::levels(data[[contrast$var]])
            stats::contrasts(data[[contrast$var]]) <- .createContrasts(levels, contrast$type)
        }
      
        for (covariate in covs)
            data[[covariate]] <- jmvcore::toNumeric(data[[covariate]])
      

        for (scaling in self$options$scaling) 
            data[[scaling$var]]<-.scaleContinuous(data[[scaling$var]],scaling$type)

        na.omit(data)
    },
    .init=function() {
        
        print("init")      
       
        dep <- self$options$dep
        factors <- self$options$factors
        modelTerms <- private$.modelTerms()
        
        if (length(modelTerms) == 0 | is.null(dep))
            return()
        
        data <- private$.cleanData()

        anovaTable      <- self$results$main
        postHocTables   <- self$results$postHoc
        contrastsTables <- self$results$contrasts
        estimatesTable <- self$results$estimates
    
        # main table
      
        modelTerms <- private$.modelTerms()
      
        if (length(modelTerms) > 0) {
           anovaTable$addRow(rowKey="r2model", list(name="Model"))
           anovaTable$addFormat(col=1, rowNo=1, format=Cell.BEGIN_END_GROUP)
           
           for (term in modelTerms) {
               anovaTable$addRow(rowKey=term, list(name=jmvcore::stringifyTerm(term)))
           }  
        
           anovaTable$addFormat(col=1, rowNo=2,format=Cell.BEGIN_GROUP)
           anovaTable$addFormat(col=1, rowNo=length(modelTerms), format=Cell.END_GROUP)
           
           } else {
           
           anovaTable$addRow(rowKey='.', list(name='.'))
           anovaTable$addFormat(col=1, rowKey='.', format=Cell.BEGIN_END_GROUP)
           }
      
           anovaTable$addRow(rowKey='', list(name='Residuals'))
           anovaTable$addFormat(col=1, rowKey='', format=Cell.BEGIN_END_GROUP)
           anovaTable$setNote("r2",paste("R-squared= 0.000, adjusted R-squared= 0.000"))
           
      # estimates
           formula<-as.formula(jmvcore::constructFormula(dep, modelTerms))
           terms<-colnames(model.matrix(formula,data))  
           labels<-.getFormulaContrastsLabels(self$options$contrasts,formula,data) 
           for (i in seq_along(terms)) 
                estimatesTable$addRow(rowKey=i, list(name=.nicifyTerms(terms[i]),label=.nicifyTerms(labels[i])))
      
       # contrasts
      
           for (contrast in self$options$contrasts) {
               table <- contrastsTables$addItem(contrast)
               var <- data[[contrast$var]]
               
               if (contrast$type=="default")
                  contrast$type="deviation"
               
               levels <- base::levels(var)
               labels <- .contrastLabels(levels, contrast$type)
               dummies<-paste(contrast$var,1:length(labels),sep="")
               groups<-paste(1:length(levels),levels,sep = "=", collapse = ", ")

               for (i in 1:length(labels)) {
                    table$addRow(rowKey=labels[[i]], values=list(contrast=labels[[i]],term=dummies[[i]],groups=groups))
               }
        
           }
      ######### simple effects tables ##############
      
           variable<-self$options$simpleVariable
           moderator<-self$options$simpleModerator
           threeway<-self$options$simple3way
           if (!is.null(variable) & !is.null(moderator)) {
               # N dummies in X-axis
               xlevels<-length(levels(data[[variable]]))
               xlevels<-ifelse(xlevels>1,(xlevels-1),1)
               # N levels of moderator
               modlevels<-length(levels(data[[moderator]]))
               modlevels<-ifelse(modlevels>1,modlevels,3)
               nrows<-modlevels*xlevels  
               # create the tables with right rows
               simpleEffectsTables<-self$results$simpleEffects
               simpleEffectsAnovas<-self$results$simpleEffectsAnovas
               if (!is.null(threeway)) {
                  mod2levels<-length(levels(data[[threeway]]))
                  mod2levels<-ifelse(mod2levels>1,mod2levels,3)
                } else
                      mod2levels<-1
               
                for (j in 1:mod2levels) {
                    title<-paste("Simple effects of",variable)
                    key<-paste(variable,j,sep="")
          
                   ## init simple ANOVA tables
                   ftable<-simpleEffectsAnovas$addItem(key=key)
                   ftable$setTitle(title)
                   
                   for (i in 1:modlevels) 
                       ftable$addRow(rowKey=i,list(variable=" "))        
          
                   ## init simple parameters tables
                   ptable<-simpleEffectsTables$addItem(key=key)
                   ptable$setTitle(title)
                   for (i in 1:nrows) 
                       ptable$addRow(rowKey=i,list(variable=" "))        
          
                }  
      }  # end of simple effects tables
      # post hoc
      private$.initPostHoc(data)
      
      # descriptives
      
      descTable <- self$results$desc
      factorNames <- self$options$factors
      
      if (length(factorNames) > 0) {
        
        data <- select(data, rev(factorNames))
        al <- as.list(data)
        names(al) <- rev(paste0('f', seq_len(length(al))))
        ll <- sapply(al, base::levels, simplify=FALSE)
        ll$stringsAsFactors <- FALSE
        grid <- do.call(base::expand.grid, ll)
        grid <- rev(grid)
        
        for (i in seq_len(ncol(grid))) {
          colName <- colnames(grid)[[i]]
          descTable$addColumn(name=colName, title=factorNames[[i]], index=i)
        }
        
        for (rowNo in seq_len(nrow(grid))) {
          row <- grid[rowNo,]
          if ( ! is.list(row))
            row <- list(f1=row)
          descTable$addRow(rowKey=row, values=row)
        }
      }
      # descriptives plots
      private$.initDescPlots(data)
    },
    .run=function() {
      print(".run")
      suppressWarnings({
        
        dep <- self$options$dep
        factors <- self$options$factors
        covs<-self$options$covs
        modelTerms <- private$.modelTerms()
        
        if (is.null(dep) ||  length(modelTerms) == 0)
          return()
        
        base::options(contrasts = c("contr.sum","contr.poly"))
        
        data <- private$.cleanData()
        
        # data <- lapply(data, function(x) {
        #   if (is.factor(x))
        #     levels(x) <- toB64(levels(x))
        #   return(x)
        # })
        
        if (is.factor(data[[dep]]))
          reject('Dependent variable must be numeric')
        
        for (factorName in factors) {
          lvls <- base::levels(data[[factorName]])
          if (length(lvls) == 1)
            reject("Factor '{}' contains only a single level", factorName=factorName)
          else if (length(lvls) == 0)
            reject("Factor '{}' contains no data", factorName=factorName)
        }
        formula <- jmvcore::constructFormula(dep, modelTerms)
        formula <- stats::as.formula(formula)
        model <- stats::lm(formula, data)
        private$.model <- model
        self$results$.setModel(model)
        singular <- NULL
        
          
          results <- try({
            if (self$options$ss == '1') {
               ss<-stats::anova(model)
            }
            if (self$options$ss == '2') {
               ss<-car::Anova(model,type=2,singular.ok=TRUE)
            }
            if (self$options$ss == '3') {
               ss<-car::Anova(model,type=3,singular.ok=TRUE)
            }
          })
          if (isError(results)) {
            message <- extractErrorMessage(results)
            reject(message)
          }
          anoFrame<-private$.cleanAnova(ss,type=self$options$ss)
          anoTable<-private$.anovaTable(model,anoFrame)

        anovaTable <- self$results$main
        rowNames<-rownames(anoTable)
        rowNames[1]<-"Model"
        for (i in seq_along(rowNames)) {
          rowName <- rowNames[i]
          tableRow<-as.list(anoTable[i,])
          if (i==length(rowNames))
            tableRow[c("F","p","etaSq","etaSqP","omegaSq")]<-" "
          anovaTable$setRow(rowNo=i, tableRow)
          }
        ## residual variance
        indices<-private$.r2indices(model)
#        anovaTable$setRow(rowKey='',list(variable="Residuals",term="",ss=errSS,df=errdf,ms=errMS,F="",etaSqP="",etaSq="",omegaSq="",p=""))
        anovaTable$setNote("r2",paste("R-squared=",indices[[1]],", adjusted R-squared=",indices[[2]]))

        estimatesTable <- self$results$estimates
        
        suppressWarnings({
        eresults<-stats::summary.lm(private$.model)[['coefficients']]
#        eresults<-stats::summary.lm(mod)[['coefficients']]
        }) #end suppresseWarnings
        if ("beta" %in% self$options$effectSize)
        {
          zdata<-data
          zdata[[dep]]<-scale(zdata[[dep]])
          for (var in covs)
            zdata[[var]]<-scale(zdata[[var]])
          beta<-coef(stats::lm(formula,data=zdata))
          beta[1]<-0
        } else beta<-1:nrow(eresults)
        
        eresults<-cbind(eresults,beta) 
        labels<-.getFormulaContrastsLabels(self$options$contrasts,formula(model),data)
        for (i in 1:nrow(eresults)) {
          tableRow=eresults[i,]
          names(tableRow)<-c("estimate","std","t","p","beta")
          estimatesTable$setRow(rowNo=i,tableRow)
          estimatesTable$setRow(rowNo=i,list(label=.nicifyTerms(labels[i])))
          
          }
        if (mf.aliased(model)) {
          anovaTable$setNote("aliased",WARNS["ano.aliased"])    
          estimatesTable$setNote("aliased",WARNS["ano.aliased"])    
          
        }          
        
        private$.populateSimple(private$.model)
        private$.prepareDescPlots(private$.model)
        private$.populateLevenes(private$.model)
        private$.populatePostHoc(data)
        private$.populateDescriptives(data)
        
      }) # suppressWarnings
    },
    .estimate=function(form,data) {
       stats::lm(form,data)
     },
    .initPostHoc=function(data) {
      
      bs <- self$options$factors
      phTerms <- self$options$postHoc
      
      bsLevels <- list()
      for (i in seq_along(bs))
        bsLevels[[bs[i]]] <- levels(data[[bs[i]]])
      
      tables <- self$results$postHoc
      
      postHocRows <- list()
      
      for (ph in phTerms) {
        
        table <- tables$get(key=ph)
        
        table$setTitle(paste0('Post Hoc Comparisons - ', stringifyTerm(ph)))
        
        for (i in seq_along(ph))
          table$addColumn(name=paste0(ph[i],'1'), title=ph[i], type='text', superTitle='Comparison', combineBelow=TRUE)
        
        table$addColumn(name='sep', title='', type='text', content='-', superTitle='Comparison', format='narrow')
        
        for (i in seq_along(ph))
          table$addColumn(name=paste0(ph[i],'2'), title=ph[i], type='text', superTitle='Comparison')
        
        table$addColumn(name='md', title='Mean Difference', type='number')
        table$addColumn(name='se', title='SE', type='number')
        table$addColumn(name='df', title='df', type='number')
        table$addColumn(name='t', title='t', type='number')
        
        table$addColumn(name='pnone', title='p', type='number', format='zto,pvalue', visible="(postHocCorr:none)")
        table$addColumn(name='ptukey', title='p<sub>tukey</sub>', type='number', format='zto,pvalue', visible="(postHocCorr:tukey)")
        table$addColumn(name='pscheffe', title='p<sub>scheffe</sub>', type='number', format='zto,pvalue', visible="(postHocCorr:scheffe)")
        table$addColumn(name='pbonferroni', title='p<sub>bonferroni</sub>', type='number', format='zto,pvalue', visible="(postHocCorr:bonf)")
        table$addColumn(name='pholm', title='p<sub>holm</sub>', type='number', format='zto,pvalue', visible="(postHocCorr:holm)")
        
        combin <- expand.grid(bsLevels[rev(ph)])
        combin <- sapply(combin, as.character, simplify = 'matrix')
        if (length(ph) > 1)
          combin <- combin[,rev(1:length(combin[1,]))]
        
        comp <- list()
        iter <- 1
        for (i in 1:(length(combin[,1]) - 1)) {
          for (j in (i+1):length(combin[,1])) {
            comp[[iter]] <- list()
            comp[[iter]][[1]] <- combin[i,]
            comp[[iter]][[2]] <- combin[j,]
            
            if (j == length(combin[,1]))
              comp[[iter]][[3]] <- TRUE
            else
              comp[[iter]][[3]] <- FALSE
            
            iter <- iter + 1
          }
        }
        
        postHocRows[[composeTerm(ph)]] <- comp
        
        for (i in seq_along(comp)) {
          row <- list()
          for (c in seq_along(comp[[i]][[1]]))
            row[[paste0(names(comp[[i]][[1]][c]),'1')]] <- as.character(comp[[i]][[1]][c])
          for (c in seq_along(comp[[i]][[2]]))
            row[[paste0(names(comp[[i]][[2]][c]),'2')]] <- as.character(comp[[i]][[2]][c])
          
          table$addRow(rowKey=i, row)
          if (comp[[i]][[3]] == TRUE)
            table$addFormat(rowNo=i, col=1, Cell.END_GROUP)
        }
      }
      private$.postHocRows <- postHocRows
    },
    .populatePostHoc=function(data) {
      terms <- self$options$postHoc
      
      if (length(terms) == 0)
        return()
      
      tables <- self$results$postHoc
      
      postHocRows <- list()
      
      for (ph in terms) {
        
        table <- tables$get(key=ph)
        
        term <- jmvcore::composeTerm(ph)
        termB64 <- jmvcore::composeTerm(toB64(ph))
        formula <- as.formula(paste('~', term))
        
        suppressWarnings({
          # table$setStatus('running')
          referenceGrid <- lsmeans::lsmeans(private$.model, formula)
          none <- summary(pairs(referenceGrid, adjust='none'))
          tukey <- summary(pairs(referenceGrid, adjust='tukey'))
          scheffe <- summary(pairs(referenceGrid, adjust='scheffe'))
          bonferroni <- summary(pairs(referenceGrid, adjust='bonferroni'))
          holm <- summary(pairs(referenceGrid, adjust='holm'))
        }) # suppressWarnings
        
        resultRows <- lapply(strsplit(as.character(none$contrast), ' - '), function(x) strsplit(x, ','))
        tableRows <- private$.postHocRows[[term]]
        
        for (i in seq_along(tableRows)) {
          location <- lapply(resultRows, function(x) {
            
            c1 <- identical(x[[1]], as.character(tableRows[[i]][[1]]))
            c2 <- identical(x[[1]], as.character(tableRows[[i]][[2]]))
            c3 <- identical(x[[2]], as.character(tableRows[[i]][[1]]))
            c4 <- identical(x[[2]], as.character(tableRows[[i]][[2]]))
            if (c1 && c4)
              return(list(TRUE,FALSE))
            else if (c2 && c3)
              return(list(TRUE,TRUE))
            else
              return(list(FALSE,FALSE))
          })
          index <- which(sapply(location, function(x) return(x[[1]])))
          reverse <- location[[index]][[2]]

          row <- list()
          row[['md']] <- if(reverse) -none[index,'estimate'] else none[index,'estimate']
          row[['se']] <- none[index,'SE']
          row[['df']] <- none[index,'df']
          row[['t']] <- if(reverse) -none[index,'t.ratio'] else none[index,'t.ratio']
          
          row[['pnone']] <- none[index,'p.value']
          row[['ptukey']] <- tukey[index,'p.value']
          row[['pscheffe']] <- scheffe[index,'p.value']
          row[['pbonferroni']] <- bonferroni[index,'p.value']
          row[['pholm']] <- holm[index,'p.value']
          
          table$setRow(rowNo=i, values=row)
          private$.checkpoint()
        }
        if (.is.scaleDependent(private$.model,ph))
          table$setNote("covs",WARNS["ph.interactions"])
        else if (.term.develop(ph)<length(private$.modelTerms()))
          table$setNote("covs",WARNS["ph.covariates"])
        table$setStatus('complete')
      }
    },
    .populateLevenes=function(model) {
      
      if ( ! self$options$homo)
        return()
      data<-model$model
      print(str(model))
      data$res<-residuals(model)
      factors <- mf.getModelFactors(model)
      rhs <- paste0('`', factors, '`', collapse=':')
      formula <- as.formula(paste0('`res`~', rhs))
      result <- car::leveneTest(formula, data, center="mean")
      
      table <- self$results$get('assump')$get('homo')
      
      table$setRow(rowNo=1, values=list(
        F=result[1,'F value'],
        df1=result[1,'Df'],
        df2=result[2,'Df'],
        p=result[1,'Pr(>F)']))
    },
    .populateDescriptives=function(data) {
      
      if ( ! self$options$descStats)
        return()
      
      descTable <- self$results$desc
      dep <- self$options$dep
      dependent <- data[[dep]]
      factorNames <- rev(self$options$factors)
      factors <- as.list(select(data, factorNames))
      
      means <- aggregate(dependent, by=factors, base::mean)
      sds    <- aggregate(dependent, by=factors, stats::sd)
      ns <- aggregate(dependent, by=factors, base::length)
      
      stat <- data.frame(mean=means$x, sd=sds$x, n=ns$x)
      
      for (i in seq_len(nrow(stat))) {
        values <- stat[i,]
        values[is.na(values)] <- NaN
        descTable$setRow(rowNo=i, values)
      }
      
    },
    .populateSimple=function(model) {
        variable<-self$options$simpleVariable
        moderator<-self$options$simpleModerator
        threeway<-self$options$simple3way
        data<-mf.getModelData(model)
        simpleEffectsTables<-self$results$simpleEffects
        simpleEffectsAnovas<-self$results$simpleEffectsAnovas
  
        .fillTheFTable<-function(results,aTable) {
          ftests<-results[[2]]
          ### ftests      
          for (i in seq_len(dim(ftests)[1])) {
              r<-ftests[i,]
              row<-list(variable=r$variable,
              term=r$level,
              df=r$Df,
              ss=r$`Sum Sq`,
              F=r$`F value`,
              p=r$`Pr(>F)`)
              aTable$setRow(rowNo=i,row)
          }
         } #### end of .fillTheFTable
  
  .fillThePTable<-function(results,aTable) {
    params<-results[[1]]
    what<-params$level
    for (i in seq_len(dim(params)[1])) {
      r<-params[i,]
      row<-list(variable=r$variable,
                term=r$level,
                estimate=r$Estimate,
                std=r$`Std. Error`,
                t=r$`t value`,
                p=r$`Pr(>|t|)`)
      aTable$setRow(rowNo=i,row)
      if (what!=r$level)  
        aTable$addFormat(col=1, rowNo=i,format=Cell.BEGIN_GROUP)
      what<-r$level
    }
  } ##### end of .fillThePTable
  
  if (is.null(variable) | is.null(moderator)) 
    return()
  
  if (is.null(threeway)) {
    results<-.simpleEffects(model,data,variable,moderator)
    
    ### ftests
    key=paste(variable,1,sep="")
    ftable<-simpleEffectsAnovas$get(key=key)
    .fillTheFTable(results,ftable)      
    ### parameters
    ptable<-simpleEffectsTables$get(key=key)
    .fillThePTable(results,ptable)
    #### add some warning ####
    term<-.interaction.term(private$.model,c(variable,moderator))
    if (!is.null(term)) {
       if (.is.scaleDependent(private$.model,term))
           ptable$setNote("inter",WARNS["se.interactions"])
      else if (.term.develop(term)<length(private$.modelTerms()))
               ptable$setNote("covs",WARNS["se.covariates"])
    } else 
         ptable$setNote("noint",WARNS["se.noint"])
    
    ### end of warnings ###
  } else {
    data$mod2<-data[,threeway]
    if (is.factor(data$mod2)) {
      levs<-levels(data$mod2)
    } else 
      levs<-c(mean(data$mod2)+sd(data$mod2),mean(data$mod2),mean(data$mod2)-sd(data$mod2))
    for(i in seq_along(levs)) {
      data[,threeway]<-data$mod2
      if (is.factor(data$mod2))
        contrasts(data[,threeway])<-contr.treatment(length(levs),base=i)
      else
        data[,threeway]<-data[,threeway]-levs[i]
      ## make nice labels and titles
      lev<-ifelse(is.numeric(levs[i]),round(levs[i],digits=2),levs[i])
      title<-paste("Simple effects of ",variable," computed for",threeway,"at",lev)

      #### populate the R table       
      results<-.simpleEffects(model,data,variable,moderator)
      
      ### populate the Jamovi table
      key=paste(variable,i,sep="")
      ### F table
      ftable<-simpleEffectsAnovas$get(key=key)
      ftable$setTitle(title)
      .fillTheFTable(results,ftable)      
      ### parameters
      ptable<-simpleEffectsTables$get(key=key)
      ptable$setTitle(title)
      .fillThePTable(results,ptable)
    } 
  } # end of if (is.null(threeway)) 
  
},
    .modelTerms=function() {
      modelTerms <- self$options$modelTerms
# i removed the option of computing a full model if modelterm empty
# because I need the the model to be well specified
#      if (length(modelTerms) == 0)
#        modelTerms <- as.formula(~1)
      modelTerms
    },
    .ff=function() {
      factors <- c(self$options$factors,self$options$covs)
      if (length(factors) > 1) {
        formula <- as.formula(paste('~', paste(paste0('`', factors, '`'), collapse='*')))
        terms   <- attr(stats::terms(formula), 'term.labels')
        modelTerms <- sapply(terms, function(x) as.list(strsplit(x, ':')), USE.NAMES=FALSE)
      } else {
        modelTerms <- as.list(factors)
      }
      
      for (i in seq_along(modelTerms)) {
        term <- modelTerms[[i]]
        quoted <- grepl('^`.*`$', term)
        term[quoted] <- substring(term[quoted], 2, nchar(term[quoted])-1)
        modelTerms[[i]] <- term
      }
      
      modelTerms
    },
    .initDescPlots=function(data) {
      isAxis <- ! is.null(self$options$plotHAxis)
      isMulti <- ! is.null(self$options$plotSepPlots)
      
      self$results$get('descPlot')$setVisible( ! isMulti && isAxis)
      self$results$get('descPlots')$setVisible(isMulti)
      
      if (isMulti) {
        return()    
        sepPlotsName <- self$options$plotSepPlots
        sepPlotsVar <- data[[sepPlotsName]]
        if(is.factor(sepPlotsVar))
             sepPlotsLevels <- 1:length(levels(sepPlotsVar))
        else sepPlotsLevels <- c(1,2,3)   
        array <- self$results$descPlots
        for (level in sepPlotsLevels)
          array$addItem(level)
      }
    },
    .prepareDescPlots=function(model) {
  
      depName <- self$options$dep
      groupName <- self$options$plotHAxis
      linesName <- self$options$plotSepLines
      plotsName <- self$options$plotSepPlots
      
      ciWidth   <- self$options$ciWidth
      errorBarType <- self$options$plotError
      if (length(depName) == 0 || length(groupName) == 0)
        return()
      
      plotData<-private$.preparePlotData(model)
      
      if (self$options$plotError != 'none') {
        yAxisRange <- pretty(c(plotData$lower, plotData$upper))
      } else {
        yAxisRange <- plotData$mean
      }
      if (is.null(plotsName)) {
        
        image <- self$results$get('descPlot')
        image$setState(list(data=plotData, range=yAxisRange))
        
      } else {
        sepPlotsLevels<-levels(plotData$plots)
        array <- self$results$descPlots
        for (level in sepPlotsLevels)
          array$addItem(paste(plotsName,"=",level))

        images <- self$results$descPlots
        
        for (level in images$itemKeys) {
          image <- images$get(key=level)
          real<-gsub(paste(plotsName,"= "),"",level)
          image$setState(list(data=subset(plotData,plots==real), range=yAxisRange))
          
        }
      }
    },

.descPlot=function(image, ggtheme, theme, ...) {
  library(ggplot2)
  if (is.null(image$state))
    return(FALSE)
  
  depName <- self$options$dep
  groupName <- self$options$plotHAxis
  linesName <- self$options$plotSepLines
  plotsName <- self$options$plotSepPlots
  errorType <- self$options$plotError
  
  if ( ! is.null(linesName)) {
    p<-.twoWaysPlot(image,theme,depName,groupName,linesName,errorType)
  } else {
    p<-.oneWayPlot(image,theme,depName,groupName,errorType)
  }      
  p<-p+ggtheme
  print(p)
  TRUE
},

    .qqPlot=function(image, ggtheme, theme, ...) {
      library(ggplot2)
      dep <- self$options$dep
      factors <- self$options$factors
      modelTerms <- private$.modelTerms()
      model<-private$.model      
      if (is.null(model) )
              return(FALSE)
      
      data <- model$model
      residuals <- rstandard(model)
      df <- as.data.frame(qqnorm(residuals, plot.it=FALSE))
      print(ggplot2::ggplot(data=df, aes(y=y, x=x)) +
              geom_abline(slope=1, intercept=0, colour=theme$color[1]) +
              geom_point(aes(x=x,y=y), size=2, colour=theme$color[1]) +
              xlab("Theoretical Quantiles") +
              ylab("Standardized Residuals") +
              ggtheme)
      
      TRUE
    },
    .preparePlotData=function(model) {
  groupName <- self$options$plotHAxis
  linesName <- self$options$plotSepLines
  plotsName <- self$options$plotSepPlots
  bars<-self$options$plotError
  
  selected<-c(groupName,linesName,plotsName)  
  vars<-all.vars(terms(model))[-1]
  
  ll<-list()
  for (v in vars) {
    if (is.factor(model$model[,v])) 
      ll[v]<-list(levels(model$model[,v]))
    else {
      if (v %in% selected) {
        if (v==groupName)
            ll[v]<-list(c(-2,-1,0,1,2)*sd(model$model[,v])+mean(model$model[,v]))
        else
            ll[v]<-list(c(-1,0,1)*sd(model$model[,v])+mean(model$model[,v]))
      }
      else 
        ll[v]<-list(0)
    }
  }
  dm<-expand.grid(ll)
  for (v in names(model$contrasts)) {
    dm[,v]<-factor(dm[,v])
  }
  mm<-predict(model,dm,interval="c",level=0.95,se.fit = T)
  dm<-as.data.frame(cbind(mm,dm))
  if (length(selected)==1) by<-list(dm[,selected])
  else by<-dm[,selected]
  lnames<-c("group","lines","plots")
  dm<-aggregate(dm[,c("fit.fit","fit.lwr","fit.upr","se.fit")],by,mean)
  if (bars=="se") {
    dm$fit.lwr<-dm$fit.fit-dm$se.fit
    dm$fit.upr<-dm$fit.fit+dm$se.fit
  }
  names(dm)<-c(lnames[1:length(selected)],c("mean","lower","upper","se"))
  if (!is.null(dm$plots) & !is.factor(model$model[,plotsName])) {
        dm$plots<-factor(dm$plots)
        levels(dm$plots)<-c("-SD","Mean","+SD")
  }

  if (!is.null(dm$lines) & !is.factor(model$model[,linesName])) {
      dm$lines<-factor(dm$lines)
      levels(dm$lines)<-c("-SD","Mean","+SD")

  }
  dm
}, # end of .preparePlotData()
########## Those are functions to clean the results ############
.anovaTable=function(model,anovaframe) {
  dss<-anovaframe
  sumr<-summary(model)
  errDF<-sumr$fstatistic[3]
  modDF<-sumr$fstatistic[2]
  modF<-sumr$fstatistic[1]
  errSS<-dss$ss[length(dss$ss)]
  errMS<-errSS/errDF
  r2<-sumr$r.squared
  totalSS<-1/((1-r2)*(1/errSS))
  modSS<-totalSS-errSS
  modp<-1 - pf(modF, modDF, errDF) 
  modRow<-c(ss=modSS,df=modDF,F=modF,p=modp)
  ss<-rbind(modRow,dss)
  ss$ms<-ss$ss/ss$df
  ss$etaSq<-ss$ss/(totalSS)
  ss$etaSqP <- ss$ss / (ss$ss + errSS)
  ss$omegaSq <- (ss$ss - (ss$df * errMS)) / (totalSS + errMS)
  ss
},

.cleanAnova=function(anovares,type) {
  res<-as.data.frame(anovares)
  if (type=='1') 
    res<-res[,c(2,1,4,5)]
  
  if (type=='3') 
    res<-res[-1,]
  colnames(res)<-c("ss","df","F","p")
  res
},

.r2indices=function(model) {
  sumr<-summary(model)
  list(r2=round(sumr$r.squared,digits = 3),ar2=round(sumr$adj.r.squared,digits=3))
},



    .sourcifyOption = function(option) {
      
      name <- option$name
      value <- option$value
      
      if (name == 'contrasts') {
        i <- 1
        while (i <= length(value)) {
          item <- value[[i]]
          if (item$type == 'default')
            value[[i]] <- NULL
          else
            i <- i + 1
        }
        if (length(value) == 0)
          return('')
      } else if (name == 'modelTerms') {
        if (base::identical(as.list(value), private$.ff()))
          return('')
      } else if (name == 'postHoc') {
        if (length(value) == 0)
          return('')
      }
      
      super$.sourcifyOption(option)
    })
)

