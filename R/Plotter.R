        ## plot should be inited and prepared storing all information needed to produce the plot in
        ## the image (Image object) state. It does not matter if the state is set in init() or run()
        ## as long as the plot does not require the model to be re-estimated
        ## Info present at .init() (number of variables, variables names, etc) me be omitted from the
        ## information contained in the image$state

aPlot <- R6::R6Class(
    "aPlot",
    cloneable = FALSE,
    class = TRUE,
    inherit = Scaffold,
    public = list(
        key          = NULL,  
        subkeys      = NULL,
        operator     = NULL,
        datamatic    = NULL,
        plotarray    = NULL,
        plot         = NULL,
        type         = NULL,
        subtype      = NULL,
        terms        = NULL,
        dep          = NULL,
        y            = NULL,
        x            = NULL,
        z            = NULL,
        moderators   = NULL,
        cluster      = NULL,
        x_scale      = FALSE,
        x_range      = list(),
        plot_bars    = FALSE,
        plot_raw     = FALSE,
        plot_dodge   = NULL,
        plot_label   = NULL,
        largedata    = NULL,
        title        = NULL,
        init = function() {

            # collect the terms as datamatic objects
            self$y <- self$datamatic$variables[[tob64(self$dep)]]
            self$x <- self$datamatic$variables[[tob64(self$terms[[1]])]]
            if (length(self$terms)>1)
                   self$z <- self$datamatic$variables[[tob64(self$terms[[2]])]]
            
            if (length(self$terms)>2)
                for (t in self$terms[3:length(self$terms)])
                     self$moderators<-c(self$moderators,self$datamatic$variables[[tob64(t)]])
     
            ### fix dimensions for special models 
            
            if (self$options$model_type == "multinomial") {
                self$moderators <- c(self$z, self$moderators)
                self$z <- self$y
                self$subtype <- "response"
            }

            if (self$options$model_type == "ordinal" & self$subtype != "mean.class") {
                self$moderators <- c(self$z, self$moderators)
                self$z <- self$y
            }
            
            ### set the title for one plot array
            self$title<-paste("Plot: ",paste(self$dep,"~",jmvcore::stringifyTerm(c(self$x$name, self$z$name))))
            self$plotarray$setTitle(self$title)

            dims <- unlist(lapply(self$moderators, function(mod) mod$levels))

            if (!is.something(self$moderators)) {
                self$subkeys <- 1
                self$plotarray$addItem(key = 1)
            }
            else {
               ldims <- lapply(self$moderators, function(mod) mod$levels_labels)
               grid  <- expand.grid(ldims,stringsAsFactors=FALSE)
               names(grid)<-unlist(lapply(self$moderators,function(mod) mod$name ))
               for (i in seq_len(nrow(grid))) {
                        self$plotarray$addItem(key = i)
                        title<-paste(names(grid),grid[i,],sep="=",collapse=" - ")
                        self$plotarray$get(key = i)$setTitle(title)
                 
               }
               self$subkeys<-seq_len(nrow(grid))
            }


            ### some options here

            if (self$options$plot_around != "none") {
                self$plot_bars <- TRUE
                self$plot_dodge <- ggplot2::position_dodge(0.2)
                self$plot_label <- paste(self$z$name, paste0("(", toupper(self$options$plot_around), ")"), sep = "\n")
            } else {
                self$plot_dodge <- ggplot2::position_dodge(0)
            }
            
            self$plot_label <- self$z$name
        
            }, ## end of init
        prepare= function() {
              
            ### stop if it is filled from previous run ###
            jinfo("PLOTTER Plot: checking main plot", self$key)
            test <- any(unlist(sapply(self$plotarray$items, function(i) !i$isNotFilled())))
            if (test) {
                return()
            }

            jinfo("PLOTTER Plot: prepare main plot", self$key)
            
            ## this is done for each plot. At the moment, all main
            ## plots share the same customization, but in the 
            ## future we can envisage a different customization per plot

            ### give a range to the x-axis, if needed
            x_range <- list(min = NA, max = NA, ticks = NA, noticks = FALSE)

            min <- as.numeric(self$optionValue("plot_x_min"))
            if (is.number(min)) {
                x_range$min <- min
            }

            max <- as.numeric(self$optionValue("plot_x_max"))
            if (is.number(max)) {
                x_range$max <- max
            }

            ticks <- as.numeric(self$optionValue("plot_x_ticks"))
            if (is.number(ticks)) {
                x_range$ticks <- ticks

                if (ticks < 0) {
                    self$warning <- list(topic = "plotnotes", message = "Invalid number of ticks (<0). The value is ignored.", type = "warning")
                    x_range$ticks <- NA
                }
                if (ticks == 1) {
                    self$warning <- list(topic = "plotnotes", message = "Invalid number of ticks (1). The value is ignored.", type = "warning")
                    x_range$ticks <- NA
                }

                if (ticks > 20) {
                    self$warning <- list(topic = "plotnotes", message = "Invalid number of ticks (>20). The value is ignored.", type = "warning")
                    x_range$ticks <- NA
                }
                if (ticks == 0) {
                    x_range$ticks <- NA
                    x_range$noticks <- TRUE
                }
            }


            
            ################



            self$x_range <- x_range 
            ### compute the expected values to be plotted ###
            data <- private$.estimate(self$x, unlist(c(self$z, self$moderators)))

            ### prepare rawData if needed
            rawData <- mf.clean(mf.data(self$operator$model))
            for (var in intersect(names(rawData), names(self$datamatic$variables))) {
                varobj <- self$datamatic$variables[[var]]
                if (varobj$type == "factor") {
                    levels(rawData[[var]]) <- varobj$levels_labels
                }
            }

            ### here we deal with plotting random effects, if needed
            randomData <- NULL

            if (self$option("plot_re") &&
                !self$option("model_type", "ordinal") &&
                !self$option("model_type", "multinomial")) {
                ### here we want to be sure that clusters passed as cluster1/cluster2 or cluster1:cluster2 works

                rawData <- private$.fix_clusters(rawData)
    
                .model <- self$operator$model

                if (self$option("plot_re_method", "average")) {
                    formula <- self$operator$formulaobj$keep(self$x$name)
                    res <- try_hard(mf.update(.model, formula = formula))
                    if (!isFALSE(res$error)) {
                        self$warning <- list(
                            topic = "plotnotes",
                            message = paste("Random effects cannot be plotted for", self$x$name, " with the averaging method."),
                            head = "warning"
                        )
                    } else {
                        .model <- res$obj
                    }
                }
                y <- stats::predict(.model, type = self$subtype)
                randomData <- as.data.frame(cbind(y, rawData))
                if (self$x$type == "factor") {
                    levels(randomData[[self$x$name64]]) <- self$x$levels_labels
                }

                self$warning <- list(topic = "plotnotes", message = paste("In",self$title,"random effects are plotted across", self$cluster$name), head = "info")
                # prepare a test for between variables to plot dots for random effects

                xbetween <- FALSE
                test <- tapply(as.numeric(rawData[[self$x$name64]]), rawData[[self$cluster$name64]], sd)
                test <- sum(sapply(test, function(x) as.numeric(is.na(x) || x == 0)))
                nc <- self$cluster$nlevels
                if ((test / nc) > .30) xbetween <- TRUE
            }
            ### end of random ###


            ### deal with Y ####
            dep64 <- self$y$name64

            ### give a range to the y-axis, if needed
            y_range <- list(min = NA, max = NA, ticks = NA, noticks = FALSE)

            if (self$option("plot_yscale")) {
                y_range$min <- self$y$descriptive$min
                y_range$min <- self$y$descriptive$max
            }

            min <- as.numeric(self$optionValue("plot_y_min"))

            if (is.number(min)) {
                y_range$min <- min
            }

            max <- as.numeric(self$optionValue("plot_y_max"))
            if (is.number(max)) {
                y_range$max <- max
            }

            ticks <- as.numeric(self$optionValue("plot_y_ticks"))

            if (is.number(ticks)) {
                y_range$ticks <- ticks

                if (ticks == 1) {
                    self$warning <- list(topic = "plotnotes", message = "Invalid number of ticks (1). The value is ignored.", head = "warning")
                    y_range$ticks <- NA
                }
                if (ticks < 0) {
                    self$warning <- list(topic = "plotnotes", message = "Invalid number of ticks (<0). The value is ignored.", head = "warning")
                    y_range$ticks <- NA
                }

                if (ticks > 20) {
                    self$warning <- list(topic = "plotnotes", message = "Invalid number of ticks (>20). The value is ignored.", head = "warning")
                    y_range$ticks <- NA
                }
                if (ticks == 0) {
                    y_range$ticks <- NA
                    y_range$noticks <- TRUE
                }
            }

            if (self$option("model_type", "multinomial")) {
                self$plot_raw <- FALSE
            }

            if (self$subtype == "link") {
                self$plot_raw <- FALSE
                y_range$min <- NA
                y_range$max <- NA
                y_range$ticks <- NA
            }

            if (self$option("model_type", "multinomial")) {
                self$plot_raw <- FALSE
            }

            if (self$subtype == "link") {
                self$plot_raw <- FALSE
                y_range$min <- NA
                y_range$max <- NA
                y_range$ticks <- NA
            }


            ### we need to be sure that the dependent variable is a continuous variable to plot the raw data ##

            if (self$y$type == "factor") {
                levels(rawData[[dep64]]) <- 0:(self$y$nlevels - 1)
                rawData[[dep64]] <- as.numeric(as.character(rawData[[dep64]]))
            }
            if (self$option("model_type", "ordinal")) {
                rawData[[dep64]] <- rawData[[dep64]] + 1
                if (self$option("plot_scale", "mean.class")) {
                    y_range$min <- NA
                    y_range$max <- NA
                    y_range$ticks <- NA
                }
            }
            if (self$option("model_type", c("logistic", "multinomial"))) {
                if (self$subtype != "link") {
                    y_range$min <- NA
                }
                y_range$max <- NA
                y_range$ticks <- NA
            }



            #### deal with rescaling
            if (self$x_scale) {
                if (self$x$covs_scale == "clusterbasedcentered") {
                    self$warning <- list(
                        topic = "plotnotes",
                        message = "Rescaling cluster-wise centered variables may be misleading. Use `Covariates Scaling=None` if the original scale is necessary.",
                        head = "warning"
                    )
                }

                if (self$x$covs_scale == "clusterbasedstandardized") {
                    self$warning <- list(
                        topic   = "plotnotes",
                        message = "Rescaling cluster-wise standardized variables may be misleading. Use `Covariates Scaling=None` if the original scale is necessary.",
                        head    = "warning"
                    )
                }

                data[[self$x$name64]] <- self$x$rescale(data[[self$x$name64]])

                if (is.something(rawData)) {
                    rawData[[self$x$name64]] <- self$x$rescale(rawData[[self$x$name64]])
                }
                if (is.something(randomData)) {
                    randomData[[self$x$name64]] <- self$x$rescale(randomData[[self$x$name64]])
                }
            }



            #### compute the levels combinations
            #### first, gets all levels of factors and covs. Then create the combinations and select the rows of the
            #### emmeans estimates needed for it. It selects the rows using the levels found in datamatic
            ### for the raw data, it selects only if the moderator is a factor, whereas all data for the
            ### continuous are retained
            #### TODO: this is abstruse, try changing it

            state <- list(
                key     = self$key,
                y_range = y_range,
                x_range = x_range
            )

            dims <- sapply(self$moderators, function(mod) mod$levels_labels, simplify = FALSE)

            if (is.something(dims)) {
              
                grid <- expand.grid(dims, stringsAsFactors = FALSE)
                .names <- unlist(sapply(self$moderators, function(mod) mod$name, simplify = FALSE))
                names(grid)<-.names
                .names64 <- tob64(.names)
                selectable <- intersect(.names, self$options$factors)
                selgrid <- as.data.frame(grid[, selectable])
                .sel64 <- tob64(selectable)

                for (i in 1:nrow(grid)) {
                    label <- paste(.names, grid[i, ], sep = "=", collapse = " , ")
                    aplot <- self$plotarray$get(key = i)
                    aplot$setTitle(label)
                    sel <- paste(paste0("data$", .names64, sep = ""), paste0('"', grid[i, ], '"'), sep = "==", collapse = " & ")
                    localdata <- data[eval(parse(text = sel)), ]
                    state[["plotData"]] <- localdata

                    if (self$plot_raw) {
                        if (length(selectable) > 0) {
                            sel <- paste(paste0("rawData$", .sel64, sep = ""), paste0('"', selgrid[i, ], '"'), sep = "==", collapse = " & ")
                            raw <- rawData[eval(parse(text = sel)), ]
                        } else {
                            raw <- rawData
                        }

                        state[["rawData"]] <- raw
                    }

                    if (!is.null(randomData)) {
                        if (length(selectable) > 0) {
                            sel <- paste(paste0("randomData$", .sel64, sep = ""), paste0('"', selgrid[i, ], '"'), sep = "==", collapse = " & ")
                            rdata <- randomData[eval(parse(text = sel)), ]
                        } else {
                            rdata <- randomData
                        }

                        selectorlist <- list(rdata[[self$cluster$name64]], rdata[[self$x$name64]])
                        .rnames <- c("cluster", "x", "y")
                        rdata <- stats::aggregate(rdata$y, selectorlist, mean)
                        names(rdata) <- .rnames
                        attr(rdata, "xbetween") <- xbetween
                        state[["randomData"]] <- rdata
                    }
                    aplot$setState(state)
                }
            } else {

                aplot <- self$plotarray$get(key = 1)
                state[["plotData"]] <- data
                if (self$plot_raw) {
                    state[["rawData"]] <- rawData
                }

                if (!is.null(randomData)) {
                    rdata <- randomData
                    selectorlist <- list(rdata[[self$cluster$name64]], rdata[[self$x$name64]])
                    .rnames <- c("cluster", "x", "y")
                    rdata <- stats::aggregate(rdata$y, selectorlist, mean)
                    names(rdata) <- .rnames
                    attr(rdata, "xbetween") <- xbetween
                    state[["randomData"]] <- rdata
                }
               
                aplot$setState(state)
            }
          } # end of prepare
    ), # end of public
    private=list(
      
        .estimate     = function(x, terms) {
          
           # here we estimate the plot data based on the predicted values of the model
            conditions <- list()
            labels <- list()
            for (var in terms) {
                if (var$type == "numeric") {
                    conditions[[var$name64]] <- var$levels
                    labels[[var$name64]] <- var$levels_labels
                }
            }

            if (x$type == "numeric") {
              
              ### min max should always be in the scale of the variables used to 
              ## estimate the model. Rescaling should be done afterwards
              min <- x$descriptive$min
              max <- x$descriptive$max
              
              if (self$option("plot_xoriginal")) {
                    self$x_scale <- TRUE
                    self$warning <- list(topic = "plotnotes", 
                                         message = paste("In", self$title,"variable",x$name," is in the original scale"), 
                                         head = "info")
              } 
                

              if (self$option("plot_extra")) {
                
                    # if extraploation is required we pass the user min and max
                    # but if the original scale is also required,
                    #  the user min and max should be first scaled to the actual scale of the
                    #  model variables
                    
                    if (is.number(self$x_range$min)) {
                        if (self$option("plot_xoriginal"))
                             min <- x$scale(self$x_range$min)
                        else
                             min <- self$x_range$min
                          
                    }
                    if (is.number(self$x_range$max)) {
                      
                        if (self$option("plot_xoriginal"))
                             max <- x$scale(self$x_range$max)
                        else
                             max <- self$x_range$max

                    }
                  
                }
                
                conditions[[self$x$name64]] <- pretty(c(min, max), n = 30)
               
                
            }
            allterm64 <- c(x$name64, unlist(sapply(terms, function(x) x$name64)))

            mode <- NULL


            if (self$option("model_type", "ordinal")) {
                if (self$option("plot_scale", "mean.class")) {
                    mode <- "mean.class"
                } else {
                    mode <- "prob"
                    self$plot_raw <- FALSE
                }
            }

            ### now we get the estimated means #######

            em_opts <- list(
                self$operator$model,
                specs = allterm64,
                at = conditions,
                type = self$subtype,
                mode = mode,
                nesting = NULL,
                options = list(level = self$operator$ciwidth),
                data = insight::get_data(self$operator$model, source = "frame")
            )

            ### mmblogit model data are not recognized by emmeans. We need to pass them explicetely
            if (self$option("model_type", "multinomial") & self$option(".caller", "glmer")) {
                em_opts[["data"]] <- self$operator$model$data
            }

            if (self$option("df_method")) {
                em_opts[["lmer.df"]] <- self$options$df_method
            }

            results <- try_hard(do.call(emmeans::emmeans, em_opts))
            self$warning <- list("topic" = "plotnotes", message = results$warning)
            self$error <- list("topic" = "plotnotes", message = results$error)
            referenceGrid <- results$obj
            tableData <- as.data.frame(referenceGrid)
            ### rename the columns ####
            names(tableData) <- c(allterm64, "estimate", "se", "df", "lower", "upper")

            if (self$options$plot_around == "se") {
                tableData$lower <- tableData$estimate - tableData$se
                tableData$upper <- tableData$estimate + tableData$se
            }
            if (self$options$plot_around == "none") {
                tableData$lower <- NULL
                tableData$upper <- NULL
            }
            if (x$type == "factor") {
                tableData[[x$name64]] <- factor(tableData[[x$name64]])
                levels(tableData[[x$name64]]) <- x$levels_labels
            }

            for (var in terms) {
                tableData[[var$name64]] <- factor(tableData[[var$name64]])
                levels(tableData[[var$name64]]) <- var$levels_labels
            }
            tableData
        },
        .fix_clusters = function(data) {
          
            test <- grep("[\\:\\/]", self$operator$formulaobj$clusters)
            if (length(test) > 0) {
                cluster <- self$operator$formulaobj$clusters[[test]]
                .clustervars <- stringr::str_split(cluster, "[\\:\\/]")[[1]]
                name64 <- tob64(cluster)
                sel <- paste0("data$", name64, "=", "paste0(", paste0("data$", tob64(.clustervars), collapse = ","), ",sep='_')")
                eval(parse(text = sel))
                data[[name64]] <- factor(data[[name64]])
                self$cluster <- list(name = cluster, name64 = name64, nlevels = nlevels(data[[name64]]))
            } else {
                self$cluster <- self$datamatic$variables[[tob64(self$operator$formulaobj$clusters[[1]])]]
            }
            return(data)
        }
      
    ) #end of private
 ) # end of class

### this class handles the plots

Plotter <- R6::R6Class(
    "Plotter",
    cloneable = FALSE,
    class = TRUE,
    inherit = Scaffold,
    public = list(
        results = NULL,
        plots   = NULL,
        initialize = function(jmvobj, operator) {
            super$initialize(jmvobj)
            private$.results <- jmvobj$results
            private$.operator <- operator
            private$.datamatic <- operator$datamatic
          
        },
        ## plot should be inited and prepared storing all information needed to produce the plot in
        ## the image (Image object) state. It does not matter if the state is set in init() or run()
        ## as long as the plot does not require the model to be re-estimated
        ## Info present at .init() (number of variables, variables names, etc) me be omitted from the
        ## information contained in the image$state
        initPlots = function() {
            private$.initMainPlot()
            private$.initJnPlot()
            private$.initClusterPlots()
            private$.initRandHist()
        },
        preparePlots = function(image, ggtheme, theme, ...) {
            ## here are the plots that require some preparations. All plots
            ## are  prepared
            if (isFALSE(private$.operator$ok)) {
                return()
            }

            private$.prepareMainPlot()
            private$.prepareQqplot()
            private$.prepareNormplot()
            private$.prepareResidplot()
            private$.prepareClusterBoxplot()
            private$.prepareClusterResPred()
            private$.prepareClusterResPredGrid()
            private$.prepareRandHist()
            private$.prepareJnPlot()
        },
        scatterPlot = function(image, ggtheme, theme) {
          
          
            ## debug: return this to see what error is in the plotter code ###
            if (!is.something(image$state$plotData)) {
                # pp<-ggplot2::ggplot(data.frame(1:3))+ggplot2::ggtitle(image$key)
                # return(pp)
                return()
            }
          
            key <- image$state$key
            plotobj<-self$plots[[key]]
            ## collect the data
            data <- image$state$plotData
            plotobj_label<-NULL

            linesdiff <- (theme$bw || self$options$plot_black)
            ### prepare aestetics for one or two way scatterplot
            if (is.null(plotobj$z)) {
                 names(data)[1] <- "x"
                .aestetics <- ggplot2::aes(x = x, y = estimate, group = 1)
                .aesbar <- ggplot2::aes(x = x, ymin = lower, ymax = upper)
            } else {
                names(data)[1:2] <- c("x", "z")
                plotobj_label<-plotobj$z$name
                if (isFALSE(linesdiff)) {
                    .aestetics <- ggplot2::aes(x = x, y = estimate, group = z, colour = z)
                    .aesbar <- ggplot2::aes(x = x, ymin = lower, ymax = upper, group = z, color = z, fill = z)
                } else {
                    .aestetics <- ggplot2::aes(x = x, y = estimate, group = z, linetype = z)
                    .aesbar <- ggplot2::aes(x = x, ymin = lower, ymax = upper, group = z, linetype = z)
                    if (!theme$bw && self$options$plot_black) {
                        .aestetics <- ggplot2::aes(x = x, y = estimate, group = z, linetype = z, colour = z)
                    }
                }
            }

            ## initialize plot
            p <- ggplot2::ggplot()

            # give a scale to the Y axis

            if (is.number(image$state$y_range$ticks)) {
                if (self$option("plot_y_ticks_exact")) {
                    if (any(sapply(image$state$y_range[c("min", "max", "ticks")], is.na))) {
                        self$warning <- list(
                            topic = "plotnotes",
                            message = paste("Exact ticking requires to set min and max and number of ticks"),
                            head = "warning"
                        )
                        p <- p + ggplot2::scale_y_continuous(limits = as.numeric(image$state$y_range))
                    } else {
                        p <- p + ggplot2::scale_y_continuous(limits = as.numeric(image$state$y_range), breaks = seq(image$state$y_range$min, image$state$y_range$max, length.out = image$state$y_range$ticks))
                    }
                } else {
                    p <- p + ggplot2::scale_y_continuous(limits = as.numeric(image$state$y_range), n.breaks = image$state$y_range$ticks)
                }
            } else {
                p <- p + ggplot2::scale_y_continuous(limits = as.numeric(image$state$y_range))
            }




            #### plot the actual data if required

            if (plotobj$plot_raw) {
                rawdata <- image$state$rawData
                y <- plotobj$y$name64
                x <- plotobj$x$name64
                z <- plotobj$z$name64
                #
                .aesraw <- ggplot2::aes(x = .data[[x]], y = .data[[y]])

                if (!is.null(self$scatterZ)) {
                    if (self$scatterZ$type == "factor") {
                        .aesraw <- ggplot2::aes(x = .data[[x]], y = .data[[y]], color = .data[[z]])
                    }
                }

                p <- p + ggplot2::geom_point(
                    data = rawdata,
                    .aesraw,
                    show.legend = FALSE, alpha = 0.5, shape = 16
                )
            }


            ##### END OF RAW DATA #############

            if (is.something(image$state$randomData)) {
                randomData <- image$state$randomData
                if ("z" %in% names(randomData)) {
                    .aesrandom <- ggplot2::aes(x = x, y = y, group = cluster, colour = z)
                    p <- p + ggplot2::geom_line(
                        data = randomData,
                        .aesrandom,
                        linewidth = 0.5,
                        alpha = .80
                    )
                } else {
                    .aesrandom <- ggplot2::aes(x = x, y = y, group = cluster)
                    p <- p + ggplot2::geom_line(
                        data = randomData,
                        .aesrandom,
                        color = "gray50",
                        linewidth = 0.5,
                        alpha = .80
                    )
                    if (attr(randomData, "xbetween")) {
                        p <- p + ggplot2::geom_point(
                            data = randomData,
                            .aesrandom,
                            color = "gray4",
                            linewidth = 0.5,
                            alpha = .80
                        )
                    }
                }
            }

            ######### fix the bars ##########
            if (plotobj$plot_bars) {
                if (plotobj$x$type == "factor") {
                    p <- p + ggplot2::geom_errorbar(data = data, .aesbar, linewidth = .9, width = .3, position = plotobj$plot_dodge, show.legend = FALSE)
                } else {
                    p <- p + ggplot2::geom_ribbon(data = data, .aesbar, linetype = 0, show.legend = F, alpha = 0.2)
                }
            }
            #########  ##########


            ### plot the lines
            p <- p + ggplot2::geom_line(
                data = data,
                .aestetics,
                linewidth = 1.2,
                position = plotobj$plot_dodge
            )


            ### plot the points for factors
            if (plotobj$x$type == "factor") {
                p <- p + ggplot2::geom_point(
                    data = data,
                    .aestetics,
                    shape = 21, size = 4, fill = "white",
                    position = plotobj$plot_dodge, show.legend = FALSE
                )
            } else {
                # give a scale to the Z axis
                p <- p + ggplot2::scale_x_continuous(limits = as.numeric(image$state$x_range))

                if (is.number(image$state$x_range$ticks)) {
                    if (self$option("plot_x_ticks_exact")) {
                        if (any(sapply(image$state$x_range[c("min", "max", "ticks")], is.na))) {
                            self$warning <- list(
                                topic = "plotnotes",
                                message = paste("Exact ticking for the X-axis requires to set min and max"),
                                head = "warning"
                            )
                        } else {
                            p <- p + ggplot2::scale_x_continuous(limits = as.numeric(image$state$x_range), breaks = seq(image$state$x_range$min, image$state$x_range$max, length.out = image$state$x_range$ticks))
                        }
                    } else {
                        p <- p + ggplot2::scale_x_continuous(limits = as.numeric(image$state$x_range), n.breaks = image$state$x_range$ticks)
                    }
                }
            }
            ### end of dealing with scales

            p <- p + ggtheme

            p <- p + ggplot2::labs(
                x = plotobj$x$name, y = plotobj$y$name,
                colour = plotobj_label,
                shape = plotobj_label,
                linetype = plotobj_label
            )

            if (self$options$plot_black) {
                p <- p + ggplot2::theme(legend.key.width = ggplot2::unit(2, "cm"))
            }

            if (image$state$y_range$noticks) {
                p <- p + ggplot2::theme(axis.ticks.y = ggplot2::element_blank())
                p <- p + ggplot2::theme(axis.text.y = ggplot2::element_blank())
            }
            if (image$state$x_range$noticks) {
                p <- p + ggplot2::theme(axis.ticks.x = ggplot2::element_blank())
                p <- p + ggplot2::theme(axis.text.x = ggplot2::element_blank())
            }


            return(p)
        },
        jnPlot = function(image, ggtheme, theme) {
          
            if (is.null(image$state)) {
                return()
            }


            datalist <- image$state
            alpha <- .05
            pmsg <- paste("p <", alpha)
            colors <- ggtheme[[2]]$palette(2)
            sig.color <- colors[2]
            insig.color <- colors[1]

            ypos <- min(c(datalist$cbso1$Lower, datalist$cbso2$Lower, datalist$cbsi$Lower), na.rm = T)

            p <- ggplot2::ggplot()
            suppressMessages(p <- p + ggtheme)


            p <- p + ggplot2::geom_path(data = datalist$cbso1, ggplot2::aes(x = z, y = slopes, color = Significance), size = .8, show.legend = FALSE)
            p <- p + ggplot2::geom_path(data = datalist$cbsi, ggplot2::aes(x = z, y = slopes, color = Significance), size = .8, show.legend = FALSE)
            p <- p + ggplot2::geom_path(data = datalist$cbso2, ggplot2::aes(x = z, y = slopes, color = Significance), size = .8, show.legend = FALSE)
            p <- p + ggplot2::geom_ribbon(data = datalist$cbso1, ggplot2::aes(x = z, ymin = Lower, ymax = Upper, fill = Significance), alpha = 0.2)
            p <- p + ggplot2::geom_ribbon(data = datalist$cbsi, ggplot2::aes(x = z, ymin = Lower, ymax = Upper, fill = Significance), alpha = 0.2)
            p <- p + ggplot2::geom_ribbon(data = datalist$cbso2, ggplot2::aes(x = z, ymin = Lower, ymax = Upper, fill = Significance), alpha = 0.2)
            p <- p + ggplot2::scale_fill_manual(
                values = c(Significant = sig.color, Insignificant = insig.color), labels = c("Significant (p < .05)", " n.s (p \u2265 .05)"),
                breaks = c("Significant", "Insignificant"), drop = FALSE, guide = ggplot2::guide_legend(order = 2)
            )
            p <- p + ggplot2::scale_color_manual(values = c(Significant = sig.color, Insignificant = insig.color))

            p <- p + ggplot2::geom_hline(ggplot2::aes(yintercept = datalist$intercept))
            p <- p + ggplot2::geom_segment(
                ggplot2::aes(
                    x = datalist$modrange[1],
                    xend = datalist$modrange[2], y = datalist$intercept, yend = datalist$intercept, linetype = "Range of\nobserved\ndata"
                ),
                lineend = "square", size = 1.25
            )
            p <- p + ggplot2::scale_linetype_discrete(name = " ", guide = ggplot2::guide_legend(order = 1))

            if (datalist$bounds[1] < datalist$modrange[1]) {
            } else if (datalist$all_sig == FALSE) {
                p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept = datalist$bounds[1]), linetype = 2, color = sig.color)
                p <- p + ggplot2::geom_text(ggplot2::aes(label = round(datalist$bounds[1], digits = 3), x = datalist$bounds[1], y = ypos), color = "black")
            }
            if (datalist$bounds[2] > datalist$modrange[2]) {
            } else if (datalist$all_sig == FALSE) {
                p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept = datalist$bounds[2]), linetype = 2, color = sig.color)
                p <- p + ggplot2::geom_text(ggplot2::aes(label = round(datalist$bounds[2], digits = 3), x = datalist$bounds[2], y = ypos), color = "black")
            }
            p <- p + ggplot2::xlim(datalist$modrange)
            p <- p + ggplot2::theme(legend.key.size = ggplot2::unit(1, "lines"))
            p <- p + ggplot2::ylab(paste("Slope of ", datalist$xname))
            p <- p + ggplot2::xlab(datalist$zname)
            #  suppressMessages(p <- p + ggtheme)
            p <- p + ggplot2::labs(fill = NULL) + ggplot2::guides(colour = "none")
            return(p)
        },
        qqplot = function(image, theme, ggtheme) {
            if (!self$option("qq_plot")) {
                return()
            }

            if (!is.something(image$state$residuals)) {
                return()
            }

            residuals <- image$state$residuals
            #              residuals <- as.numeric(scale(stats::residuals(private$.operator$model)))
            df <- as.data.frame(qqnorm(residuals, plot.it = FALSE))
            plot <- ggplot2::ggplot(data = df, ggplot2::aes(y = y, x = x)) +
                ggplot2::geom_abline(slope = 1, intercept = 0, colour = theme$color[1]) +
                ggplot2::geom_point(ggplot2::aes(x = x, y = y), linewidth = 2, colour = theme$color[1]) +
                ggplot2::xlab("Theoretical Quantiles") +
                ggplot2::ylab("Standardized Residuals")

            plot + ggtheme
        },
        normplot = function(image, theme, ggtheme) {
            if (!self$option("norm_plot")) {
                return()
            }

            if (!is.something(image$state$residuals)) {
                return()
            }

            fill <- theme$fill[2]
            color <- theme$color[1]
            data <- image$state$residuals
            names(data) <- "x"
            # library(ggplot2)
            plot <- ggplot2::ggplot(data = data, ggplot2::aes(x = x)) +
                ggplot2::labs(x = "Residuals", y = "density")
            ## after_stat() is new, so we used to wait for jamovi to update. In the meantime, we used ..density..
            ## now after_stat() works
            plot <- plot + ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)), position = "identity", stat = "bin", color = color, fill = fill)
            #       plot <- plot + ggplot2::geom_histogram(ggplot2::aes(y = "..density.."), position = "identity", stat = "bin", color = color, fill = fill)
            plot <- plot + ggplot2::stat_function(fun = stats::dnorm, args = list(mean = mean(data$x), sd = stats::sd(data$x)))

            themeSpec <- ggplot2::theme(axis.text.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank())
            plot <- plot + ggtheme + themeSpec

            return(plot)
        },
        residPlot = function(image, theme, ggtheme) {
            if (!self$option("resid_plot")) {
                return()
            }

            if (!is.something(image$state$data)) {
                return()
            }


            fill <- theme$fill[2]
            color <- theme$color[1]
            data <- image$state$data
            size <- 2

            # library(ggplot2)
            plot <- ggplot2::ggplot(data = data, ggplot2::aes(x = predicted, y = residuals)) +
                ggplot2::labs(x = "Predicted", y = "Residuals")
            plot <- plot + ggplot2::geom_point(shape = 21, color = color, fill = fill, size = size)

            if (self$option("plot_extremes")) {
                cutoff1 <- quantile(data$residuals, .01)
                cutoff2 <- quantile(data$residuals, .99)
                edata <- data[data$residuals <= cutoff1 | data$residuals >= cutoff2, ]
                plot <- plot + ggplot2::geom_label(
                    data = edata, ggplot2::aes(x = predicted, y = residuals, label = rownames(edata)),
                    show.legend = FALSE,
                    position = ggplot2::position_jitter()
                )
            }
            plot <- plot + ggplot2::geom_hline(yintercept = 0, colour = "gray")
            plot <- plot + ggtheme
            return(plot)
        },
        clusterBoxplot = function(image, ggtheme, theme) {
            ########## working here ##########

            if (!self$option("cluster_boxplot")) {
                return()
            }

            if (!is.something(image$state)) {
                return(FALSE)
            }

            cluster <- image$state$cluster
            data <- image$state$data

            data$cluster <- data[[cluster]]
            plot <- ggplot2::ggplot(data, ggplot2::aes(cluster, .resid)) +
                ggplot2::geom_boxplot()

            if (self$option("plot_extremes")) {
                cutoff1 <- quantile(data$.resid, .01)
                cutoff2 <- quantile(data$.resid, .99)
                edata <- data[data$.resid <= cutoff1 | data$.resid >= cutoff2, ]
                plot <- plot + ggplot2::geom_label(
                    data = edata, ggplot2::aes(x = cluster, y = .resid, label = rownames(edata)),
                    show.legend = FALSE,
                    position = ggplot2::position_jitter()
                )
            }
            plot <- plot + ggplot2::coord_flip()
            plot <- plot + ggplot2::xlab(fromb64(cluster)) + ggplot2::ylab("Residuals")
            plot <- plot + ggtheme

            return(plot)
        },
        clusterResPred = function(image, ggtheme, theme) {
            ########## working here ##########

            if (!self$option("cluster_respred")) {
                return()
            }

            if (!is.something(image$state)) {
                return(FALSE)
            }

            cluster <- image$state$cluster
            data <- image$state$data

            data$cluster <- data[[cluster]]
            plot <- ggplot2::ggplot(data = data, ggplot2::aes(x = .fitted, y = .resid, color = cluster))
            plot <- plot + ggplot2::labs(x = "Predicted", y = "Residuals", color = fromb64(cluster))
            plot <- plot + ggplot2::geom_point(shape = 19, size = 2)
            plot <- plot + ggplot2::geom_hline(yintercept = 0, colour = "gray")

            if (self$option("plot_extremes")) {
                cutoff1 <- quantile(data$.resid, .01)
                cutoff2 <- quantile(data$.resid, .99)
                edata <- data[data$.resid <= cutoff1 | data$.resid >= cutoff2, ]
                plot <- plot + ggplot2::geom_label(
                    data = edata, ggplot2::aes(x = .fitted, y = .resid, label = rownames(edata)),
                    show.legend = FALSE,
                    position = ggplot2::position_jitter()
                )
            }

            plot <- plot + ggtheme
            plot <- plot + ggplot2::theme(legend.position = "bottom")
            return(plot)
        },
        clusterResPredGrid = function(image, ggtheme, theme) {
            if (!self$option("cluster_respred_grid")) {
                return()
            }

            if (!is.something(image$state)) {
                return(FALSE)
            }

            cluster <- image$state$cluster
            data <- image$state$data
            data$cluster <- data[[cluster]]
            plot <- ggplot2::ggplot(data = data, ggplot2::aes(x = .fitted, y = .resid))
            plot <- plot + ggplot2::labs(x = "Predicted", y = "Residuals")
            plot <- plot + ggplot2::geom_point(shape = 19)
            plot <- plot + ggplot2::geom_hline(yintercept = 0, colour = "gray")

            if (self$option("plot_extremes")) {
                cutoff1 <- quantile(data$.resid, .01)
                cutoff2 <- quantile(data$.resid, .99)
                edata <- data[data$.resid <= cutoff1 | data$.resid >= cutoff2, ]
                plot <- plot + ggplot2::geom_label(
                    data = edata, ggplot2::aes(x = .fitted, y = .resid, label = rownames(edata)),
                    show.legend = FALSE,
                    position = ggplot2::position_jitter()
                )
            }

            plot <- plot + ggtheme
            plot <- plot + ggplot2::theme(legend.position = "bottom")
            plot <- plot + ggplot2::facet_wrap(cluster)
            return(plot)
        },
        randHist = function(image, ggtheme, theme) {
            if (!self$option("rand_hist")) {
                return()
            }
            if (!is.something(image$state)) {
                return()
            }

            data <- image$state$data
            fill <- theme$fill[2]
            color <- theme$color[1]
            alpha <- 0.4
            plot <- ggplot2::ggplot(data = data, ggplot2::aes(x = x)) +
                ggplot2::labs(x = "Coefficients", y = "density")

            plot <- plot + ggplot2::geom_histogram(ggplot2::aes(y = ..density..),
                position = "identity",
                stat = "bin", color = color, fill = fill
            )
            plot <- plot + ggplot2::stat_function(fun = dnorm, args = list(mean = mean(data$x), sd = sd(data$x)))

            themeSpec <- ggplot2::theme(
                axis.text.y = ggplot2::element_blank(),
                axis.ticks.y = ggplot2::element_blank()
            )
            plot <- plot + ggtheme + themeSpec

            return(plot)
        }
    ), # end of public
    private = list(
      
        .datamatic = FALSE,
        .results = NULL,
        .operator = NULL,
        .initMainPlot = function() {
          
            
            # first we determine how many main plots we need and which are the terms
            plot_terms<-list()
            main_terms   <-  NULL
            if (self$option("plot_x")) {
                main_terms <- self$options$plot_x
              if (self$option("plot_z")) {
                 main_terms<-c(main_terms,self$options$plot_z)
                 if (self$option("plot_by")) {
                      main_terms<-c(main_terms,self$options$plot_by)
                 }
              }
            ladd(plot_terms)<-main_terms    
            }
            
            for (i in seq_along(self$options$plot_terms)) {
              
              if (is.something(self$options$plot_terms[[i]]))
                              ladd(plot_terms)<-self$options$plot_terms[[i]]  
            }

            if (length(plot_terms)==0)
                return()

            jinfo("PLOTTER: init main plots")
           
            results<-private$.results$get("mainPlots")
            ## subtype is the response type
            subtype<-self$optionValue("plot_scale")
            if (is.null(subtype)) subtype="response"
            
            ## initialize all the main plots
            self$plots<-lapply(seq_along(plot_terms), function(i) {
                  aplot<-aPlot$new(self)
                  aplot$key<-i
                  aplot$plot_raw<-self$options$plot_raw
                  aplot$type<-"scatter"
                  aplot$subtype <- subtype
                  aplot$operator<-private$.operator
                  aplot$datamatic<-private$.datamatic
                  aplot$dep<-self$options$dep
                  aplot$terms<-plot_terms[[i]]
                  results$addItem(key = i)
                  aplot$plotarray<-results$get(key=i)
                  aplot$init()
                  aplot
            })
           
        },
        
        .prepareMainPlot = function() {
          
        
            if (!is.something(self$plots)) {
                return()
            }

            jinfo("PLOTTER: checking main plot")
  
            lapply(self$plots, function(plot) plot$prepare())
                


        },
        .initJnPlot = function() {
          
            if (!self$option("plot_jn")) {
                return()
            }
            if (!is.something(self$options$plot_x))
              return()
            if (!is.something(self$options$plot_z))
              return()
          

            if (self$option("model_type", "ordinal") || self$option("model_type", "multinomial")) {
                self$warning <- list(
                    topic = "jnplotnotes",
                    message = paste("the Johnson-Neyman plot is not available for models of type:", self$options$model_type),
                    head = "warning"
                )
                return()
            }
          

            jinfo("PLOTTER: init johnson-neyman plot")
            resultsgroup <- private$.results$get("jnPlots")

            if (is.something(self$options$plot_by)) {
                plots <- simple_models_labels(self$options$plot_by, private$.operator)
                for (i in 1:nrow(plots)) {
                    resultsgroup$addItem(key = i)
                    resultsgroup$get(key = i)$setTitle(paste(names(plots), plots[i, ], sep = "=", collapse = ","))
                }
            } else {
                aplot <- resultsgroup$addItem(key = 1)
            }
        },
        .prepareJnPlot = function() {
          
            if (!self$option("plot_jn")) {
                return()
            }

            if (self$option("model_type", "ordinal") || self$option("model_type", "multinomial")) {
                return()
            }

            jinfo("PLOTTER: prepare johnson-neyman plot")

            if (!is.something(self$options$plot_x))
              return()
            if (!is.something(self$options$plot_z))
              return()
            
            xobj<-private$.datamatic$variables[[tob64(self$options$plot_x)]]
            zobj<-private$.datamatic$variables[[tob64(self$options$plot_z)]]
            

            ### JN has no meaning if z or x is a factor

            if (zobj$type == "factor") {
                self$warning <- list(
                    topic = "jnplotnotes",
                    message = paste("Variable", zobj$name, "is a factor, the Johnson-Neyman plot cannot be computed."),
                    head = "warning"
                )
                return()
            }

            if (xobj$type == "factor" && xobj$nlevels > 2) {
                self$warning <- list(
                    topic = "jnplotnotes",
                    message = paste("Variable", xobj$name, "is a factor with more than 2 levels. The Johnson-Neyman can be be computed
                                           for continuous or dichotomous predictors"),
                    head = "warning"
                )
                return()
            }


            ### JN fails if there is no interaction between x and z
            .all <- c(zobj$name64, xobj$name64)
            test <- unlist(sapply(.all, function(x) !(.is.scaleDependent(private$.operator$model, x))))
            .issues <- .all[test]
            if (is.something(.issues)) {
                self$warning <- list(
                    topic = "jnplotnotes",
                    message = paste("Variable", paste(fromb64(.issues), collapse = ","), " not in interaction, the Johnson-Neyman plot cannot be produced."),
                    head = "warning"
                )
                return()
            }

            resultsgroup <- private$.results$get("jnPlots")

            expb <- FALSE
            if (self$option("plot_jn_expb")) expb <- TRUE

            model <- private$.operator$model
            ## deal with rescaling
            if (self$options$plot_xoriginal) {
                data <- insight::get_data(model, source = "frame")
                data[[zobj$name64]] <- zobj$rescale(data[[zobj$name64]])
                self$warning <- list(
                    topic = "jnplotnotes",
                    message = paste("Variable", zobj$name, " is in the original scale."), head = "info"
                )
                model <- mf.update(model, data = data)
            }
             
            # deal with moderators
            if (is.something(self$options$plot_by)) {
                models <- simple_models(model, tob64(self$options$plot_by), obj = private$.operator)
                for (i in seq_along(models)) {
                    mod <- models[[i]]
                    datalist <- .johnson_neyman(mod, pred = xobj, mod = zobj$name64, alpha = .05, expb = expb)
                    aplot <- resultsgroup$get(resultsgroup$itemKeys[[i]])
                    aplot$setState(datalist)
                }
            } else {
                aplot <- resultsgroup$get(resultsgroup$itemKeys[[1]])
                datalist <- .johnson_neyman(model, pred = xobj, mod = zobj$name64, alpha = .05, expb = expb)
                aplot$setState(datalist)
            }
        },
        .prepareQqplot = function() {
            if (!self$option("qq_plot")) {
                return()
            }

            residuals <- as.numeric(scale(stats::residuals(private$.operator$model)))
            image <- private$.results$assumptions$get("qqplot")
            image$setState(list(residuals = residuals))
        },
        .prepareNormplot = function() {
            if (!self$option("norm_plot")) {
                return()
            }

            jinfo("PLOTTER: prepare norm plot")

            residuals <- as.data.frame(stats::residuals(private$.operator$model))
            image <- private$.results$assumptions$get("normPlot")
            image$setState(list(residuals = residuals))
        },
        .prepareResidplot = function() {
            if (!self$option("resid_plot")) {
                return()
            }
            residuals <- stats::residuals(private$.operator$model)
            predicted <- stats::predict(private$.operator$model)
            image <- private$.results$assumptions$get("residPlot")
            image$setState(list(data = data.frame(residuals = residuals, predicted = predicted)))
        },
        .initClusterPlots = function() {
            clusters <- private$.operator$formulaobj$clusters
            if (is.null(clusters)) {
                return()
            }
            p <- length(clusters)
            ### remove built clusters combinations
            test <- grep("[\\:\\/]", clusters, invert = TRUE)
            clusters64 <- tob64(clusters[test])
            d <- length(clusters64)
            if (p != d) {
                self$warning <- list(
                    topic = "plotnotes",
                    message = "Assumptions checking plots are not produced for crossed or nested clusters. Please specify them as dataset variables to obtain this plot.",
                    head = "warning"
                )
            }


            if (self$option("cluster_boxplot")) {
                resultsgroup <- private$.results$assumptions$clusterBoxplot
                for (cluster in clusters64) {
                    title <- paste("Clustering variable:", fromb64(cluster))
                    resultsgroup$addItem(cluster)
                    resultsgroup$get(key = cluster)$setTitle(title)
                }
            }

            if (self$option("cluster_respred")) {
                resultsgroup <- private$.results$assumptions$clusterResPred
                for (cluster in clusters64) {
                    title <- paste("Clustering variable:", fromb64(cluster))
                    resultsgroup$addItem(cluster)
                    resultsgroup$get(key = cluster)$setTitle(title)
                }
            }

            if (self$option("cluster_respred_grid")) {
                resultsgroup <- private$.results$assumptions$clusterResPredGrid
                for (cluster in clusters64) {
                    title <- paste("Clustering variable:", fromb64(cluster))
                    resultsgroup$addItem(cluster)
                    resultsgroup$get(key = cluster)$setTitle(title)
                }
            }
        },
        .prepareClusterBoxplot = function() {
            if (!self$option("cluster_boxplot")) {
                return()
            }
            ### we get the clusters from the formulaobj because the model may contain less cluster variables than selected

            clusters <- private$.operator$formulaobj$clusters
            if (is.null(clusters)) {
                return()
            }

            jinfo("PLOTTER: prepare ClusterBoxplot")

            p <- length(clusters)
            ### remove built clusters combinations
            test <- grep("[\\:\\/]", clusters, invert = TRUE)
            clusters <- tob64(clusters[test])
            d <- length(clusters)
            if (p != d) {
                self$warning <- list(
                    topic = "plotnotes",
                    message = "Assumptions checking plots are not produced for crossed or nested clusters. Please specify them as dataset variables to obtain this plot.",
                    head = "warning"
                )
            }

            resultsgroup <- private$.results$assumptions$clusterBoxplot

            data <- lme4::fortify.merMod(private$.operator$model)
            if (inherits(private$.operator$model, "lme")) {
                data$.resid <- stats::resid(private$.operator$model, type = "normalized")
            }

            for (cluster in clusters) {
                resultsgroup$get(key = cluster)$setState(list(cluster = cluster, data = data))
            }
        },
        .prepareClusterResPred = function() {
            if (!self$option("cluster_respred")) {
                return()
            }

            ### we get the clusters from the formulaobj because the model may contain less cluster variables than selected

            clusters <- private$.operator$formulaobj$clusters
            if (is.null(clusters)) {
                return()
            }

            jinfo("PLOTTER: prepare ClusterResPred")

            p <- length(clusters)
            ### remove built clusters combinations
            test <- grep("[\\:\\/]", clusters, invert = TRUE)
            clusters <- tob64(clusters[test])
            d <- length(clusters)
            if (p != d) {
                self$warning <- list(
                    topic = "plotnotes",
                    message = "Assumptions checking plots are not produced for crossed or nested clusters. Please specify them as dataset variables to obtain this plot.",
                    head = "warning"
                )
            }


            resultsgroup <- private$.results$assumptions$clusterResPred
            data <- lme4::fortify.merMod(private$.operator$model)
            if (inherits(private$.operator$model, "lme")) {
                data$.resid <- stats::resid(private$.operator$model, type = "normalized")
            }

            for (cluster in clusters) {
                title <- paste("Clustering variable:", fromb64(cluster))
                id <- cluster
                resultsgroup$get(key = id)$setTitle(title)
                resultsgroup$get(key = id)$setState(list(cluster = cluster, data = data))
            }
        },
        .prepareClusterResPredGrid = function() {
            if (!self$option("cluster_respred_grid")) {
                return()
            }
            ### we get the clusters from the formulaobj because the model may contain less cluster variables than selected

            clusters <- private$.operator$formulaobj$clusters
            if (is.null(clusters)) {
                return()
            }

            jinfo("PLOTTER: prepare ClusterResPredGrid")

            p <- length(clusters)
            ### remove built clusters combinations
            test <- grep("[\\:\\/]", clusters, invert = TRUE)
            clusters <- tob64(clusters[test])
            d <- length(clusters)
            if (p != d) {
                self$warning <- list(
                    topic = "plotnotes",
                    message = "Assumptions checking plots are not produced for crossed or nested clusters. Please specify them as dataset variables to obtain this plot.",
                    head = "warning"
                )
            }

            resultsgroup <- private$.results$assumptions$clusterResPredGrid

            data <- lme4::fortify.merMod(private$.operator$model)
            if (inherits(private$.operator$model, "lme")) {
                data$.resid <- stats::resid(private$.operator$model, type = "normalized")
            }

            for (cluster in clusters) {
                title <- paste("Clustering variable:", fromb64(cluster))
                id <- cluster
                resultsgroup$get(key = id)$setTitle(title)
                resultsgroup$get(key = id)$setState(list(cluster = cluster, data = data))
            }
        },
        .initRandHist = function() {
            if (!self$option("rand_hist")) {
                return()
            }
            jinfo("PLOTTER: init RandHist")

            clusters <- private$.operator$formulaobj$clusters
            random <- private$.operator$formulaobj$random
            random <- lapply(random, function(x) unlist(lapply(x, function(z) z[-length(z)])))

            if (is.null(clusters)) {
                return()
            }

            resultsgroup <- private$.results$assumptions$randHist

            for (i in seq_along(clusters)) {
                cluster <- clusters[i]
                vars <- random[[i]]
                i <- 0
                for (v in vars) {
                    i <- i + 1
                    title <- paste("Coefficient", v, " random across", cluster)
                    id <- tob64(paste0(cluster, i))
                    resultsgroup$addItem(id)
                    resultsgroup$get(key = id)$setTitle(title)
                }
            }
        },
        .prepareRandHist = function() {
            if (!self$option("rand_hist")) {
                return()
            }

            if (is.null(private$.operator$model)) {
                return()
            }

            jinfo("PLOTTER: prepare RandHist")

            clusters <- private$.operator$formulaobj$clusters
            random <- private$.operator$formulaobj$random
            random <- lapply(random, function(x) unlist(lapply(x, function(z) z[-length(z)])))

            if (is.null(clusters)) {
                return()
            }

            res <- lme4::ranef(private$.operator$model)
            resultsgroup <- private$.results$assumptions$randHist

            for (cluster in clusters) {
                clusterres <- res[[tob64(cluster)]]
                vars <- names(clusterres)
                i <- 0
                for (v in vars) {
                    i <- i + 1
                    data <- data.frame(clusterres[, v])
                    names(data) <- "x"
                    id <- tob64(paste0(cluster, i))
                    resultsgroup$get(key = id)$setState(list(data = data))
                }
            }
        }
    ) # end of private
) # end of class


############### addition functions  ###############


#### this is taken from `interactions` package and adjusted for making it compatible with the module graphic system
#### We do not use the package function because it does not work with all models and does not fit well with the graphic rendering mechanism
#### we cite the `interactions` package anyway because this code largely depends on its.

cbands <- function(x2, y1, y3, covy1, covy3, covy1y3, tcrit) {
    upper <- c()
    slopes <- c()
    lower <- c()
    slopesf <- function(i) {
        s <- y1 + y3 * i
        return(s)
    }
    upperf <- function(i, s) {
        u <- s + tcrit * sqrt((covy1 + 2 * i * covy1y3 +
            i^2 * covy3))
        return(u)
    }
    lowerf <- function(i, s) {
        l <- s - tcrit * sqrt((covy1 + 2 * i * covy1y3 +
            i^2 * covy3))
        return(l)
    }
    slopes <- sapply(x2, slopesf, simplify = "vector", USE.NAMES = FALSE)
    upper <- mapply(upperf, x2, slopes)
    lower <- mapply(lowerf, x2, slopes)
    out <- matrix(c(x2, slopes, lower, upper), ncol = 4)
    colnames(out) <- c("z", "slopes", "Lower", "Upper")
    out <- as.data.frame(out)
    return(out)
}


.johnson_neyman <- function(model, predobj, mod, alpha = .05, expb = FALSE) {
  
    pred <- predobj$name64
    if (predobj$type == "factor") {
        pred <- predobj$paramsnames64
    }
    .terms <- c(pred, mod)
    params <- stats::coef(summary(model))
    ## how is the interaction named?
    ints <- c(paste(.terms, collapse = ":"), paste(rev(.terms), collapse = ":"))
    intterm <- rownames(params)[rownames(params) %in% ints]

    .data <- mf.data(model)
    obsrange <- range(.data[[mod]])
    modsd <- stats::sd(.data[[mod]])
    modrange <- c(obsrange[1] - modsd, obsrange[2] + modsd)
    y1 <- params[rownames(params) == pred, 1]
    y3 <- params[rownames(params) == intterm, 1]
    df <- stats::df.residual(model)
    if (is.null(df)) df <- Inf # normal approximation

    alpha <- alpha / 2
    tcrit <- stats::qt(alpha, df = df)
    tcrit <- abs(tcrit)
    vmat <- stats::vcov(model)
    covy3 <- vmat[intterm, intterm]
    covy1 <- vmat[pred, pred]
    covy1y3 <- vmat[intterm, pred]
    a <- tcrit^2 * covy3 - y3^2
    b <- 2 * (tcrit^2 * covy1y3 - y1 * y3)
    c <- tcrit^2 * covy1 - y1^2
    disc <- b^2 - 4 * a * c
    failed <- FALSE
    if (disc <= 0) {
        bounds <- c(-Inf, Inf)
    } else {
        .x1 <- (-b + sqrt(disc)) / (2 * a)
        .x2 <- (-b - sqrt(disc)) / (2 * a)
        result <- c(.x1, .x2)
        bounds <- sort(result, decreasing = FALSE)
    }

    names(bounds) <- c("Lower", "Higher")
    x2 <- seq(from = modrange[1], to = modrange[2], length.out = 1000)
    cbs <- cbands(x2, y1, y3, covy1, covy3, covy1y3, tcrit)
    sigs <- which((cbs$Lower < 0 & cbs$Upper < 0) | (cbs$Lower > 0 & cbs$Upper > 0))
    insigs <- setdiff(1:1000, sigs)
    cbs$Significance <- rep(NA, nrow(cbs))
    cbs$Significance <- factor(cbs$Significance, levels = c("Insignificant", "Significant"))
    index <- 1:1000 %in% insigs
    cbs$Significance[index] <- "Insignificant"
    index <- 1:1000 %in% sigs
    cbs$Significance[index] <- "Significant"
    index <- which(cbs$Significance == "Significant")[1]
    if (!is.na(index) & index != 0) {
        inside <- (cbs[index, "z"] > bounds[1] && cbs[index, "z"] < bounds[2])
        all_sig <- NULL
        if (is.na(which(cbs$Significance == "Insignificant")[1])) {
            all_sig <- TRUE
        } else {
            all_sig <- FALSE
        }
    } else {
        inside <- FALSE
        all_sig <- TRUE
    }
    cbso1 <- cbs[cbs[, "z"] < bounds[1], ]
    cbso2 <- cbs[cbs[, "z"] > bounds[2], ]
    cbsi <- cbs[(cbs[, "z"] > bounds[1] & cbs[, "z"] < bounds[2]), ]

    ### this is the y of the straight line in the middle of the plot
    intercept <- 0
    ## here we exp() everything for plotting odd-ratios
    if (expb) {
        cbsi$slopes <- exp(cbsi$slopes)
        cbsi$Lower <- exp(cbsi$Lower)
        cbsi$Upper <- exp(cbsi$Upper)

        cbso1$slopes <- exp(cbso1$slopes)
        cbso1$Lower <- exp(cbso1$Lower)
        cbso1$Upper <- exp(cbso1$Upper)

        cbso2$slopes <- exp(cbso2$slopes)
        cbso2$Lower <- exp(cbso2$Lower)
        cbso2$Upper <- exp(cbso2$Upper)

        intercept <- 1
    }

    out <- list(
        cbso1 = cbso1,
        cbso2 = cbso2,
        cbsi = cbsi,
        inside = inside,
        failed = failed,
        all_sig = all_sig,
        bounds = bounds,
        obsrange = obsrange,
        modrange = modrange,
        intercept = intercept,
        xname=predobj$name,
        zname=fromb64(mod)
    )
    return(out)
}

#### we do now use this stuff below. Keep it for future reference
.jn_scatter <- function(model, pred, mod, alpha = .05) {
    .terms <- c(pred, mod)
    mat <- attr(stats::terms(model), "factors")
    ## how is the interaction named?
    intterm <- names(which.min(which(apply(mat[rownames(mat) %in% .terms, ], 2, sum) == 2)))

    .data <- mf.data(model)
    obsrange <- range(.data[[mod]])
    modsd <- stats::sd(.data[[mod]])
    modrange <- c(obsrange[1] - modsd, obsrange[2] + modsd)
    params <- stats::coef(summary(model))
    y1 <- params[rownames(params) == pred, 1]
    y3 <- params[rownames(params) == intterm, 1]
    df <- stats::df.residual(model)
    if (is.null(df)) df <- Inf # normal approximation

    alpha <- alpha / 2
    tcrit <- stats::qt(alpha, df = df)
    tcrit <- abs(tcrit)
    vmat <- stats::vcov(model)
    covy3 <- vmat[intterm, intterm]
    covy1 <- vmat[pred, pred]
    covy1y3 <- vmat[intterm, pred]
    a <- tcrit^2 * covy3 - y3^2
    b <- 2 * (tcrit^2 * covy1y3 - y1 * y3)
    c <- tcrit^2 * covy1 - y1^2
    disc <- b^2 - 4 * a * c
    failed <- FALSE
    if (disc <= 0) {
        bounds <- c(-Inf, Inf)
    } else {
        .x1 <- (-b + sqrt(disc)) / (2 * a)
        .x2 <- (-b - sqrt(disc)) / (2 * a)
        result <- c(.x1, .x2)
        bounds <- sort(result, decreasing = FALSE)
    }
}
