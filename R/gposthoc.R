gposthoc.populate <- function(model, options, tables) {

    if (!is.something(options$postHoc)) 
        return()
    ginfo("Populate posthoc...")
    terms <- options$postHoc
    dep <- options$dep
    if (length(terms) == 0) 
        return()

    postHocRows <- list()

    for (ph in terms) {

        table <- tables$get(key = ph)
        table$setState(list(1:10))
        term <- jmvcore::composeTerm(ph)
        termB64 <- jmvcore::composeTerm(jmvcore::toB64(ph))
        suppressWarnings({
            none <- .posthoc(model, termB64, "none")
            bonferroni <- .posthoc(model, termB64, "bonferroni")
            holm <- .posthoc(model, termB64, "holm")
            tukey <- .posthoc(model, termB64, "tukey")

        })  # suppressWarnings
        if (is.character(none)) 
            table$setNote("nojoy", WARNS["ph.nojoy"]) else {
            table$setState("there is work done")
            tableData <- as.data.frame(none)
            tableData$contrast <- as.character(tableData$contrast)
            colnames(tableData) <- c("contrast", "estimate", "se", "df", "test", "p")
            tableData$pbonf <- bonferroni[, 6]
            tableData$pholm <- holm[, 6]
            tableData$ptukey <- tukey[, 6]
        }

        .cont <- as.character(tableData$contrast)
        .cont <- gsub(" - ", "-", .cont, fixed = T)
        .cont <- gsub(" / ", "/", .cont, fixed = T)

        .labs64 <- sapply(.cont, function(a) {
            sapply(strsplit(as.character(a), "[- ,/]"), trimws, USE.NAMES = F, simplify = F)
        })
        .labs <- sapply(.labs64, jmvcore::fromB64, simplify = F)
        labs64 <- do.call("rbind", .labs64)
        labs <- do.call("rbind", .labs)
        cols <- paste0("c", 1:ncol(labs64))
        colnames(labs64) <- cols
        colnames(labs) <- cols

        if ("df" %in% names(tableData)) 
            if (tableData$df[1] == Inf) {
                table$getColumn("test")$setTitle("z")
                table$getColumn("df")$setVisible(FALSE)
            }
        tableData <- cbind(labs, tableData)
        sortstring <- paste0("order(", paste0("tableData$", cols, collapse = ","), ")")
        tableData <- tableData[eval(parse(text = sortstring)), ]
        for (col in cols)
            tableData[,col]<-as.character(tableData[,col])
        

        tableData
    }
    ginfo("Populate posthoc done")

}


###### post hoc ##########
.posthoc <- function(x, ...) UseMethod(".posthoc")

.posthoc.default <- function(model, term, adjust) {
    # term<-jmvcore::composeTerm(term)
    termf <- stats::as.formula(paste("~", term))
    data <- mf.getModelData(model)
    suppressMessages({
        referenceGrid <- emmeans::emmeans(model, termf, type = "response", data = data)
        terms <- jmvcore::decomposeTerm(term)
        labs <- referenceGrid@grid[terms]
        newlabs <- sapply(labs, function(a) sapply(a, function(b) jmvcore::toB64(as.character(b))))
        referenceGrid@grid[terms] <- newlabs
        table <- summary(graphics::pairs(referenceGrid), adjust = adjust)
    })
    table[order(table$contrast), ]
}

.posthoc.multinom <- function(model, term, adjust) {
    results <- try({
        dep <- names(attr(stats::terms(model), "dataClass"))[1]
        dep <- jmvcore::composeTerm(dep)
        tterm <- stats::as.formula(paste("~", paste(dep, term, sep = "|")))
        data <- mf.getModelData(model)
        suppressMessages({

            referenceGrid <- emmeans::emmeans(model, tterm, transform = "response", data = data)
            terms <- jmvcore::decomposeTerm(term)
            labs <- referenceGrid@grid[terms]
            newlabs <- sapply(labs, function(a) sapply(a, function(b) jmvcore::toB64(as.character(b))))
            referenceGrid@grid[terms] <- newlabs
            res <- summary(graphics::pairs(referenceGrid, by = dep, adjust = adjust))

        })
        res <- as.data.frame(res)
        res[, dep] <- NULL
        res
    })

    return(results)
}


