gmeans.init <- function(data, options, theTables, cov_conditioning = NULL) {

    if ("modelSelection" %in% names(options)) {
        modelType <- options$modelSelection
        dep <- options$dep
    } else modelType <- FALSE

    interval <- options$paramCIWidth
    if (!options$eDesc) 
        return()

    .mterms <- options$modelTerms
    mterms <- list()
    if (!options$eCovs) {
        facts <- options$factors
        for (i in seq_along(.mterms)) if (all(.mterms[[i]] %in% facts)) 
            mterms[length(mterms) + 1] <- .mterms[i]
    } else mterms <- .mterms

    if (length(mterms) == 0) 
        return()
    ## remove higher order terms
    mterms <- lapply(mterms, unique)
    mterms <- unique(lapply(mterms, function(a) a[order(a)]))

    
    for (term in mterms) {
        aTable <- theTables$addItem(key = lf.nicifyTerms(jmvcore::composeTerm(term)))
        aTable$getColumn("upper.CL")$setSuperTitle(jmvcore::format("{}% Confidence Interval", interval))
        aTable$getColumn("lower.CL")$setSuperTitle(jmvcore::format("{}% Confidence Interval", interval))

        if (modelType == "multinomial") 
            term <- c(dep, term)

        aList = list()
        for (ter in term) {
            if (ter %in% cov_conditioning$vars) 
                aList[[ter]] <- cov_conditioning$labels(ter) else aList[[ter]] <- levels(data[, jmvcore::toB64(ter)])
        }
        grid <- expand.grid(aList)
        grid <- as.data.frame(grid, stringsAsFactors = F)
        for (i in seq_len(ncol(grid))) {
            colName <- colnames(grid)[[i]]
            aTable$addColumn(name = colName, title = term[i], index = i)
        }
        for (rowNo in seq_len(nrow(grid))) {
            row <- as.data.frame(grid[rowNo, ], stringsAsFactors = F)
            colnames(row) <- term
            aTable$addRow(rowKey = row, values = row)
        }
        if ("modelSelection" %in% names(options)) {
            info <- MINFO[[options$modelSelection]]
            aTable$getColumn("emmean")$setTitle(info$emmeanTitle)
        }

    }

    ### we make the emmean label more precise for GZLM###

    
}  # end of  means init


gmeans.populate <- function(model, options, tables, cov_conditioning = NULL) {

    if (options$eDesc) 
        {

            if ("modelSelection" %in% names(options)) {
                modelType <- options$modelSelection
                dep <- options$dep
            } else modelType <- FALSE

            .mterms <- options$modelTerms
            mterms <- list()
            if (!options$eCovs) {
                facts <- options$factors
                for (i in seq_along(.mterms)) if (all(.mterms[[i]] %in% facts)) 
                  mterms[length(mterms) + 1] <- .mterms[i]
            } else mterms <- .mterms

            ## remove higher order terms
            mterms <- lapply(mterms, unique)
            mterms <- unique(lapply(mterms, function(a) a[order(a)]))

            if (tables$isFilled()) {
                ginfo("Estimated marginal means recycled")
                return()
            }
            ginfo("Estimated marginal means computed")
            for (term in mterms) {
                key <- lf.nicifyTerms(jmvcore::composeTerm(term))
                aTable <- tables$get(key = key)
                if (modelType == "multinomial") 
                  term <- c(dep, term)
                term64 <- jmvcore::toB64(term)
                dataTable <- try(pred.means(model, term64, cov_conditioning = cov_conditioning))
                names(dataTable)[1:length(term)] <- term
                for (i in seq_len(nrow(dataTable))) {
                  values <- dataTable[i, ]
                  aTable$setRow(rowNo = i, values)
                }

                depend <- mi.dependencies(model, term64, "means")
                if (depend != FALSE) 
                  aTable$setNote(depend, WARNS[depend])
            }

        }  # end of eDesc              

}


