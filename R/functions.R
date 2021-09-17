

ginfo <- function(what = NULL, obj = NULL) {
    if (j_INFO) {
        if (!is.null(what)) 
            print(what)
        if (!is.null(obj)) {
            print(obj)
            cat("------------\n")
        }
    }
}

mark <- function(what = NULL, obj = NULL) {
    if (j_DEBUG) {
        if (!is.null(what)) 
            print(what) else print("you got here")

        if (!is.null(obj)) {
            print(obj)
            print("#### end ###")
        }
    }
}

.listdeep <- function(aList, n = 0) {
    if (!inherits(aList, "list")) 
        return(n)
    max(sapply(aList, .listdeep, n + 1))
}

.keepShape <- function(mat) {
    if (is.null(dim(mat))) 
        mat <- t(as.matrix(mat))
    mat
}



is.something <- function(x, ...) UseMethod(".is.something")

.is.something.default <- function(obj) (!is.null(obj))

.is.something.list <- function(obj) (length(obj) > 0)

.is.something.numeric <- function(obj) (length(obj) > 0)

.is.something.character <- function(obj) (length(obj) > 0)

.is.something.logical <- function(obj) !is.na(obj)



append_list <- function(alist, aelement, name = NULL) {
    alist[[length(alist) + 1]] <- aelement
    if (!is.null(name)) 
        names(alist)[length(alist)] <- name
    alist
}
prepend_list <- function(alist, aelement, name = NULL) {
    alist <- c(0, alist)
    alist[[1]] <- aelement
    if (!is.null(name)) 
        names(alist)[1] <- name
    alist
}

listify <- function(adata) {
    res <- lapply(1:dim(adata)[1], function(a) as.list(adata[a, ]))
    names(res) <- rownames(adata)
    res
}


sourcifyList <- function(option, def) {
    alist <- option$value
    test <- all(sapply(alist, function(a) a$type) == def)
    if (test) 
        return("")
    paste0(option$name, "=c(", paste(sapply(alist, function(a) paste0(a$var, " = \"", a$type, "\"")), collapse = ", "), ")")
}

getfun <- function(x) {
    if (length(grep("::", x)) > 0) {
        parts <- strsplit(x, "::")[[1]]
        getExportedValue(parts[1], parts[2])
    } else {
        x
    }
}

