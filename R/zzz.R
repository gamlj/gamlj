.onLoad <- function(libname, pkgname) {
    invisible(suppressPackageStartupMessages(sapply(c("car", "lme4"), requireNamespace, quietly = TRUE)))
}
