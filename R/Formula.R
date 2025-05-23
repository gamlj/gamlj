## an object to obtain formulas in different contexts from lists of terms
# source("R/functions.R")
gFormula <- R6::R6Class(
    "gFormula",
    class = TRUE,
    cloneable = FALSE,
    public = list(
        dep = NULL,
        offset = NULL,
        clusters = NULL,
        fixed_intercept = NULL,
        hasTerms = FALSE,
        isProper = FALSE,
        anova_terms = NULL,
        params_terms = NULL,
        interface = "standard",
        fixed_formula = function() {
            private$.buildfixed(self$dep, self$fixed)
        },
        fixed_formula64 = function() {
            private$.buildfixed(tob64(self$dep), tob64(self$fixed))
        },
        rhsfixed_formula = function() {
            private$.rhsfixed(self$fixed)
        },
        rhsfixed_formula64 = function() {
            private$.rhsfixed(tob64(self$fixed))
        },
        random_formula = function() {
       
            alist <- private$.buildrandom(self$random, self$random_corr, "plain")
            if (is.something(alist)) {
                paste("(", alist, ")", collapse = " + ")
            }
        },
        random_formula64 = function() {
            alist <- private$.buildrandom(self$random, self$random_corr, "b64")
            if (is.something(alist)) {
                paste("(", alist, ")", collapse = " + ")
            }
        },
        listify_random64 = function() {
            ## there are some functions that require a list of random effect formula
            private$.buildrandom(self$random, self$random_corr, "b64")
        },
        listify_random = function() {
            ## there are some functions that require a list of random effect formula
            private$.buildrandom(self$random, self$random_corr, "plain")
        },
        listify_random_formulas64 = function() {
            ## there are some functions that require a list of random effect formula
            ## in formula types
            alist <- private$.buildrandom(self$random, self$random_corr, "b64")
            lapply(alist, function(x) formula(paste("~", x)))
        },
        formula = function() {
            paste(c(self$fixed_formula(), self$random_formula()), collapse = " + ")
        },
        formula64 = function() {
            paste(c(self$fixed_formula64(), self$random_formula64()), collapse = " + ")
        },
        nested_tested_fixed = function(obj) {
            ## we need to sort the tems otherswize x:z will be considered different from z:x
            if (is.something(obj$fixed)) {
                full <- lapply(self$fixed, sort)
                nested <- lapply(obj$fixed, sort)
                return(private$.buildfixed(NULL, setdiff(full, nested)))
            }
        },
        nested_tested_random = function(obj) {
            ## TODO: for R, we should do some sorting of interactions

            return(private$.buildfixed(NULL, setdiff(unlist(self$random), unlist(obj$random))))
        },
        keep = function(term) {
            w <- unlist(lapply(private$.fixed, function(x) (all(term == x) || is.numeric(x))))
            fixed <- private$.fixed[w]
            f <- private$.buildfixed(tob64(self$dep), tob64(fixed))
            r <- NULL
            if (is.something(self$random)) {
                alist <- lapply(self$random, function(z) {
                    lapply(z, function(x) {
                        w <- all(x[-length(x)] %in% term) || x[1] == "Intercept"
                        if (isFALSE(w)) {
                            return(NULL)
                        } else {
                            return(x)
                        }
                    })
                })
                r <- private$.buildrandom(clean_lol(alist), self$random_corr, "b64")
                r <- paste("(", r, ")", collapse = " + ")
            }
            paste(f, r, sep = " + ")
        },
        update_terms = function(data) {
            .formulalist <- self$fixed
            ## we want to be sure that the order of terms is the same used by the estimator
            ## because in R it may arrive a formula like y~x:z+z+x, which would processed by
            ## lm() (or other estimator) in different order
            .formula <- jmvcore::composeFormula(NULL, .formulalist)
            .formula <- attr(terms(as.formula(.formula)), "term.labels")
            .formulalist <- jmvcore::decomposeTerms(.formula)
            self$anova_terms <- fromb64(.formulalist)
            self$params_terms <- fromb64(colnames(model.matrix(as.formula(self$rhsfixed_formula64()), data)))
        },
        reduced_random = function() {
            re <- self$random
            termslist <- list()
            listnames <- list()
            for (i in seq_along(re)) {
                l <- sapply(re[[i]], length)
                l <- which(l == max(l))
                for (j in l) {
                    re <- self$random
                    if (length(l) > 1 & re[[i]][[j]][[1]] == "Intercept") {
                        next
                    }

                    name <- paste(jmvcore::composeTerm(re[[i]][[j]][-length(re[[i]][[j]])]), re[[i]][[j]][length(re[[i]][[j]])], sep = " | ")
                    listnames[[length(listnames) + 1]] <- name
                    one <- re[[i]]
                    one <- one[-j]
                    re[[i]] <- one
                    termslist[[length(termslist) + 1]] <- re
                }
            }
            res <- lapply(termslist, function(x) {
                alist <- private$.buildrandom(clean_lol(x), self$random_corr, "b64")
                if (!is.something(alist)) {
                    return()
                }
                paste("(", alist, ")", collapse = " + ")
            })
            names(res) <- listnames
            res
        }
    ), # end of public
    active = list(
        fixed = function(alist) {
            if (missing(alist)) {
                return(private$.fixed)
            }
            if (is.null(self$fixed_intercept)) {
                stop("Formula$fixed_intercept has not been set")
            }
            alist <- c(as.numeric(self$fixed_intercept), alist)
            self$hasTerms <- length(alist) > 1
            self$isProper <- (self$fixed_intercept | self$hasTerms)
            private$.fixed <- alist
        },
        random = function(alist) {
            if (missing(alist)) {
                return(private$.random)
            }
            correl <- self$random_corr
            # remove empty sublists
            alist <- alist[sapply(alist, function(a) !is.null(unlist(a)))]
            # get clusters

            self$clusters <- unique(unlist(lapply(alist, function(z) lapply(z, function(x) x[length(x)]))))
            mark(self$clusters)
            ## this is for R. It overrides the re_corr option

            if (length(alist) > 1) {
                if (correl == "none") warning("Option re_corr='none' has been overriden by random effect input structure")
                correl <- "block"
            }

            # split in sublists if option re_corr=none
            if (correl == "none") {
                alist <- alist[[1]]
                alist <- lapply(alist, list)
            }
            ## we want to be sure that each cluster has its own list
            r <- list()
            alist64<-tob64(alist)
            clusters64<-tob64(self$clusters)
            for (x in alist64) {
                for (cluster in clusters64) {
                  mark(cluster,x)
                    w <- grep(cluster, x)
                    if (length(w) > 0) {
                        ladd(r) <- fromb64(x[w])
                    }
                }
            }
            alist <- r

            ## we want to be sure that the terms are ordered by order
            alist <- lapply(alist, function(x) x[order(sapply(x, length))])
           
            private$.random <- alist
        },
        random_corr = function(avalue) {
            if (missing(avalue)) {
                return(private$.random_corr)
            }
            if (is.null(avalue)) {
                return(avalue)
            }
            private$.random_corr <- avalue
        }
    ), # end of active
    private = list(
        .fixed = NULL,
        .random = NULL,
        .random_corr = "all",
        .buildfixed = function(dep, terms) {
            if (self$interface == "standard") {
                return(gsub("`0`", 0, gsub("`1`", 1, jmvcore::composeFormula(dep, terms))))
            }

            if (self$interface == "success") {
                dep <- paste0("cbind(", paste0(dep, collapse = ","), ")")
                rhform <- gsub("`0`", 0, gsub("`1`", 1, jmvcore::composeFormula(NULL, terms)))
                return(paste0(dep, rhform))
            }
        },
        .rhsfixed = function(terms) {
            gsub("`0`", 0, gsub("`1`", 1, jmvcore::composeFormula(NULL, terms)))
        },
        .buildrandom = function(terms, correl, encoding) {
        
            .intercept <- "Intercept"
            if (encoding == "b64") {
                terms <- lapply(terms, function(term) {
                    lapply(term, function(x) {
                        x64 <- tob64(x)
                        test <- grep("\\/", x)
                        if (length(test) > 0) {
                            asplit <- stringr::str_split(x[[test]], "\\/")[[1]]
                            x64[[test]] <- paste0(tob64(asplit), collapse = "/")
                        }
                        test <- grep("\\:", x)
                        if (length(test) > 0) {
                            asplit <- stringr::str_split(x[[length(x)]], "\\:")[[1]]
                            x64[[test]] <- paste0(tob64(asplit), collapse = ":")
                        }

                        x64
                    })
                })
                .intercept <- tob64(.intercept)
            }


            clusters <- lapply(terms, function(x) tail(x[[1]], 1))

            alist <- lapply(terms, function(x) {
                unlist(lapply(x, function(z) {
                    jmvcore::composeTerm(head(z, -1))
                }))
            })
            alist <- lapply(seq_along(alist), function(i) {
                r <- paste(paste(alist[[i]], collapse = " + "), clusters[[i]], sep = " | ")
                if (length(grep(.intercept, r)) > 0) {
                    gsub(.intercept, 1, r)
                } else {
                    paste("0+", r)
                }
            })
            return(alist)
        },
        .composeRandom = function(term) {
            w <- which(term == "Intercept")
            if (length(w) > 0) {
                int <- 1
                term <- term[-w]
            } else {
                int <- 0
            }
            term <- c(int, term)
            astring <- paste(paste(term[-length(term)], sep = "+", collapse = "+"), term[length(term)], sep = "|")
            astring
        },
        .composeRandom64 = function(term) {
            w <- which(term == "Intercept")
            if (length(w) > 0) {
                int <- 1
                term <- term[-w]
            } else {
                int <- 0
            }
            term <- c(int, tob64(term))
            astring <- paste(paste(term[-length(term)], sep = "+", collapse = "+"), term[length(term)], sep = "|")
            astring
        }
    ) # end of private
) # end of class

# this does the opposite, from formulas to lists.
rFormula <- R6::R6Class(
    "rFormula",
    class = TRUE,
    cloneable = FALSE,
    public = list(
        intercept = NULL,
        dep = NULL,
        factors = NULL,
        covs = NULL,
        terms = NULL,
        random = NULL,
        clusters = list(),
        initialize = function(aformula, data = NULL) {
            aformula <- as.formula(aformula)
            bars <- lme4::findbars(aformula)
            if (!is.null(bars)) {
                test <- (length(grep("^1", bars)) + length(grep("^0", bars))) == length(bars)
                if (!test) stop("The random intercept should be explicitly defined for each component with either `1` (present) or `0` (absent)")
                bars <- gsub("^1", "Intercept", bars)
                bars <- gsub("^0", "", bars)
                barslist <- lapply(bars, function(b) strsplit(b, "|", fixed = T)[[1]])
                self$random <- lapply(barslist, function(b) {
                    form <- formula(paste0("~", b[[1]]))
                    expanded <- reformulate(labels(terms(form)))
                    .terms <- jmvcore::decomposeFormula(expanded)
                    cluster <- trimws(b[[2]])
                    ladd(self$clusters) <- cluster
                    lapply(.terms, function(t) c(t, cluster))
                })
            }

            fformula <- lme4::nobars(aformula)
            if (!is.null(data)) {
                if (!is.data.frame(data)) {
                    stop("The input data is not a dataframe or it does not exist.")
                }


                self$dep <- jmvcore::marshalFormula(fformula, data, from = "lhs", permitted = c("numeric", "factor"))
                self$factors <- jmvcore::marshalFormula(fformula, data, from = "rhs", permitted = "factor")
                self$covs <- jmvcore::marshalFormula(fformula, data, from = "rhs", permitted = "numeric")
            }
            self$intercept <- as.logical(attr(terms(fformula), "intercept"))
            self$terms <- jmvcore::decomposeTerms(attr(terms(fformula), "term.labels"))
        }
    ) # end of public
) # end of class


#  aformula<-y~1+x+I(x^2)+(1+x1+x1:x2|cluster2)+(0+k+x|cluster1)+(1|cluster3)+(1+x|cluster4)
#  f<-rFormula$new(aformula)
# f$random
# l<-gFormula$new()
# l$random<-f$random
# l$random_formula()
# lreduced_random()
#
