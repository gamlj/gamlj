F_to_eta2 <- function(f, df, df_error, ci = 0.95, alternative = "greater", ...) {
  .F_to_pve(f, df, df_error, ci = ci, alternative = alternative, es = "eta2", ...)
}

.F_to_pve <- function(f, df, df_error, ci = 0.95, alternative = "greater", es = "eta2",
                      verbose = TRUE, ...) {
  alternative <- match.arg(alternative, c("greater", "two.sided", "less"))
  
  res <- switch(tolower(es),
                eta2 = data.frame(Eta2_partial = (f * df) / (f * df + df_error)),
                epsilon2 = data.frame(Epsilon2_partial = ((f - 1) * df) / (f * df + df_error)),
                omega2 = data.frame(Omega2_partial = ((f - 1) * df) / (f * df + df_error + 1)),
                stop("'es' must be 'eta2', 'epsilon2', or 'omega2'.")
  )
  
  ci_method <- NULL
  if (is.numeric(ci)) {
    stopifnot(length(ci) == 1, ci < 1, ci > 0)
    res$CI <- ci
    ci.level <- if (alternative == "two.sided") ci else 2 * ci - 1
    
    # based on MBESS::ci.R2
    f <- pmax(0, (res[[1]] / df) / ((1 - res[[1]]) / df_error))
    fs <- t(mapply(.get_ncp_F, f, df, df_error, ci.level))
    
    if (isTRUE(verbose) && anyNA(fs)){
      warning("Some CIs could not be estimated due to non-finite F, df, or df_error values.", call. = FALSE)
    }
    
    # This really is a generic F_to_R2
    res$CI_low <- F_to_eta2(fs[, 1], df, df_error, ci = NULL)[[1]]
    res$CI_high <- F_to_eta2(fs[, 2], df, df_error, ci = NULL)[[1]]
    
    ci_method <- list(method = "ncp", distribution = "F")
    if (alternative == "less") {
      res$CI_low <- 0
    } else if (alternative == "greater") {
      res$CI_high <- 1
    }
  } else {
    alternative <- NULL
  }
  
  class(res) <- c("effectsize_table", "see_effectsize_table", class(res))
  attr(res, "ci") <- ci
  attr(res, "ci_method") <- ci_method
  attr(res, "alternative") <- alternative
  return(res)
}


.get_ncp_F <- function(f, df, df_error, conf.level = 0.9) {
  if (!is.finite(f) || !is.finite(df) || !is.finite(df_error)) {
    return(c(NA, NA))
  }
  
  alpha <- 1 - conf.level
  probs <- c(alpha / 2, 1 - alpha / 2)
  
  lambda <- f * df
  ncp <- suppressWarnings(stats::optim(
    par = 1.1 * rep(lambda, 2),
    fn = function(x) {
      p <- stats::pf(q = f, df, df_error, ncp = x)
      
      abs(max(p) - probs[2]) +
        abs(min(p) - probs[1])
    },
    control = list(abstol = 1e-09)
  ))
  f_ncp <- sort(ncp$par) / df
  
  if (f <= stats::qf(probs[1], df, df_error)) {
    f_ncp[2] <- 0
  }
  
  if (f <= stats::qf(probs[2], df, df_error)) {
    f_ncp[1] <- 0
  }
  
  return(f_ncp)
}

