### R code from vignette source 'embedding.Rnw'

###################################################
### code chunk number 1: embedding.Rnw:11-12
###################################################
options(width=80, digits=4, useFancyQuotes=FALSE, prompt=" ", continue=" ")


###################################################
### code chunk number 2: embedding.Rnw:85-88
###################################################
library(car)
m2 <- lm(prestige ~ education, Prestige)
ncvTest(m2, ~ income)


###################################################
### code chunk number 3: embedding.Rnw:91-96 (eval = FALSE)
###################################################
## f3 <- function(meanmod, dta, varmod) {
##   m3 <- lm(meanmod, dta)
##   ncvTest(m3, varmod)
##   }
## f3(prestige ~ education, Prestige, ~ income)


###################################################
### code chunk number 4: embedding.Rnw:104-115
###################################################
f4 <- function(meanmod, dta, varmod) {
   assign(".dta", dta, envir=.GlobalEnv)
   assign(".meanmod", meanmod, envir=.GlobalEnv)
   m1 <- lm(.meanmod, .dta)
   ans <- ncvTest(m1, varmod)
   remove(".dta", envir=.GlobalEnv)
   remove(".meanmod", envir=.GlobalEnv)
   ans
   }
f4(prestige ~ education, Prestige, ~income)
f4(prestige ~ education, Prestige, ~income)


###################################################
### code chunk number 5: embedding.Rnw:120-128 (eval = FALSE)
###################################################
## library(effects)
## fc <- function(dta, formula, terms) {
##  print(m1 <- lm(formula, .dta))
##  Effect(terms, m1)
##  }
## form <- prestige ~ income*type + education
## terms <- c("income", "type")
## fc(Duncan, form, terms)


###################################################
### code chunk number 6: embedding.Rnw:131-139 (eval = FALSE)
###################################################
## library(effects)
## fc.working <- function(dta, formula, terms) {
##  assign(".dta", dta, env=.GlobalEnv)
##  print(m1 <- lm(formula, .dta))
##  Effect(terms, m1)
##  remove(".dta", envir=.GlobalEnv)
##  }
## fc.working(Duncan, form, terms)


###################################################
### code chunk number 7: embedding.Rnw:145-148
###################################################
m1 <- lm(time ~ t1 + t2, Transact)
b1 <- Boot(m1, R=999)
summary(b1)


###################################################
### code chunk number 8: embedding.Rnw:151-152
###################################################
confint(b1)


###################################################
### code chunk number 9: embedding.Rnw:156-157 (eval = FALSE)
###################################################
## .carEnv <- new.env(parent=emptyenv())


###################################################
### code chunk number 10: embedding.Rnw:161-204 (eval = FALSE)
###################################################
## Boot.default <- function(object, f=coef, labels=names(coef(object)),
##                      R=999, method=c("case", "residual")) {
##   if(!(require(boot))) stop("The 'boot' package is missing")
##   f0 <- f(object)
##   if(length(labels) != length(f0)) labels <- paste("V", seq(length(f0)), sep="")
##   method <- match.arg(method)
##   if(method=="case") {
##      boot.f <- function(data, indices, .fn) {
##       assign(".boot.indices", indices, envir=car:::.carEnv)
##       mod <- update(object, subset=get(".boot.indices", envir=car:::.carEnv))
##       if(mod$qr$rank != object$qr$rank){
##             out <- .fn(object)
##             out <- rep(NA, length(out)) } else  {out <- .fn(mod)}
##      out
##      }
##     } else {
##     boot.f <- function(data, indices, .fn) {
##       first <- all(indices == seq(length(indices)))
##       res <- if(first) object$residuals else
##                   residuals(object, type="pearson")/sqrt(1 - hatvalues(object))
##       res <- if(!first) (res - mean(res)) else res
##       val <- fitted(object) + res[indices]
##       if (!is.null(object$na.action)){
##             pad <- object$na.action
##             attr(pad, "class") <- "exclude"
##             val <- naresid(pad, val)
##             }
##       assign(".y.boot", val, envir=car:::.carEnv)
##       mod <- update(object, get(".y.boot", envir=car:::.carEnv) ~ .)
##       if(mod$qr$rank != object$qr$rank){
##             out <- .fn(object)
##             out <- rep(NA, length(out)) } else  {out <- .fn(mod)}
##       out
##       }
##   }
##   b <- boot(data.frame(update(object, model=TRUE)$model), boot.f, R, .fn=f)
##   colnames(b$t) <- labels
##   if(exists(".y.boot", envir=car:::.carEnv))
##      remove(".y.boot", envir=car:::.carEnv)
##   if(exists(".boot.indices", envir=car:::.carEnv))
##      remove(".boot.indices", envir=car:::.carEnv)
##   b
##   }


