## ----echo = FALSE-------------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----message=FALSE, warning=FALSE---------------------------------------------
library(sjlabelled)
library(sjmisc) # for frq()-function
library(rlang)

# unlabelled data
dummies <- data.frame(
  dummy1 = sample(1:3, 40, replace = TRUE),
  dummy2 = sample(1:3, 40, replace = TRUE),
  dummy3 = sample(1:3, 40, replace = TRUE)
)

# set labels for all variables in the data frame
test <- set_labels(dummies, labels = c("low", "mid", "hi"))

attr(test$dummy1, "labels")

frq(test, dummy1)

# and set same value labels for two of three variables
test <- set_labels(
  dummies, dummy1, dummy2,
  labels = c("low", "mid", "hi")
)

frq(test)

## ----message=FALSE, warning=FALSE---------------------------------------------
test <- val_labels(dummies, dummy1 = c("low", "mid", "hi"))
attr(test$dummy1, "labels")

# remaining variables are not labelled
frq(test)

## ----message=FALSE, warning=FALSE---------------------------------------------
labels <- c("low_quote", "mid_quote", "hi_quote")
test <- val_labels(dummies, dummy1 = !! labels)
attr(test$dummy1, "labels")

## ----message=FALSE, warning=FALSE---------------------------------------------
variable <- "dummy2"
test <- val_labels(dummies, !! variable := c("lo_var", "mid_var", "high_var"))

# no value labels
attr(test$dummy1, "labels")

# value labels
attr(test$dummy2, "labels")

## ----message=FALSE, warning=FALSE---------------------------------------------
variable <- "dummy3"
labels <- c("low", "mid", "hi")
test <- val_labels(dummies, !! variable := !! labels)
attr(test$dummy3, "labels")

## ----message=FALSE, warning=FALSE---------------------------------------------
dummy <- data.frame(
  a = sample(1:4, 10, replace = TRUE),
  b = sample(1:4, 10, replace = TRUE),
  c = sample(1:4, 10, replace = TRUE)
)

# simple usage
test <- var_labels(dummy, a = "first variable", c = "third variable")

attr(test$a, "label")
attr(test$b, "label")
attr(test$c, "label")

# quasiquotation for labels
v1 <- "First variable"
v2 <- "Second variable"
test <- var_labels(dummy, a = !! v1, b = !! v2)

attr(test$a, "label")
attr(test$b, "label")
attr(test$c, "label")

# quasiquotation for variable names
x1 <- "a"
x2 <- "c"
test <- var_labels(dummy, !! x1 := "First", !! x2 := "Second")

attr(test$a, "label")
attr(test$b, "label")
attr(test$c, "label")

# quasiquotation for both variable names and labels
test <- var_labels(dummy, !! x1 := !! v1, !! x2 := !! v2)

attr(test$a, "label")
attr(test$b, "label")
attr(test$c, "label")

