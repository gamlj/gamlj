## ----echo = FALSE-------------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## -----------------------------------------------------------------------------
library(haven)
x <- labelled(
  c(1:3, tagged_na("a", "c", "z"), 4:1),
  c("Agreement" = 1, "Disagreement" = 4, "First" = tagged_na("c"),
    "Refused" = tagged_na("a"), "Not home" = tagged_na("z"))
  )

print(x)

## -----------------------------------------------------------------------------
is.na(x)

as_factor(x)

is.na(as_factor(x))

## ----message=FALSE------------------------------------------------------------
library(sjlabelled)
# sjlabelled-sample data, an atomic vector with label attributes
data(efc)
str(efc$e16sex)

## -----------------------------------------------------------------------------
get_labels(efc$e42dep)

## -----------------------------------------------------------------------------
get_labels(efc$e42dep, values = "p")

## -----------------------------------------------------------------------------
x <- factor(c("low", "mid", "low", "hi", "mid", "low"))
get_labels(x)

## -----------------------------------------------------------------------------
x <- factor(c("low", "mid", "low", "hi", "mid", "low"))
get_labels(x, attr.only = TRUE)

## -----------------------------------------------------------------------------
# get labels, including tagged NA values
x <- labelled(
  c(1:3, tagged_na("a", "c", "z"), 4:1),
  c("Agreement" = 1, "Disagreement" = 4, "First" = tagged_na("c"),
    "Refused" = tagged_na("a"), "Not home" = tagged_na("z"))
)
get_labels(x)

## -----------------------------------------------------------------------------
get_labels(x, non.labelled = TRUE)

## -----------------------------------------------------------------------------
get_labels(x, values = "n", drop.na = FALSE)

## -----------------------------------------------------------------------------
print(x)

get_values(x)

## -----------------------------------------------------------------------------
get_values(x, drop.na = TRUE)

## -----------------------------------------------------------------------------
x <- sample(1:4, 20, replace = TRUE)

# return new labelled vector
x <- set_labels(x, labels = c("very low", "low", "mid", "hi"))
x

## -----------------------------------------------------------------------------
x <- c(2, 2, 3, 3, 2)
x <- set_labels(x, labels = c("a", "b", "c"))
x

## -----------------------------------------------------------------------------
x <- c(2, 2, 3, 3, 2)
x <- set_labels(
  x, 
  labels = c("a", "b", "c"), 
  force.labels = TRUE
)
x

## -----------------------------------------------------------------------------
x <- c(1, 2, 3, 2, 4, NA)
x <- set_labels(x, labels = c("yes", "maybe", "no"))
x

## -----------------------------------------------------------------------------
x <- c(1, 2, 3, 2, 4, NA)
x <- set_labels(
  x, 
  labels = c("yes", "maybe", "no"),
  force.values = FALSE
)
x

## -----------------------------------------------------------------------------
x <- c(1, 2, 3, 2, 4, 5)
x <- set_labels(
  x, 
  labels = c("strongly agree" = 1, 
             "totally disagree" = 4, 
             "refused" = 5,
             "missing" = 9)
)
x

## -----------------------------------------------------------------------------
tmp <- data.frame(
  a = c(1, 2, 3),
  b = c(1, 2, 3),
  c = c(1, 2, 3)
)

labels <- list(
  c("one", "two", "three"),
  c("eins", "zwei", "drei"),
  c("un", "dos", "tres")
)

tmp <- set_labels(tmp, labels = labels)
str(tmp)

## ----message=FALSE------------------------------------------------------------
library(dplyr)
library(sjmisc) # for frq()
data(efc)

efc %>% 
  select(c82cop1, c83cop2, c84cop3) %>% 
  set_labels(labels = c("not often" = 1, "very often" = 4)) %>% 
  frq()

## -----------------------------------------------------------------------------
get_label(efc$e42dep)

get_label(efc, e42dep, e16sex, e15relat)

## -----------------------------------------------------------------------------
dummy <- c(1, 2, 3)
testit <- function(x) get_label(x, def.value = deparse(substitute(x)))
# returns name of vector, if it has no variable label
testit(dummy)

## -----------------------------------------------------------------------------
data(iris)

# returns no labels, because iris-data is not labelled
get_label(iris)

# returns the column name as default labels, if data is not labelled
get_label(iris, def.value = colnames(iris))

# labels are parsed in a readable way
get_label(iris, def.value = colnames(iris), case = "parsed")

## -----------------------------------------------------------------------------
x <- sample(1:4, 10, replace = TRUE)

# return new vector
x <- set_label(x, label = "Dummy-variable")
str(x)

# label existing vector
set_label(x) <- "Another Dummy-variable"
str(x)

## -----------------------------------------------------------------------------
x <- data.frame(
  a = sample(1:4, 10, replace = TRUE),
  b = sample(1:4, 10, replace = TRUE),
  c = sample(1:4, 10, replace = TRUE)
)
x <- set_label(x, label = c("Variable A",
                            "Variable B",
                            "Variable C"))

str(x)

get_label(x)

## -----------------------------------------------------------------------------
x <- data.frame(
  a = sample(1:4, 10, replace = TRUE),
  b = sample(1:4, 10, replace = TRUE),
  c = sample(1:4, 10, replace = TRUE)
)

library(magrittr) # for pipe
x %>% 
  var_labels(
    a = "Variable A",
    b = "Variable B",
    c = "Variable C"
  ) %>% 
  str()

## -----------------------------------------------------------------------------
x <- sample(1:8, 100, replace = TRUE)
# show value distribution
table(x)

# set value 1 and 8 as tagged missings
x <- set_na(x, na = c(1, 8), as.tag = TRUE)
x

# show value distribution, including missings
table(x, useNA = "always")

# now let's see, which NA's were "1" and which were "8"
print_tagged_na(x)

x <- factor(c("a", "b", "c"))
x

# set NA into existing vector
x <- set_na(x, na = "b", as.tag = TRUE)
x

## -----------------------------------------------------------------------------
get_na(x)

## -----------------------------------------------------------------------------
get_na(x, as.tag = TRUE)

## -----------------------------------------------------------------------------
library(sjmisc) # for replace_na()
data(efc)
str(efc$c84cop3)

efc$c84cop3 <- set_na(efc$c84cop3, na = c(2, 3), as.tag = TRUE)
get_na(efc$c84cop3, as.tag = TRUE)

# this would replace all NA's into "2"
dummy <- replace_na(efc$c84cop3, value = 2)

# labels of former tagged NA's are preserved
get_labels(dummy, drop.na = FALSE, values = "p")
get_na(dummy, as.tag = TRUE)

# No more NA values
frq(dummy)


# In this example, the tagged NA(2) is replaced with value 2
# the new value label for value 2 is "restored NA"
dummy <- replace_na(efc$c84cop3, value = 2, na.label = "restored NA", tagged.na = "2")

# Only one tagged NA remains
get_labels(dummy, drop.na = FALSE, values = "p")
get_na(dummy, as.tag = TRUE)

# Some NA values remain
frq(dummy)

## -----------------------------------------------------------------------------
str(efc$c82cop1)

efc$c82cop1 <- set_na(efc$c82cop1, na = c(2, 3), as.tag = TRUE)
get_na(efc$c82cop1, as.tag = TRUE)

efc$c82cop1 <- replace_labels(efc$c82cop1, labels = c("new NA label" = tagged_na("2")))

get_na(efc$c82cop1, as.tag = TRUE)

