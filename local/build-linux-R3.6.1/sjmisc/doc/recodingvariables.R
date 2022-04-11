## ----echo = FALSE-------------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, warning = FALSE, comment = "#>")
suppressPackageStartupMessages(library(sjmisc))

## ----message=FALSE------------------------------------------------------------
library(sjmisc)
data(efc)

## -----------------------------------------------------------------------------
# age, ranged from 65 to 104, in this output
# grouped to get a shorter table
frq(efc, e17age, auto.grp = 5)

# splitting is done at the median by default:
median(efc$e17age, na.rm = TRUE)

# the recoded variable is now named "e17age_d"
efc <- dicho(efc, e17age)
frq(efc, e17age_d)

## -----------------------------------------------------------------------------
x <- dicho(efc$e17age, val.labels = c("young age", "old age"))
frq(x)

## -----------------------------------------------------------------------------
# split at upper quartile
x <- dicho(
  efc$e17age, 
  dich.by = quantile(efc$e17age, probs = .75, na.rm = TRUE), 
  val.labels = c("younger three quarters", "oldest quarter")
)
frq(x)

## -----------------------------------------------------------------------------
data(efc)
x1 <- dicho(efc$e17age)

x2 <- efc %>% 
  dplyr::group_by(c161sex) %>% 
  dicho(e17age) %>% 
  dplyr::pull(e17age_d)

# median age of total sample
frq(x1)

# median age of total sample, with median-split applied
# to distibution of age by subgroups of gender
frq(x2)

## -----------------------------------------------------------------------------
x <- split_var(efc$e17age, n = 3)
frq(x)

## -----------------------------------------------------------------------------
x <- dplyr::ntile(efc$neg_c_7, n = 3)
# for some cases, value "10" is recoded into category "1",
# for other cases into category "2". Same is true for value "13"
table(efc$neg_c_7, x)

x <- split_var(efc$neg_c_7, n = 3)
# no separation of cases with identical values.
table(efc$neg_c_7, x)

## -----------------------------------------------------------------------------
x <- dplyr::ntile(efc$neg_c_7, n = 3)
frq(x)

x <- split_var(efc$neg_c_7, n = 3)
frq(x)

## -----------------------------------------------------------------------------
set.seed(123)
x <- round(runif(n = 150, 1, 10))

frq(x)

frq(group_var(x, size = 5))

group_labels(x, size = 5)

dummy <- group_var(x, size = 5, as.num = FALSE)
levels(dummy) <- group_labels(x, size = 5)
frq(dummy)

dummy <- group_var(x, size = 3, as.num = FALSE)
levels(dummy) <- group_labels(x, size = 3)
frq(dummy)

## -----------------------------------------------------------------------------
dummy <- group_var(x, size = 4, as.num = FALSE)
levels(dummy) <- group_labels(x, size = 4)
frq(dummy)

dummy <- group_var(x, size = 4, as.num = FALSE, right.interval = TRUE)
levels(dummy) <- group_labels(x, size = 4, right.interval = TRUE)
frq(dummy)

## -----------------------------------------------------------------------------
frq(efc$e42dep)

# replace NA with 5
frq(rec(efc$e42dep, rec = "NA=5;else=copy"))

# recode 1 to 2 into 1 and 3 to 4 into 2
frq(rec(efc$e42dep, rec = "1,2=1; 3,4=2"))

# recode 1 to 3 into 4 into 2
frq(rec(efc$e42dep, rec = "min:3=1; 4=2"))

# recode numeric to character, and remaining values
# into the highest value (="hi") of e42dep
frq(rec(efc$e42dep, rec = "1=first;2=2nd;else=hi"))

data(iris)
frq(rec(iris, Species, rec = "setosa=huhu; else=copy", append = FALSE))

# works with mutate
efc %>%
  dplyr::select(e42dep, e17age) %>%
  dplyr::mutate(dependency_rev = rec(e42dep, rec = "rev")) %>%
  head()

# recode multiple variables and set value labels via recode-syntax
dummy <- rec(
  efc, c160age, e17age,
  rec = "15:30=1 [young]; 31:55=2 [middle]; 56:max=3 [old]",
  append = FALSE
)
frq(dummy)

