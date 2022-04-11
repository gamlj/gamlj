## ----echo = FALSE-------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE, 
  comment = "#>"
)
options(max.print = 1000)
suppressPackageStartupMessages(library(sjmisc))

## -----------------------------------------------------------------------------
library(sjmisc)
data(efc)

# returns a vector
x <- rec(efc$e42dep, rec = "1,2=1; 3,4=2")
str(x)

# returns a data frame
rec(efc, e42dep, rec = "1,2=1; 3,4=2", append = FALSE) %>% head()

## ----echo=FALSE, message=FALSE------------------------------------------------
library(dplyr)

## ----collapse=TRUE------------------------------------------------------------
# select all variables with "cop" in their names, and also
# the range from c161sex to c175empl
rec(
  efc, contains("cop"), c161sex:c175empl, 
  rec = "0,1=0; else=1", 
  append = FALSE
) %>% head()

# center all variables with "age" in name, variable c12hour
# and all variables from column 19 to 21
center(efc, c12hour, contains("age"), 19:21, append = FALSE) %>% head()

## -----------------------------------------------------------------------------
x <- efc[, 3:5]

x %>% str()

to_factor(x, e42dep, e16sex) %>% str()

## -----------------------------------------------------------------------------
# complete data, including new columns
rec(efc, c82cop1, c83cop2, rec = "1,2=0; 3:4=2", append = TRUE) %>% head()

# only new columns
rec(efc, c82cop1, c83cop2, rec = "1,2=0; 3:4=2", append = FALSE) %>% head()

## -----------------------------------------------------------------------------
efc %>% 
  rec(c82cop1, c83cop2, rec = "1,2=0; 3:4=2", append = FALSE) %>% 
  add_columns(efc) %>% 
  head()

## -----------------------------------------------------------------------------
# complete data, existing columns c82cop1 and c83cop2 are replaced
rec(efc, c82cop1, c83cop2, rec = "1,2=0; 3:4=2", append = TRUE, suffix = "") %>% head()

## -----------------------------------------------------------------------------
efc %>% 
  select(c82cop1, c83cop2) %>% 
  rec(rec = "1,2=0; 3:4=2") %>% 
  head()

efc %>% 
  select(c82cop1, c83cop2) %>% 
  mutate(
    c82cop1_dicho = rec(c82cop1, rec = "1,2=0; 3:4=2"),
    c83cop2_dicho = rec(c83cop2, rec = "1,2=0; 3:4=2")
  ) %>% 
  head()

