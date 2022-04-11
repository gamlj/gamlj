## ----echo = FALSE-------------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, warning = FALSE, comment = "#>")
suppressPackageStartupMessages(library(sjmisc))

## ----message=FALSE------------------------------------------------------------
library(sjmisc)
library(dplyr)
data(efc)

## -----------------------------------------------------------------------------
frq(efc$c161sex)

## -----------------------------------------------------------------------------
# find all variables with "dependency" in name or label
find_var(efc, "dependency", out = "table")

## -----------------------------------------------------------------------------
flat_table(efc, e42dep, c161sex)

## -----------------------------------------------------------------------------
flat_table(efc, e42dep, c161sex, margin = "col")

## -----------------------------------------------------------------------------
efc$burden <- rec(
  efc$neg_c_7,
  rec = c("min:9=1 [low]; 10:12=2 [moderate]; 13:max=3 [high]; else=NA"),
  var.label = "Subjective burden",
  as.num = FALSE # we want a factor
)
# print frequencies
frq(efc$burden)

## -----------------------------------------------------------------------------
efc %>% 
  select(burden, c161sex) %>% 
  group_by(c161sex) %>% 
  frq()

## -----------------------------------------------------------------------------
# convert variable to labelled factor, because we then 
# have the labels as factor levels in the output
efc$e42dep <- to_label(efc$e42dep, drop.levels = T)
efc %>%
  select(e42dep, burden, c161sex, quol_5) %>%
  group_by(e42dep) %>%
  tidyr::nest()

## -----------------------------------------------------------------------------
efc %>%
  select(e42dep, burden, c161sex, quol_5) %>%
  group_by(e42dep) %>%
  tidyr::nest() %>% 
  na.omit() %>%       # remove nested group for NA
  arrange(e42dep) %>% # arrange by order of levels
  mutate(models = purrr::map(
    data, ~ 
    lm(quol_5 ~ burden + c161sex, data = .))
  ) %>%
  spread_coef(models)

## -----------------------------------------------------------------------------
efc %>%
  select(e42dep, burden, c161sex, quol_5) %>%
  group_by(e42dep) %>%
  tidyr::nest() %>% 
  na.omit() %>%       # remove nested group for NA
  arrange(e42dep) %>% # arrange by order of levels
  mutate(models = purrr::map(
    data, ~ 
    lm(quol_5 ~ burden + c161sex, data = .))
  ) %>%
  spread_coef(models, burden3)

