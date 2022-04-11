# Check dependencies
require('lme4')
require('lmerTest')
require('gamlj')

# Read in the data
#dataset <- read.csv(unz('dataset.csv.zip','dataset.csv'))
dataset <- read.csv("extdata/dataset.csv")

# Formula copy-pasted from jamovi/GAMLj
formula_str <- "Y ~ 1 + X_num + X_cat + X_num:X_cat + (0+X_num|S) + (0+X_cat|S) + (0+X_num:X_cat|S) + (1|S)"


# REML solutions with lmer, default and bobyqa optimizations, and gamljMixed
lmer_res_default <- lmer(formula = formula_str,  data=dataset)
lmer_res_bobyqa <- lmer(formula = formula_str,  data=dataset, control=lmerControl(optimizer='bobyqa'))
gamlj_res <- gamljMixed(formula = as.formula(formula_str),  data=dataset, correlatedEffects = "nocorr",
                        scaling =list(list(var="X_num",type="none"),list(var="X_cat",type="none")) )




# ML solutions with lmer, default and bobyqa optimizations, and gamljMixed
lmer_res_default_ML <- lmer(formula = formula_str,  data=dataset, REML=FALSE)
lmer_res_bobyqa_ML <- lmer(formula = formula_str,  data=dataset, control=lmerControl(optimizer='bobyqa'), REML=FALSE)
gamlj_res_ML <- gamljMixed(formula = as.formula(formula_str),  data=dataset, correlatedEffects = "nocorr", reml=FALSE)

# Display output for comparison
print(summary(lmer_res_bobyqa_ML)$AICtab)
print(gamlj_res_ML)
