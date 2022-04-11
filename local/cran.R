#devtools::check_win_devel()
devtools::check()
usethis::use_vignette("gamljGLM")

browseVignettes("gamlj")

devtools::build_manual()

devtools::check()

data("beers_bars")

mod<-gamlj::gamljMixed(
        formula = smile ~ 1 + beer+( 1|bar ),
        data = beers_bars)

str(mod)
gamlj::gamljGlm(
  formula = smile ~ 1 + beer,
  data = beers_bars)

