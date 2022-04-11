#install.packages('jmvtools', repos=c('https://repo.jamovi.org', 'https://cran.r-project.org'))
#install.packages('jmvcore')
install.packages('jmvtools', repos=c('https://repo.jamovi.org'))
devtools::install_github('jamovi/jmvcore')



#devtools::install_github("gamlj/gamlj")
source("local/functions.R")
library(jmvcore)

home<-"C:\\Program Files/jamovi 2.2.5.0/"
jmvtools::version()
jmvtools::check(home=home)

installme("gamlj",home = home)

#write('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', file = "~/.Renviron", append = TRUE)
