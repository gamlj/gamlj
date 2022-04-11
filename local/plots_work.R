library(ggplot2)
devtools::install_github("gamlj/gamlj")


df <- data.frame(
  gp = factor(rep(letters[1:3], each = 10)),
  y = rnorm(30)
)
ds <- do.call(rbind, lapply(split(df, df$gp), function(d) {
  data.frame(mean = mean(d$y), sd = sd(d$y), gp = d$gp)
}))
gobj<-ggplot(df, aes(gp, y)) +
  geom_point() +
  geom_point(data = ds, aes(y = mean), colour = 'red', size = 3)

gobj+ylim(-5,+5)

library(ggplot2)
library(gamlj)


mod<-gamljMixed(
  formula=smile~beer+(1|bar),
  data=beers_bars
)
plot1<-plot(mod,formula =~beer )
### this plot show Y-axis from 5 to 12 

### this plot show Y-axis from 5 to 12 (ignore the warnings)
plot1+ylim(0,15)
