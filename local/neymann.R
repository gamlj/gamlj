jnt = function(.lm, predictor, moderator, alpha=.05) {
  require(stringi)
  b1 = coef(.lm)[predictor]
  b3 = coef(.lm)[stri_startswith_fixed(names(coef(.lm)), paste0(predictor,":")) | stri_endswith_fixed(names(coef(.lm)), paste0(":",predictor))]
  se_b1 = coef(summary(.lm))[predictor, 2]
  se_b3 = coef(summary(.lm))[stri_startswith_fixed(names(coef(.lm)), paste0(predictor,":")) | stri_endswith_fixed(names(coef(.lm)), paste0(":",predictor)), 2]
  COV_b1b3 = vcov(.lm)[predictor, stri_startswith_fixed(names(coef(.lm)), paste0(predictor,":")) | stri_endswith_fixed(names(coef(.lm)), paste0(":",predictor))]
  t_crit = qt(1-alpha/2, .lm$df.residual)
  # see Bauer & Curran, 2005
  a = t_crit^2 * se_b3^2 - b3^2
  b = 2 * (t_crit^2 * COV_b1b3 - b1 * b3)
  c = t_crit^2 * se_b1^2 - b1^2
  jn = c(
    (-b - sqrt(b^2 - 4 * a * c)) / (2 * a),
    (-b + sqrt(b^2 - 4 * a * c)) / (2 * a)
  )
  JN = sort(unname(jn))
  JN = JN[JN>=min(.lm$model[,moderator]) & JN<=max(.lm$model[,moderator])]
  JN
}

data<-read.csv("extdata/exercise.csv")
names(data)
mod<-lm(yendu~xage*zexer,data=data)
jnt(mod,"xage","zexer")
