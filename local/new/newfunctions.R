standardize_coefficients<-function(model) {
  vars<-unlist(insight::find_variables(model))
  vars<-setdiff(vars,unlist(names(attr(stats::model.matrix(model),"contrasts"))))
  data<-insight::get_data(data)[,preds]
  for (v in covs)
    data[[v]]<-scale(data[[v]])
  form<-insight::find_formula(mod)
  beta<-coef(update(mod,formula=xform[[1]], data=data))
}

