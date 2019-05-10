context("R interface")

data("qsport")
obj<-gamlj::gamljGLM(
    formula = performance ~ hours,
    data = qsport)
preds<-gamlj_predict(obj)

test_that("glm predict", {
  expect_equal(round(mean(preds),2),37.88)
})
