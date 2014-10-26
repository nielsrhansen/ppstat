ppLasso <- function(
  formula,
  data,
  family = Hawkes("identity"),
  support = 1,
  N = 200,
  Delta,
  ...) {
  
  model <- pointProcessModel(
    formula = formula,
    data = data,
    family = family,
    support = support,
    N = N,
    Delta = Delta,
    coefficients = 0,
    fit = FALSE,
    varMethod = 'none',
    selfStart = FALSE,
    ...)
  
  ppmFit(model, optim = "glmnet", ...)
}
