#' Make a logistic regression for a binary outcome based on a continuous predictor.
#'
#' @param data `data.frame` with the columns named in the other arguments.
#' @param predictor Continuous predictor variable.
#' @param outcome `logical` outcome variable with values TRUE or FALSE.
#'
#' @return A logistic regression model in a `glm` class object.
#' @export
logistic.regression <- function(data, predictor, outcome) {
  predictor.sym <- rlang::ensym(predictor)
  outcome.sym   <- rlang::ensym(outcome)

  stats::glm(rlang::new_formula(outcome.sym, predictor.sym),
             data = data,
             family = stats::binomial)
}
