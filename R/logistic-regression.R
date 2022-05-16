#' Make a logistic regression for a binary outcome based on a continuous predictor.
#'
#' @param data `data.frame` with the columns named in the other arguments.
#' @param predictor Data variable - continuous predictor.
#' @param outcome Data variable - column with values 1 for success and 0 for failure.
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
