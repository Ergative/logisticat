#' Find the value of a predictor variable that has the given probability.
#'
#' @param regression A logistic regression of the form `probability ~ predictor`
#'                   in a `glm` class object.
#' @param probability Value in range (0,1) for which predictor should be found.
#'
#' @return The value of the predictor variable for which the given probability
#'         is predicted.
#' @export
inverse.predict <- function(regression, probability){
  (log(probability/(1-probability)) - stats::coef(regression)[[1]])/stats::coef(regression)[[2]]
}

