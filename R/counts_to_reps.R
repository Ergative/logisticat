#' Transform pass and fail counts to repeated rows.
#'
#' Given a data frame with a column representing the count _p_ of individuals
#' that passed some binary test, and another column representing the count _q_
#' of individuals that failed the test, return a new data frame where each row
#' in the original is repeated _p_ times with a `TRUE` value in a new
#' `result.col` column and _q_ times with a `FALSE` value in that column.
#'
#' @param data `data.frame` with columns `pass.col` and `fail.col`.
#' @param pass.col `integer` count of individuals passing some binary test
#' @param fail.col `integer` count of individuals failing some binary test
#' @param result.col name to use for result column
#'
#' @return A `data.frame` with counts represented as repeated rows with a
#'   `logical` flag representing a pass or fail.
#' @export
#'
#' @examples
#' d <- tibble::tribble(~treatment, ~day, ~metamorphosed, ~did.not.metamorphose,
#'    "A", 1, 0, 5,
#'    "B", 1, 1, 4,
#'    "C", 1, 0, 6,
#'    "A", 2, 2, 3,
#'    "B", 2, 2, 4,
#'    "C", 2, 0, 6,
#'    "A", 3, 4, 2,
#'    "B", 3, 5, 3,
#'    "C", 3, 2, 7)
#' counts.to.reps(d, metamorphosed, did.not.metamorphose, competent)
counts.to.reps <- function(data, pass.col, fail.col, result.col) {
  pass.sym <- rlang::ensym(pass.col)
  fail.sym <- rlang::ensym(fail.col)
  result.sym <- rlang::ensym(result.col)

  successes <- data %>%
    tidyr::uncount(!!pass.sym) %>%
    dplyr::select(-!!fail.sym) %>%
    dplyr::mutate(!!result.sym := T)

  failures <- data %>%
    tidyr::uncount(!!fail.sym) %>%
    dplyr::select(-!!pass.sym) %>%
    dplyr::mutate(!!result.sym := F)

  rbind(successes, failures)
}
