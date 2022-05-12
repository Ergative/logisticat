#' Transform data with counts of binary outcomes into repeated rows.
#'
#' Given a `data.frame` containing results of a test with two possible outcomes
#' that is arranged with the number of "successes" per group in one column
#' and the number of "failures" per group in another column, return a new
#' `data.frame` that has a single outcome column with `1` for every success
#' and `0` for every failure, mapping each outcome to a single row.
#'
#' The output is the format `glm()` expects when constructing logistic
#' regressions. The "successes" and "failures" can be any two mutually
#' exclusive states. Note that, while one would expect these to be logical
#' (TRUE and FALSE) values, `glm()` requires numeric values. You can think
#' about these numbers as representing the probability of success, which is
#' 1 for success that actually happened and 0 for successes that did not.
#'
#' @param data `data.frame` with columns for success and failure counts.
#' @param success.counts Column with number of successes per group.
#' @param failure.counts Column with number of failures per group.
#' @param outcome Name to use for the outcome column.
#'
#' @return A `data.frame` with counts represented as repeated rows with a
#'   number 1 representing a success or a 0 represnting a failure.
#' @export
outcome.counts.to.rows <- function(data, success.counts, failure.counts, outcome) {
  success.counts.sym <- rlang::ensym(success.counts)
  failure.counts.sym <- rlang::ensym(failure.counts)
  outcome.sym        <- rlang::ensym(outcome)

  # Make as many copies of each row as there were successes,
  # throw away the success/failure counts, and mark each one
  # has having the "1" outcome.
  successes <- data %>%
    tidyr::uncount(!!success.counts.sym) %>%
    dplyr::select(-!!failure.counts.sym) %>%
    dplyr::mutate(!!outcome.sym := 1)

  # Make as many copies of each row as there were failures,
  # throw away the success/failure counts, and mark each one
  # has having the "0" outcome.
  failures <- data %>%
    tidyr::uncount(!!failure.counts.sym) %>%
    dplyr::select(-!!success.counts.sym) %>%
    dplyr::mutate(!!outcome.sym := 0)

  # Concatenate all the rows into a single table.
  rbind(successes, failures)
}
