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


#' Create a Data frame with an inverse prediction for each treatment
#'
#' @param data A Data frame with a single row per individual
#' @param predictor Data variable to predict values in regressions.
#' @param outcome Data variable to represent outcomes.
#' @param treatment Data variable representing treatments.
#' @param probability Probability for which to make inverse predictions.
#'
#' @return A tibble with columns for replicate, treatment, and predictor
inverse.predict.by.treatment = function(data, predictor, outcome,
                                        treatment, probability)
{
  treatments <-
    data %>%
    column.to.vector({{treatment}}) %>%
    unique()

  treatment.regressions <-
    treatments %>%
    lapply(function(treatment.level){
      filtered.df <-
        data %>%
        dplyr::filter({{treatment}} == treatment.level[[1]]) %>%
        dplyr::mutate(pred = {{predictor}}, out={{outcome}})
      # We rename the columns above to make it simpler to specify them
      # in a formula without dealing with weird R stuff.
      stats::glm(out ~ pred,
                 data = filtered.df,
                 family = stats::binomial)
    })

  # Inverse predict from each regression we just made.
  treatment.inverse.predictions <-
    # Need to unlist() result of lapply(), because ggplot will not accept a
    # list (which we didn't want anyway)
    unlist(
      lapply(treatment.regressions,
             inverse.predict,
             probability = probability)
    )

  # Populate a new data frame with inverse prediction and treatment so it
  # can be plotted with the correct color scheme.
  tibble::tibble(
    {{predictor}} := treatment.inverse.predictions,
    {{treatment}} := treatments
  )
}



#' Create a Data frame with an inverse prediction for each replicate.
#'
#' @param data A Data frame with a single row per individual
#' @param predictor Data variable to predict values in regressions.
#' @param outcome Data variable to represent outcomes.
#' @param treatment Data variable representing treatments.
#' @param replicate Data variable representing replicates within treatments.
#' @param probability Probability for which to make inverse predictions.
#'
#' @return A tibble with columns for replicate, treatment, and predictor
inverse.predict.by.replicate = function(data, predictor, outcome,
                                        treatment, replicate, probability)
{
  # Make a regression for each replicate. Remember that the actual replicates
  # are not each level of the replicate factor (probably) but every combination
  # of treatment and replicate: there may be a treatment A rep 1 and a treatment
  # B rep 1, etc.

  unique.combos <-
    data %>%
    dplyr::select({{replicate}}, {{treatment}}) %>%
    unique()

  replicate.regressions <-
    unique.combos %>%
    list.df.rows() %>%
    lapply(function(combo){
      filtered.df <-
        data %>%
        dplyr::filter({{replicate}} == combo[[1]]) %>%
        dplyr::filter({{treatment}} == combo[[2]]) %>%
        dplyr::mutate(pred = {{predictor}}, out={{outcome}})
      # We rename the columns above to make it simpler to specify them
      # in a formula without dealing with weird R stuff.
      stats::glm(out ~ pred,
                 data = filtered.df,
                 family = stats::binomial)
    })

  # Inverse predict from each regression we just made.
  replicate.inverse.predictions <-
    # Need to unlist() result of lapply(), because ggplot will not accept a
    # list (which we didn't want anyway)
    unlist(
      lapply(replicate.regressions,
             inverse.predict,
             probability = probability)
    )

  # Populate a new data frame with inverse prediction and treatment so it
  # can be plotted with the correct color scheme.
  tibble::tibble(
    {{predictor}} := replicate.inverse.predictions,
    {{treatment}} := column.to.vector(unique.combos, {{treatment}}),
    {{replicate}} := column.to.vector(unique.combos, {{replicate}})
  )
}
