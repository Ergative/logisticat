#' Create a geom representing a logistic regression on the data.
#'
#' @note x and y must be specified as aesthetics on the ggplot to which this is added.
#'
#' @param individual.rows Data frame with each row representing a pass or fail.
#' @param group Data variable specifying what groups to make into regressions.
#' @param color Data variable specifying how to color regression lines.
#' @param alpha Alpha value for line.
#' @param ... Additional arguments to pass to the underlying geom.
#'
#' @keywords internal
#'
#' @return geom that can be added to a ggplot to show regressions.
make.regression.line <- function(individual.rows, group, color, alpha=1, ...) {
  ggplot2::geom_line(
    data = {{individual.rows}},
    stat = "smooth",
    mapping = ggplot2::aes(group = {{group}}, color = {{color}}),
    formula = y ~ x,
    method = "glm",
    method.args = list(family = "binomial"),
    se = F,
    na.rm = T,
    size = 1,
    alpha = alpha,
    ...
  )
}


#' Create a geom for a box plot of success fractions.
#'
#' @param fraction.data Data frame with colum representing fraction of success.
#' @param x x-axis data variable, should be a predictor.
#' @param y y-axis data variable, fraction of successes in range \[0,1\].
#' @param group Data variable for groups and color.
#' @param fill.alpha Opacity of fill color for boxplot in range \[0,1\].
#' @param ... Additional arguments for underlying geom.
#'
#' @keywords internal
#'
#' @return A geom of a boxplot.
make.boxplot <- function(fraction.data, x, y, group, fill.alpha = 0.25, ...) {
  # Note these only make sense if the predictor (x-axis) can be organized
  # meaningfully into discrete units which will show up in the data as
  # many rows with the same value of predictor - think time steps.
  #
  # Two tricky things here:
  # (1) Grouping data correctly. We want a box per treatment per predictor
  #     step (e.g., in the common case where it represents time points),
  #     so we use interaction(predictor, treatment) to bind the group
  #     aesthetic to their combination.
  # (2) Controlling boxplot widths. If there are any "missing" group
  #     values (say, not all treatments were measured for a certain
  #     predictor step, e.g., in the common case where they are time points),
  #     then the remaining boxes will fill the full width that would be
  #     normally allotted to that bunch of boxes. Setting the position
  #     fixes this.
  ggplot2::geom_boxplot(
    data = fraction.data,
    mapping = ggplot2::aes(
      x = {{x}},
      y = {{y}},
      color = {{group}},
      fill = {{group}},
      group = interaction({{x}}, {{group}})),
    alpha = fill.alpha,
    position = ggplot2::position_dodge(preserve = "single"),
    ...
  )
}


#' Draw "T"-shaped intersection of predicted with probability.
#'
#' @param inverse.prediction.df Data frame of invers predictions.
#' @param predictor Data variable for which inverse prediction was created.
#' @param probability Probabily for which inverse prediction was run.
#' @param alpha Opacity in range \[0,1\].
#' @param digits Digits of inverse predicted values to show.
#' @param ... Additional arguments to geoms.
#'
#' @keywords internal
#'
#' @return A list of geoms.
make.inverse.prediction.geoms <- function(
  inverse.prediction.df, predictor, probability, alpha = 1, digits = 3, ...){
  list(
    # Draw horizontal line.
    ggplot2::geom_hline(yintercept = probability),

    # Lines from x-axis up to intersection, representing value of predictor
    # that produces given probability.
    ggplot2::geom_segment(
      mapping = ggplot2::aes(xend = {{predictor}}),
      data = inverse.prediction.df,
      y = 0,
      yend = probability,
      ...),

    # Label for the line. geom_label_repel is good at getting labels out
    # of the way, though it doesn't necessarily mean they don't look silly.
    ggrepel::geom_label_repel(
      mapping = ggplot2::aes(
        label = format({{predictor}}, digits = digits)),
      data = inverse.prediction.df,
      y = 0,
      alpha = alpha,
      show.legend = F,# Don't show a little letter "a" in legend.
      ...)
  )
}


make.point <- function(data, jitter.width, dodge.width, alpha=1, seed=114, ...) {
  # TODO make configurable - not everyone will need this.
  # In the common case that there are only several discrete "steps" along
  # the predictor axis that have been measured (e.g. observations taken
  # every day), we want some jitter so points don't overlap, but the height
  # is meaningful, so only jitter the x-values.
  # We also want dodge, otherwise it becomes hard to see any patterns in
  # the data with all treatments jumbled together.
  # We keep the jitter seed constant, so that users don't get different
  # plots every time they run this.
  ggplot2::geom_point(
    data = data,
    position = ggplot2::position_jitterdodge(jitter.width = jitter.width,
                                             jitter.height = 0,
                                             dodge.width = dodge.width,
                                             seed = seed),
    alpha = alpha,
    ...)
}
