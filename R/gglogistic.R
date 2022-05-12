#' Plot logistic regression of probability from 0% to 100% across a predictor.
#'
#' @param data Data with continuous predictor and success and failure columns.
#' @param predictor Continuous predictor for x-axis.
#' @param success.counts Column representing number of successes per observation.
#' @param failure.counts Column representing number of failures per observation.
#' @param treatment Treatment variable.
#' @param replicate Within-treatment replicates. NOT IMPLEMENTED YET.
#' @param probability.of.interest Calculate predictor value in each group that
#'        would yield this probability, or pass NULL to ignore.
#' @param xlabel Label for x-axis.
#' @param ylabel Label for y-axis.
#' @param treatment.label Label for treatment.
#' @param treatment.colors Vector mapping treatment levels to colors.
#' @param integer.x.breaks.by If specified, integer breaks in x-axis. Forces
#'        ggplot to break at integer values.
#' @param geoms Vector containing any or all of c("line", "point", "boxplot").
#'             "boxplot" only makes sense for data where the predictor is
#'             conceptually continuous, but was only sampled at discrete steps.
#'             For instance, if the predictor is time, and samples were only
#'             taken once per day, a boxplot might be useful.
#' @param fill.alpha Opacity in range \[0,1\] for fill of boxplots. Boxplots can
#'                   be hard to see if there is a lot of data, and having a
#'                   light fill color helps to distinguish them.
#' @param jitter.width If specified, amount to jitter points in x-axis. Useful
#'                     if predictor was sampled at discrete steps that would
#'                     otherwise cause points to be hard to tell apart.
#' @param dodge.width If specified, amount to dodge replicate boxplots or groups
#'                    of points by.
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' logisticat::metamorphosis() %>%
#'   logisticat::gglogistic(
#'     predictor = days,
#'     success.counts = metamorphosed,
#'     failure.counts = did.not.metamorphose,
#'     treatment = ration,
#'     replicate = beaker,
#'     probability.of.interest = 0.5,
#'     xlabel = "Age (days)",
#'     ylabel = "Metamorphic Competence",
#'     treatment.label = "Algal Ration",
#'     treatment.colors = c("high" = "#f2af09","low" = "#ea1578"),
#'     geoms = c("line", "boxplot"))
gglogistic <- function(
  data,
  predictor,
  success.counts,
  failure.counts,
  treatment,
  replicate, # TODO implement
  probability.of.interest = 0.5,
  xlabel                  = NULL,
  ylabel                  = NULL,
  treatment.label         = NULL,
  treatment.colors        = NULL,
  integer.x.breaks.by     = NULL,
  geoms                   = "line",
  fill.alpha              = 0.25,
  jitter.width            = 0,
  dodge.width             = 0) {

  row.data <- data %>% outcome.counts.to.rows({{success.counts}},
                                              {{failure.counts}},
                                              outcome)

  p <- ggplot2::ggplot(row.data,
                       mapping = ggplot2::aes(x = {{predictor}},
                                              y = outcome,
                                              color = {{treatment}})
  )

  want.line = F
  want.point = F
  want.boxplot = F
  if (!is.null(geoms)) {
    handled.geoms = 0
    if ("line" %in% geoms) {
      want.line = T
      handled.geoms = handled.geoms + 1
    }
    if ("point" %in% geoms) {
      want.point = T
      handled.geoms = handled.geoms + 1
    }
    if ("boxplot" %in% geoms) {
      want.boxplot = T
      handled.geoms = handled.geoms + 1
    }

    if (length(geoms) != handled.geoms) {
      warning(glue::glue("Don't know how to draw {length(geoms)-handled.geoms} unknown options in geoms argument."))
    }
    if (handled.geoms == 0){
      warning(glue::glue("No geoms that can be drawn; plot will be empty."))
    }
  }


  # ============================================================================
  # TRADITIONAL GEOMS
  # ============================================================================

  if (want.line) {
    p <- p + ggplot2::geom_line(
      data = row.data,
      stat = "smooth",
      mapping = ggplot2::aes(group = {{treatment}}),
      formula = y ~ x,
      method = "glm",
      method.args = list(family = "binomial"),
      se = F,
      na.rm = T,
      size = 1,
      alpha = 1
    )
  }

  if (want.point | want.boxplot) {
    # Calculate fraction competent each day and put it on y-axis.
    replicate.fractions = data %>%
      dplyr::mutate(outcome = {{success.counts}}/({{success.counts}}+{{failure.counts}}))

    if (want.point) {
      # TODO make configurable - not everyone will need this.
      # In the common case that there are only several discrete "steps" along
      # the predictor axis that have been measured (e.g. observations taken
      # every day), we want some jitter so points don't overlap, but the height
      # is meaningful, so only jitter the x-values.
      # We also want dodge, otherwise it becomes hard to see any patterns in
      # the data with all treatments jumbled together.
      p = p +
        ggplot2::geom_point(
          data = replicate.fractions,
          position = ggplot2::position_jitterdodge(jitter.width = jitter.width,
                                                   jitter.height = 0,
                                                   dodge.width = dodge.width,
                                                   seed = 114)
        )
    }

    if (want.boxplot)
    {
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
      p = p +
        ggplot2::geom_boxplot(
          data = replicate.fractions,
          mapping = ggplot2::aes(
            x = {{predictor}},
            y = outcome,
            color = {{treatment}},
            fill = {{treatment}},
            group = interaction({{predictor}}, {{treatment}})),
          #width = boxplot.width, # TODO width of actual BOX, not dodge.
          position = ggplot2::position_dodge(preserve = "single")
        )
    }
  }


  # ============================================================================
  # INVERSE PREDICTION
  # ============================================================================

  want.inverse.prediction <- !is.null(probability.of.interest)
  if (want.inverse.prediction){
    if (!is.numeric(probability.of.interest) |
        probability.of.interest <= 0 |
        probability.of.interest >= 1 ){
      warning(glue::glue("probability.of.interest should be a number in range (0,1), but was: {probability.of.interest}"))
      want.inverse.prediction <- F
    }
  }

  if (want.inverse.prediction) {

    # Draw horizontal line.
    p <- p + ggplot2::geom_hline(yintercept = probability.of.interest)


    # Need to make more like tc50.by.beaker to support rep + treatment
    # maybe extract ALL this in a function, which could be applied to rep + treatment separatelY?
    treatment.names <-
      row.data %>%
      ###################### Begin "tc50.by.beaker"
      dplyr::select({{treatment}}) %>%
      unique() %>%
      dplyr::pull(1) # We just want the first column, but don't know its name.

    rows.by.treatment <- function(name) {
      row.data %>%
        dplyr::filter({{treatment}} == name)
    }

    # Make regression for each level in treatment
    regressions.by.treatment <-
      lapply(treatment.names,
             function(treatment.name) {
               logisticat::logistic.regression(
                 treatment.name %>% rows.by.treatment,
                 predictor = {{predictor}},
                 outcome = outcome)
             })

    # Calculate intersection for each regression we just made.
    treatment.inverse.predictions <-
      lapply(regressions.by.treatment,
             # Anonymous function because I never learned how else to
             # pass two arguments with lapply().
             function(regression.for.a.treatment) {
               logisticat::inverse.predict(regression.for.a.treatment,
                                           probability.of.interest)
             }) %>%
      unlist() # ggplot hates lists.

    # Populate a new data frame with inverse predictions and <here is whre we would map back in treatment to get right color>
    inverse.prediction.df <-
      tibble::tibble(
        {{predictor}} := treatment.inverse.predictions,
        {{treatment}} := treatment.names
        # treatment would go here
      )
    ###################### END "tc50.by.beaker"

    # Draw ascenders.
    p <- p +
      ggplot2::geom_segment(
        mapping = ggplot2::aes(xend = {{predictor}}),
        data = inverse.prediction.df,
        y = 0,
        yend = probability.of.interest
      )

    # Label the ascenders.
    p <- p +
      ggrepel::geom_label_repel(
        mapping = ggplot2::aes(
          label = format({{predictor}}, digits = 3)), # TODO make configurable
        data = inverse.prediction.df,
        y = 0,
        show.legend = F # Don't show a little letter "a" in legend.
      )
  }


  # ============================================================================
  # AESTHETICS
  # ============================================================================

  if (!is.null(treatment.colors)){
    # Extract the vector of level values from the factor in the treatment
    # column so that we can pass this same vector as the `breaks` argument
    # of the scale_*_manual() calls. This is required in order to force
    # ggplot to list the levels in the legend in the same order that it is
    # arranging them on the plot - e.g., if we don't do this, then even if it
    # always puts treatments in the order A, B, C on the plot itself, the legend
    # will frequently be ordered C, B, A, or something silly.
    treatment.levels <-
      levels(data %>% dplyr::select({{treatment}}) %>% dplyr::pull(1))

    p <- p +
      ggplot2::scale_color_manual(values = treatment.colors,
                                  breaks = treatment.levels)

    # Use the same colors for fill, only needed for boxplots. When there's a
    # lot of data, they are hard to see when they don't have a slight fill.
    p <- p +
      ggplot2::scale_fill_manual(values = with.alpha(treatment.colors, fill.alpha),
                                 breaks = treatment.levels)
  }

  # Labels and axes.
  if (!is.null(xlabel)){
    p <- p + ggplot2::xlab(xlabel)
  }

  if (!is.null(ylabel)){
    p <- p + ggplot2::ylab(ylabel)
  }

  if (!is.null(treatment.label)) {
    p <- p + ggplot2::labs(color = treatment.label, fill = treatment.label)
  }


  # ============================================================================
  # AXIS EXTENT AND DIVISIONS
  # ============================================================================

  # Force x-axis to label at integers if caller desires.
  # Based on Nat's answer here: https://stackoverflow.com/a/54678868/4104189
  if (!is.null(integer.x.breaks.by)) {
    if (!is.numeric(integer.x.breaks.by) |
        integer.x.breaks.by < 1 |
        !is.finite(integer.x.breaks.by)){
      warning(glue::glue("integer.x.breaks.by should be a positive integer, but was: {integer.x.breaks.by}"))
    } else {
      p <- p +
        ggplot2::scale_x_continuous(breaks =
                                      function(x) seq(ceiling(x[1]),
                                                      floor(x[2]),
                                                      by = (ceiling(integer.x.breaks.by))))
    }
  }

  p <- p +
    # Force y-axis to stop at 0. If we don't do this, then ascender
    # will just awkwardly stop a bit above the x-axis.
    ggplot2::scale_y_continuous(labels = scales::percent_format(),
                                expand = ggplot2::expansion(mult = c(0, 0))) +
    ggplot2::coord_cartesian(ylim = c(0,1)) +


  # ============================================================================
  # THEME
  # ============================================================================
  ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position  = "bottom",
      legend.key       = ggplot2::element_rect(fill = NA),
      plot.background  = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      axis.line        = ggplot2::element_line(),
      text             = ggplot2::element_text(size = 12)
    )

  p
}
