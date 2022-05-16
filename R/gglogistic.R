#' Plot logistic regression of probability from 0% to 100% across a predictor.
#'
#' @param data Data with continuous predictor and success and failure columns.
#' @param predictor Continuous predictor for x-axis.
#' @param success.counts Column representing number of successes per observation.
#' @param failure.counts Column representing number of failures per observation.
#' @param treatment Treatment variable.
#' @param replicate Within-treatment replicates.
#' @param line.var Variable to plot regression curves for: "both" (default),
#'                 "treatment", "replicate", or "none".
#' @param point.var Variable to plot points for: "both", "treatment", "replicate",
#'                  or "none" (default). y-value represents fraction of
#'                  successes in each treatment or replicate.
#' @param boxplot.var Variable to plot boxplots for: "both", "treatment",
#'                    "replicate", or "none" (default).
#' @param inverse.var Variable to draw lines at inverse predictions for:
#'                    "both", "treatment", "replicate", "none" or "line"
#'                    (same as whatever was specified for line.var; default).
#' @param probability.of.interest Calculate predictor value in each group that
#'        would yield this probability.
#' @param xlabel Label for x-axis.
#' @param ylabel Label for y-axis.
#' @param treatment.label Label for treatment.
#' @param treatment.colors Vector mapping treatment levels to colors.
#' @param integer.x.breaks.by If specified, integer breaks in x-axis. Forces
#'        ggplot to break at integer values.
#' @param fill.alpha Opacity in range \[0,1\] for fill of boxplots. Boxplots can
#'                   be hard to see if there is a lot of data, and having a
#'                   light fill color helps to distinguish them.
#' @param jitter.width If specified, amount to jitter points in x-axis. Useful
#'                     if predictor was sampled at discrete steps that would
#'                     otherwise cause points to be hard to tell apart.
#' @param dodge.width If specified, amount to dodge replicate boxplots or groups
#'                    of points by.
#' @param replicate.alpha Opacity in range \[0,1\] to draw `replicate`
#'                        geoms at when the same geom is also drawn for
#'                        `treatment`. Helps distinguish `treatment` from
#'                        `replicate` values.
#' @param inverse.predict.digits Number of digits to print for inverse predicted values.
#'
#' @return A ggplot object.
#' @export
#' @examples
#' metamorphosis.data <- logisticat::metamorphosis()
#' head(metamorphosis.data) # Has columns for success and failure counts
#' logisticat::gglogistic(metamorphosis.data,
#'                        predictor=days,
#'                        success.counts=metamorphosed,
#'                        failure.counts=did.not.metamorphose,
#'                        treatment=ration,
#'                        replicate=beaker,
#'                        probability.of.interest=0.5)
gglogistic <- function(
  # Data Frame and Columns, plus what to draw for them.
  data,
  predictor,
  success.counts,
  failure.counts,
  treatment,
  replicate               = NULL,
  line.var                = "both",
  point.var               = "none",
  boxplot.var             = "none",
  inverse.var             = "line", # Will be interpreted as line.var if missing.
  probability.of.interest = 0.5,

  # Describing data.
  xlabel                  = NULL,
  ylabel                  = NULL,
  treatment.label         = NULL,
  treatment.colors        = NULL,

  # Tweak plot to make it look better.
  integer.x.breaks.by     = NULL,
  fill.alpha              = 0.25,
  replicate.alpha         = 0.5,
  jitter.width            = 0,
  dodge.width             = 0,
  inverse.predict.digits  = 3) {

  # TODO TODO TODO: all of this has assumed replicates are levels that aren't
  # shared across treatments (eg, they don't all have "A", "B", "C")
  # we probably actually need some combination of retaining both columns
  # and of looking at interaction() to get the right results.

  # ============================================================================
  # INTERPRETING ARGUMENTS
  # ============================================================================

  # This section just figures out WHAT the caller asked us to plot and IF
  # it is possible, trying to report problems in a helpful way.

  # Allow columns to be specified as variable names or as strings.
  illegal.null <- F
  tryCatch(
    {
      predictor <- as.name(substitute(predictor))
      predictor <- rlang::ensym(predictor)
      success.counts <- as.name(substitute(success.counts))
      success.counts <- rlang::ensym(success.counts)
      failure.counts <- as.name(substitute(failure.counts))
      failure.counts <- rlang::ensym(failure.counts)
      treatment <- as.name(substitute(treatment))
      treatment <- rlang::ensym(treatment)
    },
    error = function(cond){
      if (cond$message == "invalid type/length (symbol/0) in vector allocation")
      {
        # An actual case for the <<- operator! Better to not call stop() in
        # here or error message is attributed to wrong statement, and <-
        # does not suffice to assign out of this scope.
        illegal.null <<- T
      }
    }
  )
  if (illegal.null) {
    stop(glue::glue("One or more of predictor, success.counts, failure.counts, or treatment were NULL"))
  }

  #data <- data %>% dplyr::mutate(treatment = as.factor(treatment))

  if (!missing(replicate)) { # This one can be null, the rest SHOULD throw error
    replicate <- as.name(substitute(replicate))
    replicate <- rlang::ensym(replicate)

    #data <- data %>% dplyr::mutate(replicate = as.factor(replicate))
  }

  # For each geom we can draw, figure out which of the following was specified:
  # - "none":         Don't draw it.
  # - "treatment":    Display treatment.
  # - "replicate":    Display replicate.
  # - "both":         Display treatment and replicate.
  # - "line":         Do whatever we are doing for line.var. Only applies to
  #                   inverse.var
  #
  # When displaying the treatment, we pool data from all replicates, rather than
  # doing something more sophisticated like taking the means.
  illegal.var <- function(var.name, value, extra.opts = c()){
    opts = c("none", "treatment", "replicate", "both", extra.opts)
    warning(
      paste0("Invalid value for parameter ", var.name, ": ", value,
             "Must be one of: ", paste(opts, collapse = ", ")),
      call. = F)
  }

  switch(line.var %??% "none",
         "none"      = { line.treatment <- F; line.replicate <- F},
         "treatment" = { line.treatment <- T; line.replicate <- F},
         "replicate" = { line.treatment <- F; line.replicate <- T},
         "both"      = { line.treatment <- T; line.replicate <- T},
         {illegal.var("line.var", line.var)})

  switch(point.var %??% "none",
         "none"      = { point.treatment <- F; point.replicate <- F},
         "treatment" = { point.treatment <- T; point.replicate <- F},
         "replicate" = { point.treatment <- F; point.replicate <- T},
         "both"      = { point.treatment <- T; point.replicate <- T},
         {illegal.var("point.var", point.var)})

  switch(boxplot.var %??% "none",
         "none"      = { boxplot.treatment <- F; boxplot.replicate <- F},
         "treatment" = { boxplot.treatment <- T; boxplot.replicate <- F},
         "replicate" = { boxplot.treatment <- F; boxplot.replicate <- T},
         "both"      = { boxplot.treatment <- T; boxplot.replicate <- T},
         {illegal.var("boxplot.var", boxplot.var)})

  switch(inverse.var %??% "none",
         "none"      = { inverse.treatment <- F; inverse.replicate <- F},
         "treatment" = { inverse.treatment <- T; inverse.replicate <- F},
         "replicate" = { inverse.treatment <- F; inverse.replicate <- T},
         "both"      = { inverse.treatment <- T; inverse.replicate <- T},
         "line"      = {
           inverse.treatment <- line.treatment
           inverse.replicate <- line.replicate},
         {illegal.var("inverse.var", inverse.var, "line")})

  # We'll only draw lines for inverse prediction if we have a probability that's
  # valid. We need a regression to do this, even if it isn't plotted.
  if (inverse.treatment || inverse.replicate) {
    if (is.null(probability.of.interest)) {
      warning(glue::glue("Don't know what to plot for inverse prediction; probability.of.interest was NULL"))
      inverse.treatment <- F
      inverse.replicate <- F
    } else if (!is.numeric(probability.of.interest) ||
                probability.of.interest <= 0 ||
                probability.of.interest >= 1 ){
      warning(glue::glue("probability.of.interest should be a number in range (0,1), but was: {probability.of.interest}"))
      inverse.treatment <- F
      inverse.replicate <- F
    }
  }

  # Detect if caller didn't give us anything to draw.
  if (!(line.treatment    || line.replicate  ||
        point.treatment   || point.replicate ||
        boxplot.treatment || boxplot.replicate ||
        inverse.treatment || inverse.replicate)) {
    warning(glue::glue("No valid geoms specificed; use at least one of line.var, point.var, boxplot.var, inverse.var or else your plot will be very boring."))
  }

  # If we are plotting the replicate data, make sure it is valid.
  if (line.replicate || point.replicate || boxplot.replicate || inverse.replicate) {
    if (is.null(replicate)){ # Unreachable?
      warning(glue::glue("replicate cannot be plotted because it is NULL."))
      line.replicate    <- F
      point.replicate   <- F
      boxplot.replicate <- F
      inverse.replicate <- F
    } else if(treatment == replicate) {
      warning(glue::glue("treatment and replicate should not be the same variable ({rlang::as_name(treatment)})."))
      line.replicate    <- F
      point.replicate   <- F
      boxplot.replicate <- F
      inverse.replicate <- F
    }
  }


  # Shorthand for more complicated logic.
  plot.treatment <- line.treatment || point.treatment || boxplot.treatment || inverse.treatment
  plot.replicate <- line.replicate || point.replicate || boxplot.replicate || inverse.replicate
  plot.line      <- line.treatment    || line.replicate
  plot.point     <- point.treatment   || point.replicate
  plot.boxplot   <- boxplot.treatment || boxplot.replicate
  plot.inverse   <- inverse.treatment || inverse.replicate
  need.regressions <- plot.line  || plot.inverse
  need.fractions   <- plot.point || plot.boxplot


  # ============================================================================
  # PREPARING DATA FRAMES FOR INTERNAL USE
  # ============================================================================

  # TODO: Decide heuristically whether predictor looks "discrete-ish" enough
  # to dodge values.

  # This prevents R check notes for undefined globals, but you don't actually
  # need to define this variable first.
  outcome = rlang::sym("outcome")

  if (need.regressions) {
    individual.rows <- data %>% outcome.counts.to.rows({{success.counts}},
                                                       {{failure.counts}},
                                                       outcome)

    # TODO could have two different group_by versions up here, so grouping
    # logic below can be simplified.
  }

  if (need.fractions) {
    # Replace the success and failure counts with a column containing the
    # fraction of successes out of total tests per row.
    # This is pulled out into this closure so we can pass in either data with
    # separate rows per replicate, or data with matching rows combined by
    # treatment depending on what we do to the data frame we pass in as `d`.
    success.fractions <- function(d) {
      d %>%
        dplyr::mutate(outcome = {{success.counts}}/({{success.counts}}+{{failure.counts}}))
    }

    if (plot.treatment) {
      # Fractions calculated after summing successes and failures across
      # replicates.
      treatment.fractions <-
        data %>%
        dplyr::group_by({{predictor}}, {{treatment}}) %>%
        dplyr::summarise({{success.counts}} := sum({{success.counts}}),
                         {{failure.counts}} := sum({{failure.counts}})) %>%
        success.fractions()
    }

    if (plot.replicate) {
      # Fractions calculated with replicates still separate.
      replicate.fractions <-
        data %>%
        dplyr::group_by({{predictor}}, {{treatment}}, {{replicate}}) %>%
        dplyr::summarise({{success.counts}} := sum({{success.counts}}),
                         {{failure.counts}} := sum({{failure.counts}})) %>%
        success.fractions()
    }
  }


  # ============================================================================
  # CREATE EMPTY PLOT WITH JUST AES SO WE CAN ACCRETE STUFF ONTO IT
  # ============================================================================

  p <- ggplot2::ggplot(mapping = ggplot2::aes(x = {{predictor}},
                                              y = outcome,
                                              color = {{treatment}}))


  # ============================================================================
  # TRADITIONAL GEOMS + OUR "INVERSE PREDICTION INTERSECTION" "GEOM"
  # ============================================================================

  effective.replicate.alpha <- function(treatment.plotted){
    if (treatment.plotted){
      # Make replicate geoms a bit transparent so they are easy to distinguish
      # from, and don't hide, the treatment geoms.
      # Caller can prevent this behavior by simply passing 1 for fill.alpha.
      return(replicate.alpha)
    } else {
      # No point making them transparent if there's nothing else.
      return(1)
    }
  }

  if (plot.line) {
    if(line.treatment) {
      p <- p + make.regression.line(individual.rows = individual.rows,
                                    group = {{treatment}},
                                    color = {{treatment}})
    }

    if(line.replicate){
      p <- p + make.regression.line(individual.rows = individual.rows,
                                    group = interaction({{treatment}}, {{replicate}}),
                                    color = {{treatment}},
                         alpha = effective.replicate.alpha(line.treatment))
    }
  }

  if (plot.point) {
    # This geom won't have the semantics we want unless we pass in data frames
    # we've prepared specially for treatment vs replicate. Treatment points
    # are fractions from all pooled data per predictor value, not just every
    # fraction from every observation separately, which is what would happen
    # for treatment by default.

    if (point.treatment) {
      p <- p + make.point(treatment.fractions,
                          jitter.width = jitter.width,
                          dodge.width = dodge.width)
    }

    if (point.replicate) {
      p <- p + make.point(replicate.fractions,
                          jitter.width = jitter.width,
                          dodge.width = dodge.width,
                          alpha=effective.replicate.alpha(point.treatment))
    }
  }

  if (plot.boxplot) {
    if (boxplot.treatment) {
      p <- p + make.boxplot(treatment.fractions, x={{predictor}}, y=outcome,
                            group={{treatment}}, fill.alpha = fill.alpha)
    }

    if (boxplot.replicate) {
      p <- p + make.boxplot(replicate.fractions, x={{predictor}}, y=outcome,
                            group={{treatment}}, fill.alpha = fill.alpha)
    }
  }

  if (plot.inverse) {
    if (inverse.treatment){
      inverse.prediction.df <- inverse.predict.by.treatment(
        data = individual.rows,
        predictor = {{predictor}},
        outcome = {{outcome}},
        treatment = {{treatment}},
        probability = probability.of.interest)

      p <- p + make.inverse.prediction.geoms(
        inverse.prediction.df = inverse.prediction.df,
        predictor = {{predictor}},
        probability = probability.of.interest,
        alpha = 1,
        digits = inverse.predict.digits
      )
    }

    if(inverse.replicate) {
      inverse.prediction.df <- inverse.predict.by.replicate(
        data = individual.rows,
        predictor = {{predictor}},
        outcome = {{outcome}},
        treatment = {{treatment}},
        replicate = {{replicate}},
        probability = probability.of.interest)

      p <- p + make.inverse.prediction.geoms(
        inverse.prediction.df = inverse.prediction.df,
        predictor = {{predictor}},
        probability = probability.of.interest,
        alpha = effective.replicate.alpha(inverse.treatment),
        digits = inverse.predict.digits
      )
    }
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
    # TODO: how should we handle separate alpha for treatment/replicate?
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
    if (!is.numeric(integer.x.breaks.by) ||
        integer.x.breaks.by < 1 ||
        !is.finite(integer.x.breaks.by)){
      warning(glue::glue("integer.x.breaks.by should be a positive integer, but was: {integer.x.breaks.by}"))
    } else {
      p <- p +
        ggplot2::scale_x_continuous(
          breaks = function(x) seq(ceiling(x[1]),
                                   floor(x[2]),
                                   by = (ceiling(integer.x.breaks.by))))
    }
  }

  # Force y-axis to stop at 0. If we don't do this, then ascender
  # will just awkwardly stop a bit above the x-axis.
  p <- p +
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
