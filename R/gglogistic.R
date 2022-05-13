#' Plot logistic regression of probability from 0% to 100% across a predictor.
#'
#' @param data Data with continuous predictor and success and failure columns.
#' @param predictor Continuous predictor for x-axis.
#' @param success.counts Column representing number of successes per observation.
#' @param failure.counts Column representing number of failures per observation.
#' @param treatment Treatment variable.
#' @param replicate Within-treatment replicates.
#' @param line.var Variable to plot regression curves for. Must be the same as
#'                 `treatment` or `replicate`, or the character vector "both"
#'                 to plot both. Pass NULL to hide. Defaults to same variable
#'                 as `treatment`. y-axis represents predicted probability ad
#'                 each value of predictor.
#' @param point.var Variable to represent with points. Must be the same as
#'                 `treatment` or `replicate`, or the character vector "both"
#'                 to plot both. Pass NULL to hide. Defaults to NULL. y-axis
#'                 value represents fraction of successes in each observation
#'                 (for `replicate`) or across all observations with the
#'                 same treatment and same predictor (for `treatment`).
#' @param boxplot.var Variable to represent with points. Must be the same as
#'                 `treatment` or `replicate`, or the character vector "both"
#'                 to plot both. Pass NULL to hide. Defaults to NULL. y-axis
#'                 has same meaning as for `point.var`.
#' @param inverse.var Variable to draw lines representing inverse predictions
#'                    for. Must be the same as `treatment` or `replicate`, or
#'                    the character vector "both" to plot both. Pass NULL to
#'                    hide. Defaults to same variable as `line.var`.
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
#'
#' @return A ggplot object.
#' @export
gglogistic <- function(
  # Data Frame and Columns, plus what to draw for them.
  data,
  predictor,
  success.counts,
  failure.counts,
  treatment,
  replicate,
  line.var                = "both",
  point.var               = NULL,
  boxplot.var             = NULL,
  inverse.var             = NULL, # Will be interpreted as line.var if missing.
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
  dodge.width             = 0) {

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

  if (!missing(replicate)) { # This one can be null, the rest SHOULD throw error
    replicate <- as.name(substitute(replicate))
    replicate <- rlang::ensym(replicate)
  }

  # For each geom we can draw, figure out which of the following was specified:
  # - NULL:           Don't draw it.
  # - Variable name:  Display treatment if it matches treatment, display
  #                    replicate if it matches replicate, else error.
  # - String:         If it is the string "both", display both treatment and
  #                    replicate, if it is the same as the treatment or
  #                    replicate, display that, else error.
  #
  # When displaying the treatment, we pool data from all replicates, rather than
  # doing something more sophisticated like taking the means.
  #
  # Note: to test if an argument was a string or variable name, we first need
  # to substitute() it or typeof() will cause an object not found error for
  # variable names.
  line.treatment <- F
  line.replicate <- F
  if (!is.null(line.var))
  {
    if (typeof(substitute(line.var)) == "character"){
      # Safe to test against strings inside here.
      if(line.var == "both"){
        # Plot both. Note if the caller named a variable "both" they'll get
        # treatment and replicate both plotted instead of their `both` variable.
        line.treatment <- T
        line.replicate <- T
      } else if (line.var == rlang::as_name(treatment)){
        # Plot treatment.
        line.treatment <- T
      } else if (line.var == rlang::as_name(replicate)){
        # Plot replicate.
        line.replicate <- T
      } else {
        warning(glue::glue("unknown variable name for line.var: {line.var}. Must be either same name as treatment or replicate, 'both', or NULL."))
      }
    } else if (typeof(substitute(line.var)) == "symbol") {
      if (rlang::as_name(substitute(line.var)) == rlang::as_name(treatment)){
        line.treatment <- T
      } else if (rlang::as_name(substitute(line.var)) == rlang::as_name(replicate)) {
        line.replicate <- T
      } else {
        warning(glue::glue("unknown variable name for line.var: {rlang::as_name(substitute(line.var))}. Must be either same name as treatment or replicate, 'both', or NULL."))
      }
    } else {
      warning(glue::glue("line.var must be character vector or variable name."))
    }
  }

  # Same logic for the other geoms as we just did above.
  point.treatment <- F
  point.replicate <- F
  if (!is.null(point.var)){
    if (typeof(substitute(point.var)) == "character"){
      # Safe to test against strings inside here.
      if(point.var == "both"){
        # Plot both. Note if the caller named a variable "both" they'll get
        # treatment and replicate both plotted instead of their `both` variable.
        point.treatment <- T
        point.replicate <- T
      } else if (point.var == rlang::as_name(treatment)){
        # Plot treatment.
        point.treatment <- T
      } else if (point.var == rlang::as_name(replicate)){
        # Plot replicate.
        point.replicate <- T
      } else {
        warning(glue::glue("unknown variable name for point.var: {point.var}. Must be either same name as treatment or replicate, 'both', or NULL."))
      }
    } else if (typeof(substitute(point.var)) == "symbol") {
      if (rlang::as_name(substitute(point.var)) == rlang::as_name(treatment)){
        point.treatment <- T
      } else if (rlang::as_name(substitute(point.var)) == rlang::as_name(replicate)) {
        point.replicate <- T
      } else {
        warning(glue::glue("unknown variable name for point.var: {rlang::as_name(substitute(point.var))}. Must be either same name as treatment or replicate, 'both', or NULL."))
      }
    } else {
      warning(glue::glue("point.var must be character vector or variable name."))
    }
  }


  boxplot.treatment <- F
  boxplot.replicate <- F
  if (!is.null(boxplot.var)){
    if (typeof(substitute(boxplot.var)) == "character"){
      # Safe to test against strings inside here.
      if(boxplot.var == "both"){
        # Plot both. Note if the caller named a variable "both" they'll get
        # treatment and replicate both plotted instead of their `both` variable.
        boxplot.treatment <- T
        boxplot.replicate <- T
      } else if (boxplot.var == rlang::as_name(treatment)){
        # Plot treatment.
        boxplot.treatment <- T
      } else if (boxplot.var == rlang::as_name(replicate)){
        # Plot replicate.
        boxplot.replicate <- T
      } else {
        warning(glue::glue("unknown variable name for boxplot.var: {boxplot.var}. Must be either same name as treatment or replicate, 'both', or NULL."))
      }
    } else if (typeof(substitute(boxplot.var)) == "symbol") {
      if (rlang::as_name(substitute(boxplot.var)) == rlang::as_name(treatment)){
        boxplot.treatment <- T
      } else if (rlang::as_name(substitute(boxplot.var)) == rlang::as_name(replicate)) {
        boxplot.replicate <- T
      } else {
        warning(glue::glue("unknown variable name for boxplot.var: {rlang::as_name(substitute(boxplot.var))}. Must be either same name as treatment or replicate, 'both', or NULL."))
      }
    } else {
      warning(glue::glue("boxplot.var must be character vector or variable name."))
    }
  }


  # The other thing we can draw is lines showing where each treatment or
  # replicate reached <probability.of.interest> on the y-axis. This isn't a
  # "legit" geom but just something bespoke we're assembling.
  inverse.treatment <- F
  inverse.replicate <- F
  if (is.null(inverse.var)){
    # If it is NULL, it could either be NULL because the caller explicitly
    # passed NULL as an argument, which we interpret as "don't plot it", or
    # because NULL is the default value, which we interpret as "plot it,
    # specifically, plot whatever variable is in `line.var`.
    # Despite what I've read, `missing()` does NOT have the right semantics for
    # this. `missing()` returns TRUE for arguments that have had NULL passed
    # EXPLICITLY. We have to inspect R representation of the actual call itself,
    # and even then we'll get the wrong answer without names()
    inverse.var.is.missing <-
      !("inverse.var" %in% names(as.list(match.call(expand.dots=FALSE))))
    if (inverse.var.is.missing){
      inverse.treatment <- line.treatment
      inverse.replicate <- line.replicate
    } else {
      # Do nothing, already F
    }
  } else {
    if (typeof(substitute(inverse.var)) == "character"){
      # Safe to test against strings inside here.
      if(inverse.var == "both"){
        # Plot both. Note if the caller named a variable "both" they'll get
        # treatment and replicate both plotted instead of their `both` variable.
        inverse.treatment <- T
        inverse.replicate <- T
      } else if (inverse.var == rlang::as_name(treatment)){
        # Plot treatment.
        inverse.treatment <- T
      } else if (inverse.var == rlang::as_name(replicate)){
        # Plot replicate.
        inverse.replicate <- T
      } else {
        warning(glue::glue("unknown variable name for inverse.var: {inverse.var}. Must be either same name as treatment or replicate, 'both', or NULL."))
      }
    } else if (typeof(substitute(inverse.var)) == "symbol") {
      if (rlang::as_name(substitute(inverse.var)) == rlang::as_name(treatment)){
        inverse.treatment <- T
      } else if (rlang::as_name(substitute(inverse.var)) == rlang::as_name(replicate)) {
        inverse.replicate <- T
      } else {
        warning(glue::glue("unknown variable name for inverse.var: {rlang::as_name(substitute(inverse.var))}. Must be either same name as treatment or replicate, 'both', or NULL."))
      }
    } else {
      warning(glue::glue("inverse.var must be character vector or variable name."))
    }
  }


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

  if (plot.line) {
    # Closure because almost everything is the same no matter which we plot.
    # We just need to choose the group and glm will figure out the rest.
    # We also vary alpha if both variables are being plotted.
    make.line <- function(group.var, alpha) {
      ggplot2::geom_line(
        # For this geom, the same data frame works for both treatment
        # and for replicate.
        data = individual.rows,
        stat = "smooth",
        mapping = ggplot2::aes(group = {{group.var}}),
        formula = y ~ x,
        method = "glm",
        method.args = list(family = "binomial"),
        se = F,
        na.rm = T,
        size = 1,
        alpha = alpha
      )
    }

    if(line.treatment) {
      p <- p + make.line({{treatment}}, 1)
    }

    if(line.replicate){
      if (line.treatment){
        # Make replicate lines a bit transparent so they are easy to distinguish
        # from and don't hide the treatment lines.
        effictive.replicate.alpha <- replicate.alpha
      } else {
        # No point making them transparent if there's nothing else.
        # TODO: Or maybe caller has other geoms for treatment?
        effictive.replicate.alpha <- 1
      }

      p <- p + make.line({{replicate}}, effictive.replicate.alpha)
    }
  }

  if (plot.point) {
    # This geom won't have the semantics we want unless we pass in data frames
    # we've prepared specially for treatment vs replicate. Treatment points
    # are fractions from all pooled data per predictor value, not just every
    # fraction from every observation separately, which is what would happen
    # for treatment by default.
    make.point <- function(d) {
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
        data = d,
        position = ggplot2::position_jitterdodge(jitter.width = jitter.width,
                                                 jitter.height = 0,
                                                 dodge.width = dodge.width,
                                                 seed = 114))
    }

    if (point.treatment) {
      p <- p + make.point(treatment.fractions)
    }

    if (point.replicate) {
      p <- p + make.point(replicate.fractions)
    }
  }

  if (plot.boxplot) {
    make.boxplot <- function(d) {
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
        data = d,
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

    if (boxplot.treatment) {
      p <- p + make.boxplot(treatment.fractions)
    }

    if (boxplot.replicate) {
      p <- p + make.boxplot(replicate.fractions)
    }
  }

  if (plot.inverse) {
    # Draw horizontal line.
    p <- p + ggplot2::geom_hline(yintercept = probability.of.interest)


    make.inverse <- function(d, group.var, alpha) {
      # TODO group_by/interaction for replicate?
      group.names <-
        d %>%
        ## TODO: see tc50.by.beaker
        dplyr::select({{group.var}}) %>%
        unique() %>% # TODO: does order wrt/pull() matter?
        dplyr::pull(1) # Get first (only) column as vector.

      get.rows.matching.group.name <- function(group.name) {
        d %>% dplyr::filter({{group.var}} == group.name)
      }

      # Make regression for each level in group.var.
      regressions.by.group <-
        lapply(group.names,
               function(group.name){
                 logisticat::logistic.regression(
                   group.name %>% get.rows.matching.group.name,
                   predictor = {{predictor}},
                   outcome = outcome)})

      # Calculate intersection for each regression we just made.
      group.inverse.predictions <-
        lapply(regressions.by.group,
               # Anonymous function because I never learned how else to
               # pass two arguments with lapply().
               function(regression.for.a.group) {
                 logisticat::inverse.predict(regression.for.a.group,
                                             probability.of.interest)}) %>%
        unlist() # ggplot hates lists in data frames.

      # Populate a new data frame with inverse predictions and <here is whre we would map back in treatment to get right color>
      inverse.prediction.df <-
        tibble::tibble(
          {{predictor}} := group.inverse.predictions,
          {{treatment}} := group.names
          # TODO: replicate?
        )
      ###################### END "tc50.by.beaker"

      # Return these two geoms as a list because you can't `+` ggproto objects
      # together unless you have already `+`ed them to a ggplot.
      list(
        # Lines from x-axis up to intersection, representing value of predictor
        # that produces given probability.
        ggplot2::geom_segment(
        mapping = ggplot2::aes(xend = {{predictor}}),
        data = inverse.prediction.df,
        y = 0,
        yend = probability.of.interest,
        alpha = 1),

      # Label for the line. geom_label_repel is good at getting labels out
      # of the way, though it doesn't necessarily mean they don't look silly.
      ggrepel::geom_label_repel(
        mapping = ggplot2::aes(
          label = format({{predictor}}, digits = 3)), # TODO make configurable
        data = inverse.prediction.df,
        y = 0,
        show.legend = F) # Don't show a little letter "a" in legend.
      )
    }

    if (inverse.treatment){
      p <- p + make.inverse(individual.rows, {{treatment}}, 1)
    }

    if(inverse.replicate) {
      if (inverse.treatment){
        # Make replicate lines a bit transparent so they are easy to distinguish
        # from and don't hide the treatment lines.
        effictive.replicate.alpha <- replicate.alpha # Needs to be <- not =
      } else {
        # No point making them transparent if there's nothing else.
        # TODO: Or maybe caller has other geoms for treatment?
        effective.replicate.alpha <- 1
      }

      make.inverse(individual.rows, {{replicate}}, effective.replicate.alpha)

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
