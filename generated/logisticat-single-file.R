# logisticat

# By Peter Nilsson
# This source file was automatically generated from an R project on 2022/05/16.

# NOTE: You will need to install the following packages:
# dplyr
# ggplot2
# ggrepel
# glue
# grDevices
# magrittr
# rlang (>= 0.4.11)
# scales
# stats
# tibble
# tidyr

# Package: logisticat
# Title: Plot Inverse Predictions for Logistic Regressions
# Version: 0.0.0.9000
# Authors@R: 
#     person("Peter", "Nilsson", , "peter.nilsson@student.csulb.edu", role = c("aut", "cre"),
#            comment = c(ORCID = "0000-0001-8572-5518"))
# Description: Visualize a logistic regression and calculate the value of a predictor variable for a given probability, such as the time it takes 50% of animals to reach a specific stage of development.
# License: MIT + file LICENSE
# Encoding: UTF-8
# Roxygen: list(markdown = TRUE)
# RoxygenNote: 7.1.2
# Imports: 
#     dplyr,
#     ggplot2,
#     ggrepel,
#     glue,
#     grDevices,
#     magrittr,
#     rlang (>= 0.4.11),
#     scales,
#     stats,
#     tibble,
#     tidyr
# Suggests: 
#     roxygen2,
#     testthat (>= 3.0.0)
# Config/testthat/edition: 3
# URL: https://github.com/Ergative/logisticat
# BugReports: https://github.com/Ergative/logisticat/issues



## ==== IMPORTED SYMBOLS ==================================================== ##

"%>%" <- magrittr::"%>%"
":=" <- rlang::":="
.data <- rlang::.data
as_label <- rlang::as_label
as_name <- rlang::as_name
enquo <- rlang::enquo
enquos <- rlang::enquos


## ==== example-data.R ====================================================== ##

#' Data for a hypothetical experiment on time to competence in echinoid larvae.
#'
#' When exposed to potassium chloride, and echinoid larvae will either
#' metamorphose, in which case it has reached metamorphic competence, or it will
#' fail to metamorphose, in which case it is not competent.
#'
#' Note that the total of the "success" and "failure" counts don't need to
#' add up to the same value, they just happen to in this data set.
#'
#' @return A `data.frame` with example data.
#' @export
metamorphosis <- function() {
  # days:                 The predictor variable - age in days.
  # ration:               Treatment groups.
  # beaker:               Replicate groups within each treatment.
  # metamorphosed:        The "success" count column.
  # did.not.metamorphose: The "failure" count column.
  tibble::tribble(
    ~days, ~ration, ~beaker, ~metamorphosed, ~did.not.metamorphose,
       11,   "low",     "A",              0,                    10,
       11,   "low",     "B",              0,                    10,
       11,   "low",     "C",              0,                    10,
       11,  "high",     "A",              0,                    10,
       11,  "high",     "B",              0,                    10,
       11,  "high",     "C",              0,                    10,

       11,   "low",     "A",              1,                     9,
       11,   "low",     "B",              0,                    10,
       11,   "low",     "C",              2,                     8,
       11,  "high",     "A",              1,                     9,
       11,  "high",     "B",              1,                     9,
       11,  "high",     "C",              1,                     9,

       12,   "low",     "A",              2,                     8,
       12,   "low",     "B",              2,                     8,
       12,   "low",     "C",              4,                     6,
       12,  "high",     "A",              2,                     8,
       12,  "high",     "B",              2,                     8,
       12,  "high",     "C",              4,                     6,

       13,   "low",     "A",              2,                     8,
       13,   "low",     "B",              3,                     7,
       13,   "low",     "C",              3,                     7,
       13,  "high",     "A",              5,                     5,
       13,  "high",     "B",              6,                     4,
       13,  "high",     "C",              5,                     5,

       14,   "low",     "A",              2,                     8,
       14,   "low",     "B",              3,                     7,
       14,   "low",     "C",              3,                     7,
       14,  "high",     "A",              8,                     2,
       14,  "high",     "B",              7,                     3,
       14,  "high",     "C",              7,                     3,

       15,   "low",     "A",              5,                     5,
       15,   "low",     "B",              6,                     4,
       15,   "low",     "C",              5,                     5,
       15,  "high",     "A",              8,                     2,
       15,  "high",     "B",              9,                     1,
       15,  "high",     "C",              8,                     2,

       16,   "low",     "A",              6,                     4,
       16,   "low",     "B",              8,                     2,
       16,   "low",     "C",              8,                     2,
       16,  "high",     "A",             10,                     0,
       16,  "high",     "B",             10,                     0,
       16,  "high",     "C",              9,                     1,

       17,   "low",     "A",              7,                     3,
       17,   "low",     "B",              9,                     1,
       17,   "low",     "C",              8,                     2,
       17,  "high",     "A",             10,                     0,
       17,  "high",     "B",             10,                     0,
       17,  "high",     "C",             10,                     0,
  )
}

#' Small data set for use testing functions in this package.
#'
#' @keywords internal
#' @return A `data.frame` with example data.
sanity.check <- function() {
  tibble::tribble(
    ~label, ~pass, ~fail,
    "p3f0",     3,     0, # Three with result=T.
    "p0f2",     0,     2, # Two with result=F.
    "p1f1",     1,     1, # One with result=T, one with result=F.
    "p0f0",     0,     0, # None in output.
  )
}



## ==== example-plot.R ====================================================== ##

#' Create an example plot using the [logisticat::gglogistic()] function.
#'
#' @export
example.plot <- function (){
  logisticat::metamorphosis() %>%
    logisticat::gglogistic(
      predictor = "days",
      success.counts = "metamorphosed",
      failure.counts = "did.not.metamorphose",
      treatment = "ration",
      replicate = "beaker",
      probability.of.interest = 0.5,
      xlabel = "Age (days)",
      ylabel = "Metamorphic Competence",
      treatment.label = "Algal Ration",
      boxplot.var = "replicate")
}

## ==== gglogistic.R ======================================================== ##

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

## ==== inverse-predict.R =================================================== ##

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
#' @keywords internal
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
#' @keywords internal
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

## ==== logistic-regression.R =============================================== ##

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

## ==== make-geoms.R ======================================================== ##

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
#' @param fraction.data Data frame with column representing fraction of success.
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

## ==== outcome-counts-to-rows.R ============================================ ##

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
#'   number 1 representing a success or a 0 representing a failure.
#' @export
outcome.counts.to.rows <- function(data, success.counts, failure.counts, outcome) {

  # Make as many copies of each row as there were successes,
  # throw away the success/failure counts, and mark each one
  # has having the "1" outcome.
  successes <- data %>%
    tidyr::uncount({{success.counts}}) %>%
    dplyr::select(-{{failure.counts}}) %>%
    dplyr::mutate({{outcome}} := 1)

  # Make as many copies of each row as there were failures,
  # throw away the success/failure counts, and mark each one
  # has having the "0" outcome.
  failures <- data %>%
    tidyr::uncount({{failure.counts}}) %>%
    dplyr::select(-{{success.counts}}) %>%
    dplyr::mutate({{outcome}} := 0)

  # Concatenate all the rows into a single table.
  rbind(successes, failures)
}

## ==== utils.R ============================================================= ##

#' Set the alpha value of a color.
#'
#' @param color Color specified as a name or hex string.
#' @param alpha Value in range \[0,1\] for alpha.
#'
#' @keywords internal
#'
#' @return A hex string representing the color with the chosen alpha.
with.alpha <- Vectorize(function(color, alpha=1) {
  a <- clamp(floor(alpha * 255), 0, 255)
  rgb <- grDevices::col2rgb(color, F)
  paste0("#", byte2hex(rgb[1]), byte2hex(rgb[2]), byte2hex(rgb[3]), byte2hex(a))
}, vectorize.args = "color")


#' Constrain a numeric value within a range.
#'
#' @param value Value to constrain.
#' @param min.value Minimum, inclusive.
#' @param max.value Maximum, inclusive.
#'
#' @keywords internal
#'
#' @return The closest number in the range \[min.value,max.value\] to value.
clamp <- function(value, min.value, max.value) {
  max(min.value, min(max.value, value))
}


#' Get a two-character hex string representation of a number.
#'
#' @param byte Integer value in range \[0,255\]
#'
#' @keywords internal
#'
#' @return String representation of byte.
byte2hex <- function(byte) {
  paste0(nybble2hex(bitwShiftR(byte, 4)), nybble2hex(bitwAnd(byte, 15)))
}


#' Get a hex string representation of a nybble
#'
#' @param nybble Integer value in range \[0,15\]
#'
#' @keywords internal
#'
#' @return String representation of nybble.
nybble2hex <- function(nybble) {
  nybble.names = c("0", "1", "2", "3", "4", "5", "6", "7",
                   "8", "9", "a", "b", "c", "d", "e", "f")
  nybble.names[[nybble + 1]] # 1-indexed.

}


#' Get a vector from a data frame column.
#'
#' @param data Data frame.
#' @param column Column to extract - can be used with \{\{\}\}.
#'
#' @keywords internal
#'
#' @return Vector of column contents.
column.to.vector <- function(data, column) {
  column.name <- as_name(rlang::ensym(column))
  data[[column.name]]
}


#' Get a list a row vectors from a data frame.
#'
#' @param data Data frame.
#'
#' @keywords internal
#'
#' @return A list, with one vector for each row.
list.df.rows <- function(data){
  # See https://stackoverflow.com/a/14370455/4104189
  lapply(stats::setNames(split(data, seq(nrow(data))), rownames(data)),
         unlist) # Unlist so that we have vector elements instead of tibbles.

}


#' C#-style null-coalescing operator.
#' @name nullcoalesce
#' @keywords internal
#' @return Second argument if first argument is NULL, else first argument.
`%??%` <- function(maybe.null, replacement){
  # Extremely weird behavior of devtools::document():
  # If I don't specify a @name for this function, it creates a file
  # "grapes-help-help-grapes.Rd" which I finally figured out is it's attempt
  # to create a human readable name from the string %??%.
  if (is.null(maybe.null)) return(replacement)
  return(maybe.null)
}


log.caller <- function(){
  caller <- sys.call(which=1)
  print (caller)
}
