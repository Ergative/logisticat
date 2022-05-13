are.all.required.geoms.present <- function(p, required.classes){
  # Each geom is stored in a ggplot's layers list.
  geoms <- lapply(p[["layers"]], function(layer) {layer[["geom"]]})
  classes <- unlist(lapply(geoms, class))

  n.found.classes = 0
  for (class_ in required.classes) {
    if (class_ %in% classes){
      n.found.classes = n.found.classes + 1
    }
  }

  return(n.found.classes == length(required.classes))
}


test_that("gglotistic plots something", {
  testthat::expect_s3_class(example.plot(), c("gg", "ggplot"))
})

test_that("gglotistic can take character arguments as variables", {
  testthat::expect_s3_class(logisticat::metamorphosis() %>%
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
                                treatment.colors = c("high" = "#f2af09",
                                                     "low" = "#ea1578")),
                            c("gg", "ggplot"))
})

test_that("gglotistic can take symbols as variables", {
  testthat::expect_s3_class(logisticat::metamorphosis() %>%
                              logisticat::gglogistic(
                                predictor = days,
                                success.counts = metamorphosed,
                                failure.counts = did.not.metamorphose,
                                treatment = ration,
                                replicate = beaker,
                                probability.of.interest = 0.5,
                                xlabel = "Age (days)",
                                ylabel = "Metamorphic Competence",
                                treatment.label = "Algal Ration",
                                treatment.colors = c("high" = "#f2af09",
                                                     "low" = "#ea1578")),
                            c("gg", "ggplot"))
})

test_that("gglotistic makes desired geoms", {
  p <-
    logisticat::metamorphosis() %>%
    logisticat::gglogistic(
      predictor = days,
      success.counts = metamorphosed,
      failure.counts = did.not.metamorphose,
      treatment = ration,
      replicate = "beaker",
      probability.of.interest = 0.5,
      xlabel = "Age (days)",
      ylabel = "Metamorphic Competence",
      treatment.label = "Algal Ration",
      treatment.colors = c("high" = "#f2af09","low" = "#ea1578"),
      line.var = "ration",
      boxplot.var = "ration",
      point.var = "ration",
      inverse.var = "ration")

  testthat::expect_equal(
    are.all.required.geoms.present(p, c("GeomLine",
                                        "GeomBoxplot",
                                        "GeomHline",
                                        "GeomSegment",
                                        "GeomLabelRepel")),
    T)
})
