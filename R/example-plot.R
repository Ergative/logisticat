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
      boxplot.var = "beaker",
      replicate.alpha = 1)
}


