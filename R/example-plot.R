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
      boxplot.var = "beaker")
}

jwidth = 1

xlt <- function (){
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
      line.var = "ration",
      inverse.var = NULL,
      jitter.width = jwidth)
}

xpt <- function (){
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
      line.var = NULL,
      point.var = "ration",
      jitter.width = jwidth)
}

xbt <- function (){
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
      line.var = NULL,
      boxplot.var = "ration",
      jitter.width = jwidth)
}

xit <- function (){
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
      line.var = NULL,
      inverse.var = "ration",
      jitter.width = jwidth)
}

xlit <- function (){
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
      line.var = "ration",
      inverse.var = "ration",
      jitter.width = jwidth)
}

xplit <- function (){
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
      line.var = "ration",
      point.var = "ration",
      inverse.var = "ration",
      jitter.width = jwidth)
}






xlr <- function (){
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
      line.var = "beaker",
      inverse.var = NULL,
      jitter.width = jwidth)
}

xpr <- function (){
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
      line.var = NULL,
      point.var = "beaker",
      jitter.width = jwidth)
}

xbr <- function (){
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
      line.var = NULL,
      boxplot.var = "beaker",
      jitter.width = jwidth)
}

xir <- function (){
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
      line.var = NULL,
      inverse.var = "beaker",
      jitter.width = jwidth)
}

xlir <- function (){
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
      line.var = "beaker",
      inverse.var = "beaker",
      jitter.width = jwidth)
}

xplir <- function (){
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
      line.var = "beaker",
      point.var = "beaker",
      inverse.var = "beaker",
      jitter.width = jwidth)
}





xlb <- function (){
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
      line.var = "both",
      inverse.var = NULL,
      jitter.width = jwidth)
}

xpb <- function (){
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
      line.var = NULL,
      point.var = "both",
      jitter.width = jwidth)
}

xbb <- function (){
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
      line.var = NULL,
      boxplot.var = "both",
      jitter.width = jwidth)
}

xib <- function (){
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
      line.var = NULL,
      inverse.var = "both",
      jitter.width = jwidth)
}

xlib <- function (){
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
      line.var = "both",
      inverse.var = "both",
      jitter.width = jwidth)
}

xplib <- function (){
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
      line.var = "both",
      point.var = "both",
      inverse.var = "both",
      jitter.width = jwidth)
}

