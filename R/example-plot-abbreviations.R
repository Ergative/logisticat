# Mnemonic function names for quickly testing example data set with various
# plotting options.

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
      line.var = "treatment",
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
      line.var = "none",
      point.var = "treatment",
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
      line.var = "none",
      boxplot.var = "treatment",
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
      line.var = "none",
      inverse.var = "treatment",
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
      line.var = "treatment",
      inverse.var = "treatment",
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
      line.var = "treatment",
      point.var = "treatment",
      inverse.var = "treatment",
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
      line.var = "replicate",
      inverse.var = "none",
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
      line.var = "none",
      point.var = "replicate",
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
      line.var = "none",
      boxplot.var = "replicate",
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
      line.var = "none",
      inverse.var = "replicate",
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
      line.var = "replicate",
      inverse.var = "replicate",
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
      line.var = "replicate",
      point.var = "replicate",
      inverse.var = "replicate",
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
      inverse.var = "none",
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
      line.var = "none",
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
      line.var = "none",
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
      line.var = "none",
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

