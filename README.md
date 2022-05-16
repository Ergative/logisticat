
<!-- README.md is generated from README.Rmd. Please edit that file -->

# logisticat

<!-- badges: start -->
<!-- badges: end -->

The goal of logisticat is to …

## Installation

You can install the development version of logisticat from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Ergative/logisticat")
```

You can uninstall it with:

``` r
remove.packages("logisticat")
```

## Example

Say you wanted to know whether feeding sea urchin larvae higher rations
of food enables them to develop to metamorphosis more quickly.

You run an experiment with two food ration treatments and three
replicate beakers of larvae per treatment. You sample several larvae per
beaker each day and record what number metamorphose.

The below code will plot curves estimating the progress of each beaker
of larvae towards becoming metamorphically competent as a whole
population. These curves are constructed from logistic regressions.

If we are interested in comparing the time at which each treatment
reached 50% metamorphosis, we can specify that as our
“probability.of.interest” and get inverse predictions from the logistic
regression.

``` r
library(logisticat)

# Load example dataset.
# This is data from a hypothetical experiment, where echinoid larvae were raised
# on two rations of food, with the goal of comparing the time larvae in each
# ration treatment took to develop enough to metamorphose.
metamorphosis.data <- metamorphosis()

# Each ration treatment has several replicate beakers, from which a number of
# larvae were tested for metamorphosis each day.
# The number of larvae that succeeded or failed in each trial is listed.
head(metamorphosis.data)
#> # A tibble: 6 x 5
#>    days ration beaker metamorphosed did.not.metamorphose
#>   <dbl> <chr>  <chr>          <dbl>                <dbl>
#> 1    11 low    A                  0                   10
#> 2    11 low    B                  0                   10
#> 3    11 low    C                  0                   10
#> 4    11 high   A                  0                   10
#> 5    11 high   B                  0                   10
#> 6    11 high   C                  0                   10

# We want to determine how long it took larvae in each of the two food ration
# treatments to reach 50% metamorphic competence. This can be visualized
# by plotting a logistic regression and seeing where the curve intersects a
# horizontal line at 0.5.
gglogistic(metamorphosis.data,
           predictor      = days,                # x-axis
           success.counts = metamorphosed,       # number of successes per trial
           failure.counts = did.not.metamorphose,# number of failures per trial
           treatment      = ration,              # column representing treatment
           replicate      = beaker,              # column representing replicate
           probability.of.interest = 0.5,        # y-axis value to find.
           
           # Extra options for customization.
           line.var     = "both",      # See curves for treatment and replicates.
           point.var    = "replicate", # Points per replicate.
           inverse.var  = "treatment", # Per-treatment inverse predictions.
           jitter.width = 0.2,         # Jitter points a bit.
           xlabel = "Days post fertilization",
           ylabel = "Metamorphic competence",
           treatment.colors = c("low" = "#4393C3",
                                "high" = "#5AAE61"))
```

<img src="man/figures/README-example-1.png" width="100%" />
