test_that("logistic.regression works", {
  reps <-
    logisticat::outcome.counts.to.rows(
      logisticat::metamorphosis(),
      metamorphosed,
      did.not.metamorphose,
      competent)

  regression <- logisticat::logistic.regression(reps, days, competent)
  testthat::expect_s3_class(regression, c("glm", "lm"))
  testthat::expect_equal(stats::coef(regression)[[1]],
                         -10.79459, tolerance = 0.00001)
  testthat::expect_equal(stats::coef(regression)[[2]],
                         0.7776058, tolerance = 0.00001)
})
