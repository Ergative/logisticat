test_that("inverse.predict works", {
  reps <-
    logisticat::outcome.counts.to.rows(
      logisticat::metamorphosis(),
      metamorphosed,
      did.not.metamorphose,
      competent)

  regression <- logisticat::logistic.regression(reps, days, competent)

  testthat::expect_equal(
    Vectorize(inverse.predict, vectorize.args = "probability")
    (regression,
      c(0.001, 0.010, 0.100,
        0.400, 0.500, 0.600,
        0.900, 0.990, 0.999)),
    c( 4.999757,  7.972516, 11.05621,
      13.36041 , 13.88183 , 14.40326,
      16.70746 , 19.79115 , 22.76391),
    tolerance = 0.00001)
})
