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
    c(-4.407405, -1.606432, 1.299062,
       3.470109,  3.961405, 4.452701,
       6.623749,  9.529242, 12.33022),
    tolerance = 0.00001)
})
