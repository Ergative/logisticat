testthat::test_that("counts.to.reps works on small data set", {
  expected.output <- tibble::tribble(
    ~label, ~result,
    "p3f0",  T,
    "p3f0",  T,
    "p3f0",  T,
    "p1f1",  T,
    "p0f2",  F,
    "p0f2",  F,
    "p1f1",  F,
  )

  actual.output <- logisticat::outcome.counts.to.rows(
    logisticat::sanity.check(), pass, fail, result)
  testthat::expect_equal(actual.output, expected.output, ignore_attr = T)
})
