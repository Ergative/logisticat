testthat::test_that("counts.to.reps works on small data set", {
  expected.output <- tibble::tribble(
    ~label, ~result,
    "p3f0",  1,
    "p3f0",  1,
    "p3f0",  1,
    "p1f1",  1,
    "p0f2",  0,
    "p0f2",  0,
    "p1f1",  0,
  )

  actual.output <- logisticat::outcome.counts.to.rows(
    logisticat::sanity.check(), pass, fail, result)
  testthat::expect_equal(actual.output, expected.output, ignore_attr = T)
})
