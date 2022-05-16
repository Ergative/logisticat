testthat::test_that("outcome.counts.to.rows works on small data set", {
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
    # Triple colon so we can test without exporting it.
    logisticat:::sanity.check(), pass, fail, result)
  testthat::expect_equal(actual.output, expected.output, ignore_attr = T)
})

testthat::test_that("outcome.counts.to.rows works on larger data set", {
  actual.output <- outcome.counts.to.rows(metamorphosis(),
                                          metamorphosed,
                                          did.not.metamorphose,
                                          competent)
  testthat::expect_equal(nrow(actual.output), 480, ignore_attr = T)
  testthat::expect_equal(ncol(actual.output), 4, ignore_attr = T)
})
