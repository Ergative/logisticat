test_that("gglotistic plots something", {
  testthat::expect_s3_class(example.plot(), c("gg", "ggplot"))
})
