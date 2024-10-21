test_that("plus signs are caught in column names", {
  
  df <- data.frame(
    id = c(1, 2, 3, 4),
    event = c('ham', 'is', 'good', 'food'),
    time_to_event = c(100, 200, 100, 200),
    exposure = c(0L, 1L, 0L, 1L),
    time_to_exposure = c(NA_real_, 2.3, NA_real_, 3.3)
  )

  expect_error(
    generate_ccw_calc_weights(df, event_times_df, predvars = "hamburger+"),
    "No plus signs allowed in column names -- this impacts the formula creation"
  )

})
