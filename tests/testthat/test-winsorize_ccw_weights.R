toy_df_ <- data.frame(
  outcome = rep(1, 16),
  fup_outcome = rep(1, 16),
  censor = rep(0, 16),
  fup_censor = rep(1, 16),
  clone = rep(0:1, each = 2),
  t_start = rep(0, 16),
  t_stop = rep(1, 16),
  time_id = rep(1:16, each = 5),
  t_event = rep(1, 16),
  weight_cox = seq(0.1, 1.6, .1)
)

class(toy_df_) <- c("ccw_clones_long_weights", class(toy_df_))
attributes(toy_df_)$weight_name <- "weight_cox"

# Input checks ---- 
test_that("incorrect classes are caught", {
  
  toy_df_2 <- toy_df_
  class(toy_df_2) <- "data.frame"
  expect_error(winsorize_ccw_weights(toy_df_2, c(0.01, 0.99), per_clone = FALSE), "Must inherit from class 'ccw_clones_long_weights'")

})

test_that("required columns are there", {
  toy_df_3 <- toy_df_[,-which(colnames(toy_df_) == "outcome")]
  expect_error(winsorize_ccw_weights(toy_df_3, c(0.01, 0.99), per_clone = FALSE), "The input data.frame is missing at least one of the required columns")
})

test_that("correct length of quantiles", {
  expect_error(winsorize_ccw_weights(toy_df_, c(0.5), per_clone = FALSE), "quantiles must be a vector of two values between 0 and 1.")
})

test_that("within range [0,1]", {
  expect_error(winsorize_ccw_weights(toy_df_, c(-0.1, 1.1), per_clone = FALSE), "quantiles must be between 0 and 1.")
})

test_that("passed in correct order", {
  expect_error(winsorize_ccw_weights(toy_df_, c(0.5, 0.3), per_clone = FALSE), "The first quantile must be smaller than the second quantile")
})

# Input checks ---- 
test_that("works with default quantiles and per_clone = FALSE", {

  q <- c(0.5, 1.0)

  # Period 1 is 0.1, 0.2, 0.3, 0.4
  r1 <- c(0.25, 0.25, 0.3, 0.4)

  # Period 2 is 0.5, 0.6, 0.7, 0.8
  r2 <- c(0.65, 0.65, 0.7, 0.8)

  # Period 3 is 0.9, 1.0, 1.1, 1.2
  r3 <- c(1.05, 1.05, 1.1, 1.2)

  result <- winsorize_ccw_weights(toy_df_, q, per_clone = FALSE)
  expect_equal(result$weight_cox, c(r1, r2, r3))

  q <- c(0.0, 0.5)

  # Period 1 is 0.1, 0.2, 0.3, 0.4
  r1 <- c(0.1, 0.2, 0.25, 0.25)

  # Period 2 is 0.5, 0.6, 0.7, 0.8
  r2 <- c(0.5, 0.6, 0.65, 0.65)

  # Period 3 is 0.9, 1.0, 1.1, 1.2
  r3 <- c(0.9, 1.0, 1.05, 1.05)

  result <- winsorize_ccw_weights(toy_df_, q, per_clone = FALSE)
  expect_equal(result$weight_cox, c(r1, r2, r3))

})

test_that("works with default quantiles and per_clone = TRUE", {

  q <- c(0.5, 1.0)

  # Period 1 - clone 1 is 0.1, 0.2, 0.3



})