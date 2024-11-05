test_that("incorrect classes are caught", {

  df <- data.frame(
    id = c(1, 2, 3, 4),
    event = c(0L, 1L, 0L, 1L),
    time_to_event = c(100, 200, 100, 200),
    exposure = c(0L, 1L, 0L, 1L),
    time_to_exposure = c(NA_real_, 2.3, NA_real_, 3.3),
    ham = c(0, 1, 0, 1)
  )

  expect_error(
    generate_ccw(df, predvars = c("ham")),
    "Must inherit from class 'ccw_clones_long', but has class 'data.frame'"
  )

  df_long <- dummy_data |>
    create_clones(id = "id", event = "event", time_to_event = "timetoevent", exposure = "exposure", time_to_exposure = "timetoexposure", ced_window = 100) |>
    cast_clones_to_long()

  attributes(df_long)$id <- NULL

  expect_error(
    generate_ccw(df_long, "ham"),
    "The input data.frame is missing at least one attribute: id, event, time_to_event, exposure, time_to_exposure, ced_window. Did you remove these or try to make a custom data.frame?"
  )

  df_long$t_start <- NULL

  expect_error(
    generate_ccw(df_long),
    "The input data.frame is missing at least one of the required columns: outcome, fup_outcome, censor, fup_censor, clone, t_start, t_stop, time_id, t_event. Did you remove this?"
  )
})

test_that("when predvars columns are missing, an error is thrown", {

  df_long <- dummy_data |>
    create_clones(id = "id", event = "event", time_to_event = "timetoevent", exposure = "exposure", time_to_exposure = "timetoexposure", ced_window = 100) |>
    cast_clones_to_long()

  expect_error(
    generate_ccw(df_long, predvars = "hamburger"),
    "At least one of these predvars columns is not on the data.frame: hamburger"
  )

  expect_error(
    generate_ccw(df_long, predvars = NULL),
    "predvars cannot be NULL. Please specify at least one variable to use for weights"
  )

})

test_that("categorical vars are dealt with", {

  dummy_data_s <- dummy_data
  dummy_data_s$sandwich <- rep(c("ham", "turkey", "cheese"), length.out = NROW(dummy_data_s))

  df_long <- dummy_data_s |>
    create_clones(id = "id", event = "event", time_to_event = "timetoevent", exposure = "exposure", time_to_exposure = "timetoexposure", ced_window = 100) |>
    cast_clones_to_long()

  expect_error(
    generate_ccw(df_long, predvars = "sandwich"),
    "At least one of the predvars columns is character/factor. In this early version of `survivalCCW`, only numeric variables are considered. Please make dummy vars on your own!"
  )

  df_long$sandwich_f <- factor(df_long$sandwich)

  expect_error(
    generate_ccw(df_long, predvars = "sandwich_f"),
    "At least one of the predvars columns is character/factor. In this early version of `survivalCCW`, only numeric variables are considered. Please make dummy vars on your own!"
  )

})
