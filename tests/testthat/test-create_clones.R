test_that("CED trims time to exposure appropriately", {

  df <- data.frame(
    id = c(1, 2, 3, 4),
    event = c(0L, 1L, 0L, 1L),
    time_to_event = c(200L, 250L, 9900L, 100L),
    exposure = c(0L, 1L, 0L, 1L),
    time_to_exposure = c(NA_real_, 2.3, NA_real_, 3.3)
  )

  expect_equal(1L, df[df$id==4, "exposure"])

  expect_message(
    ccw_df <- create_clones(df, id = "id", event = "event", time_to_event = "time_to_event", exposure = "exposure", time_to_exposure = "time_to_exposure", ced_window = 2.5),
    "Updating 1 patients' exposure and time-to-exposure based on CED window"
  )

  expect_equal(0L, unique(ccw_df[ccw_df$id==4, "exposure"]))

})

test_that("All clone statuses are correct", {

  # Check statuses for all combinations:
  # Exposed before CED vs unexposed before CED 
  # Censored before CED vs censored after CED
  # Death before CED vs death after CED

  ced <- 100
  exposure_time <- c(80, NA_real_)
  fup_time <- c(85, 125)
  event <- c(0L, 1L)

  df <- expand.grid(
    exposure_time = exposure_time,
    fup_time = fup_time,
    event = event
  )

  df$exposure <- !is.na(df$exposure_time)
  df$id <- 1:NROW(df)

  ccw_df <- create_clones(
    df, 
    id = "id", 
    event = "event", 
    time_to_event = "fup_time", 
    exposure = "exposure", 
    time_to_exposure = "exposure_time", 
    ced_window = ced
  )

  ## Truly exposed
  ### Exposed clone
  expect_equal(ccw_df[ccw_df$id %in% df[df$exposure == 1L, "id"] & ccw_df$clone == 1L, "outcome"], df[df$exposure == 1L, "event"])
  expect_equal(ccw_df[ccw_df$id %in% df[df$exposure == 1L, "id"] & ccw_df$clone == 1L, "fup_outcome"], df[df$exposure == 1L, "fup_time"])
  expect_true(all(ccw_df[ccw_df$id %in% df[df$exposure == 1L, "id"] & ccw_df$clone == 1L, "censor"] == 0L))
  expect_equal(ccw_df[ccw_df$id %in% df[df$exposure == 1L, "id"] & ccw_df$clone == 1L, "fup_censor"], df[df$exposure == 1L, "exposure_time"])
  
  ### Unexposed clone
  expect_true(all(ccw_df[ccw_df$id %in% df[df$exposure == 1L, "id"] & ccw_df$clone == 0L, "outcome"] == 0))
  expect_equal(ccw_df[ccw_df$id %in% df[df$exposure == 1L, "id"] & ccw_df$clone == 0L, "fup_outcome"], df[df$exposure == 1L, "exposure_time"])
  expect_true(all(ccw_df[ccw_df$id %in% df[df$exposure == 1L, "id"] & ccw_df$clone == 0L, "censor"] == 1L))
  expect_equal(ccw_df[ccw_df$id %in% df[df$exposure == 1L, "id"] & ccw_df$clone == 0L, "fup_censor"], df[df$exposure == 1L, "exposure_time"])

  ## Truly unexposed
  ### Censor before CED 
  #### Exposed clone
  expect_equal(ccw_df[ccw_df$id %in% df[df$exposure == 0L & df$fup_time < ced, "id"] & ccw_df$clone == 1L, "outcome"], df[df$exposure == 0L & df$fup_time < ced, "event"])
  expect_equal(ccw_df[ccw_df$id %in% df[df$exposure == 0L & df$fup_time < ced, "id"] & ccw_df$clone == 1L, "fup_outcome"], df[df$exposure == 0L & df$fup_time < ced, "fup_time"])
  expect_true(all(ccw_df[ccw_df$id %in% df[df$exposure == 0L & df$fup_time < ced, "id"] & ccw_df$clone == 1L, "censor"] == 0L))
  expect_equal(ccw_df[ccw_df$id %in% df[df$exposure == 0L & df$fup_time < ced, "id"] & ccw_df$clone == 1L, "fup_censor"], df[df$exposure == 0L & df$fup_time < ced, "fup_time"])

  #### Unexposed clone
  expect_equal(ccw_df[ccw_df$id %in% df[df$exposure == 0L & df$fup_time < ced, "id"] & ccw_df$clone == 0L, "outcome"], df[df$exposure == 0L & df$fup_time < ced, "event"])
  expect_equal(ccw_df[ccw_df$id %in% df[df$exposure == 0L & df$fup_time < ced, "id"] & ccw_df$clone == 0L, "fup_outcome"], df[df$exposure == 0L & df$fup_time < ced, "fup_time"])
  expect_true(all(ccw_df[ccw_df$id %in% df[df$exposure == 0L & df$fup_time < ced, "id"] & ccw_df$clone == 0L, "censor"] == 0L))
  expect_equal(ccw_df[ccw_df$id %in% df[df$exposure == 0L & df$fup_time < ced, "id"] & ccw_df$clone == 0L, "fup_censor"], df[df$exposure == 0L & df$fup_time < ced, "fup_time"])

  ### Censor after CED
  #### Exposed clone
  expect_true(all(ccw_df[ccw_df$id %in% df[df$exposure == 0L & df$fup_time >= ced, "id"] & ccw_df$clone == 1L, "outcome"] == 0))
  expect_true(all(ccw_df[ccw_df$id %in% df[df$exposure == 0L & df$fup_time >= ced, "id"] & ccw_df$clone == 1L, "fup_outcome"] == ced))
  expect_true(all(ccw_df[ccw_df$id %in% df[df$exposure == 0L & df$fup_time >= ced, "id"] & ccw_df$clone == 1L, "censor"] == 1L))
  expect_true(all(ccw_df[ccw_df$id %in% df[df$exposure == 0L & df$fup_time >= ced, "id"] & ccw_df$clone == 1L, "fup_censor"] == ced))

  #### Unexposed clone
  expect_equal(ccw_df[ccw_df$id %in% df[df$exposure == 0L & df$fup_time >= ced, "id"] & ccw_df$clone == 0L, "outcome"], df[df$exposure == 0L & df$fup_time >= ced, "event"])
  expect_equal(ccw_df[ccw_df$id %in% df[df$exposure == 0L & df$fup_time >= ced, "id"] & ccw_df$clone == 0L, "fup_outcome"], df[df$exposure == 0L & df$fup_time >= ced, "fup_time"])
  expect_true(all(ccw_df[ccw_df$id %in% df[df$exposure == 0L & df$fup_time >= ced, "id"] & ccw_df$clone == 0L, "censor"] == 0L))
  expect_true(all(ccw_df[ccw_df$id %in% df[df$exposure == 0L & df$fup_time >= ced, "id"] & ccw_df$clone == 0L, "fup_censor"] == ced))

})


test_that("Study attributes are passed correctly", {

  df <- data.frame(
    id = 1:6,
    event = c(1L, 1L, 1L, 1L, 1L, 1L),
    time_to_event = c(10, 100, 100, 10, 100, 100),
    exposure = c(rep(0L, 3), rep(1L, 3)),
    time_to_exposure = c(rep(NA_real_, 3), 2, 8, 12)
  )

  ccw_df <- create_clones(df,
                          id = "id", 
                          event = "event", 
                          time_to_event = "time_to_event", 
                          exposure = "exposure", 
                          time_to_exposure = "time_to_exposure", 
                          ced_window = 20)

  expect_equal(
    attributes(ccw_df)$id,
    "id"
  )

  expect_equal(
    attributes(ccw_df)$event,
    "event"
  )

  expect_equal(
    attributes(ccw_df)$time_to_event,
    "time_to_event"
  )

  expect_equal(
    attributes(ccw_df)$exposure,
    "exposure"
  )

  expect_equal(
    attributes(ccw_df)$time_to_exposure,
    "time_to_exposure"
  )

  expect_equal(
    attributes(ccw_df)$ced_window,
    20
  )

})