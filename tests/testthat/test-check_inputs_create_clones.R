test_that("incorrect input types are caught", {

  df <- data.frame(
    id = c(1, 2, 3, 4),
    event = c(0L, 1L, 0L, 1L),
    time_to_event = c(100, 200, 100, 200),
    exposure = c(0L, 1L, 0L, 1L),
    time_to_exposure = c(NA_real_, 2.3, NA_real_, 3.3)
  )

  expect_error(
    create_clones_check_inputs(df, id = id, event = "event", time_to_event = "time_to_event", exposure = "exposure", time_to_exposure = "time_to_exposure", ced_window = 200),
    "'id' not found"
  )

  expect_error(
    create_clones_check_inputs(df, id = "id", event = df$event, time_to_event = "time_to_event", exposure = "exposure", time_to_exposure = "time_to_exposure", ced_window = 200),
    "Assertion on 'event' failed"
  )

  expect_error(
    create_clones_check_inputs(df, id = "id", event = "event", time_to_event = 2.0, exposure = "exposure", time_to_exposure = "time_to_exposure", ced_window = 200),
    "Assertion on 'time_to_event' failed"
  )

  expect_error(
    create_clones_check_inputs(df, id = "id", event = "event", time_to_event = "time_to_event",  exposure = "exposure", time_to_exposure = "time_to_exposure", ced_window = "ced_window"),
    "Assertion on 'ced_window' failed"
  )

})

test_that("columns not in the data are caught", {

  df <- data.frame(
    id = c(1, 2, 3, 4),
    event = c(0L, 1L, 0L, 1L),
    time_to_event = c(100, 200, 100, 200),
    exposure = c(0L, 1L, 0L, 1L),
    time_to_exposure = c(NA_real_, 2.3, NA_real_, 3.3)
  )

  expect_error(
    create_clones_check_inputs(df, id = "hamburger", event = "event", time_to_event = "time_to_event", exposure = "exposure", time_to_exposure = "time_to_exposure", ced_window = 200),
    "Must be a subset of \\{'id','event','time_to_event','exposure','time_to_exposure'\\}, but has additional elements \\{'hamburger'\\}"
  )

  expect_error(
    create_clones_check_inputs(df, id = "id", event = "sausage", time_to_event = "time_to_event", exposure = "exposure", time_to_exposure = "time_to_exposure", ced_window = 200),
    "Must be a subset of \\{'id','event','time_to_event','exposure','time_to_exposure'\\}, but has additional elements \\{'sausage'\\}"
  )

  expect_error(
    create_clones_check_inputs(df, id = "id", event = "event", time_to_event = "time_to_event", exposure = "pumpkin", time_to_exposure = "time_to_exposure", ced_window = 200),
    "Must be a subset of \\{'id','event','time_to_event','exposure','time_to_exposure'\\}, but has additional elements \\{'pumpkin'\\}"
  )

})

test_that("missing data is caught when in study columns (other than time to exposure)", {

  df <- data.frame(
    id = c(1, 2, 3, 4),
    event = c(0L, 1L, NA_integer_, 1L),
    time_to_event = c(100, 200, 100, 200),
    exposure = c(0L, 1L, 0L, 1L),
    time_to_exposure = c(NA_real_, 2.3, NA_real_, 3.3)
  )

  expect_error(
    create_clones_check_inputs(df, id = "id", event = "event", time_to_event = "time_to_event", exposure = "exposure", time_to_exposure = "time_to_exposure", ced_window = 200),
    "There are missing data in one of the study columns: id, event, time_to_event, exposure"
  )

})

test_that("duplicate column names are caught", {

  df <- data.frame(
    id = c(1, 2, 3, 4),
    event = c(0L, 1L, 0L, 1L),
    time_to_event = c(100, 200, 100, 200),
    exposure = c(0L, 1L, 0L, 1L),
    time_to_exposure = c(NA_real_, 2.3, NA_real_, 3.3)
  )

  expect_error(
    create_clones_check_inputs(df, id = "id", event = "event", time_to_event = "event", exposure = "exposure", time_to_exposure = "time_to_exposure", ced_window = 200),
    "You passed the same column name twice"
  )

})

test_that("incorrect classes in columns are caught", {

  df <- data.frame(
    id = c(1, 2, 3, 4),
    event = c('ham', 'is', 'good', 'food'),
    time_to_event = c(100, 200, 100, 200),
    exposure = c(0L, 1L, 0L, 1L),
    time_to_exposure = c(NA_real_, 2.3, NA_real_, 3.3)
  )

  expect_error(
    create_clones_check_inputs(df, id = "id", event = "event", time_to_event = "time_to_event", exposure = "exposure", time_to_exposure = "time_to_exposure", ced_window = 200),
    "class\\(df\\[, event\\]\\)"
  )

  df <- data.frame(
    id = c(1, 2, 3, 4),
    event = c(1.0, 1.0, 0.0, 1.0),
    time_to_event = c(100, 200, 100, 200),
    exposure = c(0L, 1L, 0L, 1L),
    time_to_exposure = c(NA_real_, 2.3, NA_real_, 3.3)
  )

  expect_error(
    create_clones_check_inputs(df, id = "id", event = "event", time_to_event = "time_to_event", exposure = "exposure", time_to_exposure = "time_to_exposure", ced_window = 200),
    "class\\(df\\[, event\\]\\)"
  )

  df <- data.frame(
    id = c(1, 2, 3, 4),
    event = c(0L, 1L, 0L, 1L),
    time_to_event = c(100, 200, 100, 200),
    exposure = c(1.0, 1.0, 0.0, 1.0),
    time_to_exposure = c(2.2, 2.3, NA_real_, 3.3)
  )

  expect_error(
    create_clones_check_inputs(df, id = "id", event = "event", time_to_event = "time_to_event", exposure = "exposure", time_to_exposure = "time_to_exposure", ced_window = 200),
    "class\\(df\\[, exposure\\]\\)"
  )


  df <- data.frame(
    id = c(1, 2, 3, 4),
    event = c(0L, 1L, 0L, 1L),
    time_to_event = c('ham', 'is', 'great', 'food'),
    exposure = c(0L, 1L, 0L, 1L),
    time_to_exposure = c(NA_real_, 2.3, NA_real_, 3.3)
  )

  expect_error(
    create_clones_check_inputs(df, id = "id", event = "event", time_to_event = "time_to_event", exposure = "exposure", time_to_exposure = "time_to_exposure", ced_window = 200),
    "df\\[, time_to_event\\]"
  )

  df <- data.frame(
    id = c(1, 2, 3, 4),
    event = c(0L, 1L, 0L, 1L),
    time_to_event = c(100, 200, 100, 200),
    exposure = c(0L, 1L, 0L, 1L),
    time_to_exposure = factor(c(NA_character_, 'is', NA_character_, 'food'))
  )

  expect_error(
    create_clones_check_inputs(df, id = "id", event = "event", time_to_event = "time_to_event", exposure = "exposure", time_to_exposure = "time_to_exposure", ced_window = 200),
    "df\\[, time_to_exposure\\]"
  )

})

test_that("incomplete time to exposure is caught", {

  df <- data.frame(
    id = c(1, 2, 3, 4),
    event = c(0L, 1L, 0L, 1L),
    time_to_event = c(100, 200, 100, 200),
    exposure = c(T, T, F, T),
    time_to_exposure = c(1.1, NA_real_, NA_real_, 3.3)
  )

  expect_error(
    create_clones_check_inputs(df, id = "id", event = "event", time_to_event = "time_to_event", exposure = "exposure", time_to_exposure = "time_to_exposure", ced_window = 200),
    "Time to exposure should be complete for patients who have exposure = 1"
  )

  df <- data.frame(
    id = c(1, 2, 3, 4),
    event = c(0L, 1L, 0L, 1L),
    time_to_event = c(100, 200, 100, 200),
    exposure = c(T, T, F, T),
    time_to_exposure = c(1.1, 3.3, 9.4, 3.3)
  )

  expect_error(
    create_clones_check_inputs(df, id = "id", event = "event", time_to_event = "time_to_event", exposure = "exposure", time_to_exposure = "time_to_exposure", ced_window = 200),
    "Time to exposure should only be for patients who received the exposure at some time"
  )

})

test_that("protected column names are blocked", {

  df <- data.frame(
    id = c(1, 2, 3, 4),
    clone = c(0L, 1L, 0L, 1L),
    time_to_event = c(100, 200, 100, 200),
    exposure = c(T, T, F, T),
    time_to_exposure = c(1.1, 3.3, NA_real_, 3.3)
  )

  expect_error(
    create_clones_check_inputs(df, id = "id", event = "clone", time_to_event = "time_to_event", exposure = "exposure", time_to_exposure = "time_to_exposure", ced_window = 200),
    "'clone' is a protected column name"
  )

  df <- data.frame(
    id = c(1, 2, 3, 4),
    event = c(0L, 1L, 0L, 1L),
    time_to_event = c(100, 200, 100, 200),
    censor = c(T, T, F, T),
    time_to_exposure = c(1.1, 3.3, NA_real_, 3.3)
  )

  expect_error(
    create_clones_check_inputs(df, id = "id", event = "event", time_to_event = "time_to_event", exposure = "censor", time_to_exposure = "time_to_exposure", ced_window = 200),
    "'censor' is a protected column name"
  )

})

test_that("incorrect exposure vars are caught (should be 0/1 or T/F)", {

  df <- data.frame(
    id = c(1, 2, 3, 4),
    event = c(0L, 0L, 0L, 0L),
    time_to_event = c(100, 200, 100, 200),
    exposure = c(0L, 1L, 2L, 3L),
    time_to_exposure = c(NA_real_, 2.2, 3.3, 3.3)
  )

  expect_error(
    create_clones_check_inputs(df, id = "id", event = "event", time_to_event = "time_to_event", exposure = "exposure", time_to_exposure = "time_to_exposure", ced_window = 200),
    "Exposure should be 0/1 or T/F"
  )

})

test_that("Outcomes before exposure dates are caught", {

  df <- data.frame(
    id = c(1, 2, 3, 4),
    event = c(1L, 1L, 0L, 0L),
    time_to_event = c(1, 2, 1, 2),
    exposure = c(1L, 1L, 0L, 1L),
    time_to_exposure = c(1.1, 3.3, NA_real_, 3.3)
  )

  expect_error(
    create_clones_check_inputs(df, id = "id", event = "event", time_to_event = "time_to_event", exposure = "exposure", time_to_exposure = "time_to_exposure", ced_window = 1.0),
    "There are outcomes before exposure dates"
  )

})


test_that("Time to event cannot be negative", {

  df <- data.frame(
    id = c(1, 2, 3, 4),
    event = c(1L, 1L, 0L, 0L),
    time_to_event = c(1, 2, -1, 2),
    exposure = c(1L, 1L, 0L, 1L),
    time_to_exposure = c(0.9, 1.9, NA_real_, 1.3)
  )

  expect_error(
    create_clones_check_inputs(df, id = "id", event = "event", time_to_event = "time_to_event", exposure = "exposure", time_to_exposure = "time_to_exposure", ced_window = 1.0),
    "Time to event cannot be negative"
  )

})


test_that("Time to exposure cannot be negative", {

  df <- data.frame(
    id = c(1, 2, 3, 4),
    event = c(1L, 1L, 0L, 0L),
    time_to_event = c(1, 2, 1, 2),
    exposure = c(1L, 1L, 1L, 1L),
    time_to_exposure = c(0.9, 1.9, -2, 1.3)
  )

  expect_error(
    create_clones_check_inputs(df, id = "id", event = "event", time_to_event = "time_to_event", exposure = "exposure", time_to_exposure = "time_to_exposure", ced_window = 1.0),
    "Time to exposure cannot be negative"
  )

})