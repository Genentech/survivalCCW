test_that("Plots are the same as previously", {

  clones <- create_clones(toy_df, 
                          id = "id", 
                          event = "death", 
                          time_to_event = "fup_obs", 
                          exposure = "surgery", 
                          time_to_exposure = "timetosurgery", 
                          ced_window = 365.25/2)

  clones_long <- cast_clones_to_long(clones)
  clones_long_w <- generate_ccw(clones_long, predvars =  c("age", "sex", "perf", "stage", "deprivation", "charlson", "emergency"))

  vdiffr::expect_doppelganger(
    title = "Weights over time",
    plot_ccw_over_time(clones_long_w)
  )

})