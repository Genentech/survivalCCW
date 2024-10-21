test_that("Plots are the same as previously", {

  library(ggplot2)

  clones <- create_clones(dummy_data, 
                          id = "id", 
                          event = "event", 
                          time_to_event = "timetoevent", 
                          exposure = "exposure", 
                          time_to_exposure = "timetoexposure", 
                          ced_window = 100)

  clones_long <- cast_clones_to_long(clones)
  clones_long_w <- generate_ccw(clones_long, predvars =  c("cov1", "cov2"))

  vdiffr::expect_doppelganger(
    title = "Age over time",
    plot_var_mean_over_time(clones_long_w, "cov1")
  )

})