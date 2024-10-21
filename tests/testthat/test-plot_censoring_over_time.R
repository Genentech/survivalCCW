test_that("Plots are the same as previously", {

  library(ggplot2)

  # Load the toy dataset
  data(dummy_data)
  
  # Create clones
  clones <- create_clones(dummy_data, 
                          id = "id", 
                          event = "event", 
                          time_to_event = "timetoevent", 
                          exposure = "exposure", 
                          time_to_exposure = "timetoexposure", 
                          ced_window = 100)

  # Generate the plot
  plot <- plot_censoring_over_time(clones)

  # Use vdiffr to check if the plot is the same as previously
  vdiffr::expect_doppelganger(
    title = "Censoring over time",
    plot
  )

})