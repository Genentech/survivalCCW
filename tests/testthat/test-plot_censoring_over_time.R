test_that("Plots are the same as previously", {

  library(ggplot2)

  # Load the toy dataset
  data(dummy_data)
  
  # Create clones
  clones <- create_clones(dummy_data, 
                          id = "id", 
                          event = "death", 
                          time_to_event = "fup_obs", 
                          exposure = "surgery", 
                          time_to_exposure = "timetosurgery", 
                          ced_window = 365.25/2)

  # Generate the plot
  plot <- plot_censoring_over_time(clones)

  # Use vdiffr to check if the plot is the same as previously
  vdiffr::expect_doppelganger(
    title = "Censoring over time",
    plot
  )

})