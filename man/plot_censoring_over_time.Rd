% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_censoring_over_time.R
\name{plot_censoring_over_time}
\alias{plot_censoring_over_time}
\title{Plot censoring over time}
\usage{
plot_censoring_over_time(df)
}
\arguments{
\item{df}{A data.frame with one row per clone per observation period that contains weights for each patient-period as returned by \code{\link[=create_clones]{create_clones()}}}
}
\value{
a ggplot2 object
}
\description{
Plot censoring over time
}
\examples{

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

plot_censoring_over_time(clones)
}
