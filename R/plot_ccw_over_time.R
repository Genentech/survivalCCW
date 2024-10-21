# declare global variables
utils::globalVariables(c("clone", "t_start", "value", "metric"))

#' Visualize weights and variables over time
#' 
#' @param df A data.frame with one row per clone per observation period that contains weights for each patient-period as returned by [generate_ccw()]
#' @return a ggplot2 object
#' 
#' @importFrom stats aggregate median reshape
#' @export
#' @examples 
#' 
#' # Load the toy dataset
#' data(dummy_data)
#' 
#' # Create clones
#' clones <- create_clones(dummy_data, 
#'                         id = "id", 
#'                         event = "death", 
#'                         time_to_event = "fup_obs", 
#'                         exposure = "surgery", 
#'                         time_to_exposure = "timetosurgery", 
#'                         ced_window = 365.25/2)
#' 
#' clones_long <- cast_clones_to_long(clones)
#' clones_long_w <- generate_ccw(clones_long, predvars = c("age"))
#' plot_ccw_over_time(clones_long_w)
plot_ccw_over_time <- function(df) {

   # Confirm ggplot2 loaded
   if (!requireNamespace("ggplot2", quietly = TRUE)) {
      stop("ggplot2 package is required for this function. Install w/ `install.packages('ggplot2')")
   }

   # Confirm inputs are correct
   checkmate::assert_class(df, "ccw_clones_long_weights")
   weight_name <- attributes(df)$weight_name
   ced_window <- attributes(df)$ced_window

   # Calculate mean, median, 25th and 75th quantiles, min, max
   summary_df <- aggregate(df[[weight_name]], by = list(clone = df$clone, t_start = df$t_start), 
                           FUN = function(x) c(mean = mean(x), median = median(x), 
                                               q25 = quantile(x, 0.25), q75 = quantile(x, 0.75), 
                                               min = min(x), max = max(x)))
   
   # Convert the summary_df to a data frame and rename columns
   summary_df <- do.call(data.frame, summary_df)
   colnames(summary_df) <- c("clone", "t_start", "mean", "median", "q25", "q75", "min", "max")

   # Pivot longer
   summary_long <- reshape(summary_df, 
                        varying = list(c( "mean", "median", "q25", "q75", "min", "max")), 
                        v.names = "value", 
                        timevar = "metric", 
                        times = c("mean", "median", "q25", "q75", "min", "max"), 
                        direction = "long")

   summary_long$metric <- factor(summary_long$metric, levels = c("min", "q25", "mean", "median", "q75",  "max"))
   alphas <- c(.1, .3, .8, .8, .3, .1)
   linetypes <- c(1, 1, 3, 1, 1, 1)

   # Plot
   p <- ggplot2::ggplot(summary_long, ggplot2::aes(x = t_start, y = value, linetype = metric, alpha = metric, color = factor(clone))) +
      ggplot2::geom_line(linewidth = 1.25) +
      ggplot2::scale_color_manual(values = c("#1f1fee", "#13b313")) +
      ggplot2::scale_linetype_manual(values = linetypes) +
      ggplot2::scale_alpha_manual(values = alphas) +
      ggplot2::geom_vline(ggplot2::aes(xintercept = ced_window), linetype = "dashed") +
      ggplot2::labs(title = "Weights over time",
           x = "Time",
           y = weight_name,
           linetype = "Metric",
           alpha = "Metric",
           color = "Clone")

   return(p)

}