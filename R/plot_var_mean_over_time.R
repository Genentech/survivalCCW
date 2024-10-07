# declare global variables
utils::globalVariables(c("clone", "t_start", "value", "metric"))

#' Plot a variable over time for each clone, weighted and unweighted
#' 
#' @param df A data.frame with one row per clone per observation period that contains weights for each patient-period as returned by [generate_ccw()]
#' @param var A variable in colnames(df) to plot over time
#' @return a ggplot2 object
#' 
#' @importFrom stats aggregate median reshape
#' @export
#' @examples 
#' 
#' # Load the toy dataset
#' data(toy_df)
#' 
#' # Create clones
#' clones <- create_clones(toy_df, 
#'                         id = "id", 
#'                         event = "death", 
#'                         time_to_event = "fup_obs", 
#'                         exposure = "surgery", 
#'                         time_to_exposure = "timetosurgery", 
#'                         ced_window = 365.25/2)
#' 
#' clones_long <- cast_clones_to_long(clones)
#' clones_long_w <- generate_ccw(clones_long, predvars = c("age"))
#' plot_var_mean_over_time(clones_long_w, "sex")
plot_var_mean_over_time <- function(df, var) {

   # Confirm ggplot2 loaded
   if (!requireNamespace("ggplot2", quietly = TRUE)) {
      stop("ggplot2 package is required for this function. Install w/ `install.packages('ggplot2')")
   }

   # Confirm inputs are correct
   checkmate::assert_class(df, "ccw_clones_long_weights")
   weight_name <- attributes(df)$weight_name
   ced_window <- attributes(df)$ced_window
   valid_cn <- colnames(df)[!colnames(df) %in% c("clone", "t_start", "t_stop",
                                                 "time_id", "t_event", "censor", "fup_censor",
                                                 "id", "fup_obs", weight_name,
                                                 "outcome", "lp", "t", "hazard", "p_uncens", "fup_outcome")]
   if (!var %in% valid_cn) {
      stop("The variable '", var, "' is not in the data.frame. Valid columns are: '", paste(valid_cn, collapse = "', '"), "'.")
   }

   # Calculate weighted and unweighted mean.
   split_df <- split(df, list(df$clone, df$t_start))

   # Custom weight fun
   summary_list <- lapply(
      split_df,
      \(sub_df) {
         unweighted_mean = mean(sub_df[[var]])
         weighted_mean = sum(sub_df[[var]] * sub_df[[weight_name]]) / sum(sub_df[[weight_name]])
         return(data.frame(clone = sub_df$clone[1], t_start = sub_df$t_start[1], unweighted_mean = unweighted_mean, weighted_mean = weighted_mean))
      }
   )

   # Combine the results into a single data frame
   summary_df <- do.call(rbind, summary_list)

   # Pivot longer
   summary_long <- reshape(summary_df, 
                           varying = list(c("unweighted_mean", "weighted_mean")), 
                           v.names = "value", 
                           timevar = "metric", 
                           times = c("unweighted_mean", "weighted_mean"), 
                           direction = "long")
   
   # Convert metric to factor
   summary_long$metric <- factor(summary_long$metric, levels = c("unweighted_mean", "weighted_mean"), labels = c("Unweighted", "Weighted"))

   # Remove NAs (these are periods with only changes in one of the 2 clones)
   summary_long <- summary_long[!is.na(summary_long$clone), ]
   
   # Plot
   p <- ggplot2::ggplot(summary_long, ggplot2::aes(x = t_start, y = value, color = factor(clone))) +
      ggplot2::geom_line(linewidth = 1.25) +
      ggplot2::scale_color_manual(values = c("#1f1fee", "#13b313")) +
      ggplot2::geom_vline(ggplot2::aes(xintercept = ced_window), linetype = "dashed") +
      ggplot2::facet_wrap(~metric, ncol = 1) +
      ggplot2::labs(title = "Balance over time",
           x = "Time",
           y = var,
           color = "Clone")

   return(p)

}