# Declare global variables
utils::globalVariables(c("type", "value", "t", "ced_window", "N", "clone"))

#' Plot censoring over time
#' 
#' @param df A data.frame with one row per clone per observation period that contains weights for each patient-period as returned by [create_clones()]
#' @return a ggplot2 object
#' 
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
#' plot_censoring_over_time(clones)
plot_censoring_over_time <- function(df) {

   # Confirm ggplot2 loaded
   if (!requireNamespace("ggplot2", quietly = TRUE)) {
      stop("ggplot2 package is required for this function. Install w/ `install.packages('ggplot2')")
   }

   # Just for binary outcomes
   if (!all(unique(df$outcome) %in% 0:1)) {
      stop("This function only works for binary outcomes")
   }

   # Confirm inputs are correct
   checkmate::assert_class(df, "ccw_clones")

   # Import attributes
   id <- attributes(df)$id
   event <- attributes(df)$event
   exposure <- attributes(df)$exposure
   time_to_event <- attributes(df)$time_to_event
   time_to_exposure <- attributes(df)$time_to_exposure
   ced_window <- attributes(df)$ced_window

   # Get eventtimes
   event_times <- sort(unique(c(df[,time_to_event], df[,time_to_exposure], ced_window)))
   
   # Get total N
   N <- NROW(df)/2

   # Stacked df
   df_stack <- data.frame(
      clone = c(0, 1),
      t = c(0, 0), 
      cumu_uncensored = c(N, N),
      cumu_admin_censored = c(0, 0),
      cumu_true_censored_or_event = c(0, 0)
   )

   # Now iterate over event_times and add to df_stack
   ## @TODO speed up and remove loop
   for (time in event_times) {

      # Deviation from treatment strategy
      ac1 <- NROW(df[df$clone == 1 & df$censor == 1 & df$fup_censor <= time, ])
      ac0 <- NROW(df[df$clone == 0 & df$censor == 1 & df$fup_censor <= time, ])

      # True censored or event
      tc1 <- NROW(df[df$clone == 1 & df$fup_outcome <= time & (df$outcome == 1 | (df$outcome == 0 & df$fup_outcome > df$fup_censor)), ])
      tc0 <- NROW(df[df$clone == 0 & df$fup_outcome <= time & (df$outcome == 1 | (df$outcome == 0 & df$fup_outcome > df$fup_censor)), ])

      # Uncensored
      uc1 <- N - ac1 - tc1
      uc0 <- N - ac0 - tc0

      # Add to df_stack
      df_stack <- rbind(
         df_stack,
         data.frame(
            clone = c(0, 1),
            t = c(time, time),
            cumu_uncensored = c(uc0, uc1),
            cumu_admin_censored = c(ac0, ac1),
            cumu_true_censored_or_event = c(tc0, tc1)
         )
      )
   }

   # Cast to long
   df_stack_long <- reshape(
      df_stack, 
      varying = list(c("cumu_admin_censored", "cumu_true_censored_or_event")),
      v.names = "value",
      timevar = "type",
      times = c("Treatment strategy deviation", "Loss to follow-up or event"),
      direction = "long"
   )

   # Plot
   df_stack_long |>
      ggplot2::ggplot() + 
      ggplot2::geom_step(
         ggplot2::aes(x = t, y = value, color = type), 
         position = "stack", 
         direction = "hv",
         linewidth = 2
      ) + 
      ggplot2::scale_color_manual(values = c("#1f1fee", "#13b313")) +
      ggplot2::annotate(
         "segment",
         x = ced_window, xend = ced_window, y = 0, yend = N, 
         linetype = 2
      ) +
      ggplot2::geom_hline(ggplot2::aes(yintercept = N), linetype = 2) +
      ggplot2::facet_wrap(~ paste0("Clone: ", clone)) + 
      ggplot2::labs(
         x = "Time",
         y = "Cumulative N censored",
         color = "Censorship type"
      )

}