#' Cast one-row-per-clone data to long format
#' 
#' @param df A data.frame with one row per clone as returned by [create_clones()]
#' 
#' @return A data.frame with one row per patient per time period per clone.
#' 
#' @export
#' 
#' @examples 
#' 
#' # Load the toy dataset
#' data(dummy_data)
#' 
#' # Create clones
#' clones <- create_clones(dummy_data, 
#'                         id = "id", 
#'                         event = "event", 
#'                         time_to_event = "timetoevent", 
#'                         exposure = "exposure", 
#'                         time_to_exposure = "timetoexposure", 
#'                         ced_window = 100)
#' 
#' clones_long <- cast_clones_to_long(clones)
cast_clones_to_long <- function(df) {

   # Check inputs
   checkmate::assert_class(df, "ccw_clones")
   if (!all(c("outcome", "fup_outcome", "censor", "fup_censor", "clone") %in% names(df))) {
      stop("The input data.frame is missing at least one of the required columns: outcome, fup_outcome, censor, fup_censor, clone. Did you remove this?")
   }
   if (!all(c("id", "event", "time_to_event", "exposure", "time_to_exposure", "ced_window") %in% names(attributes(df)))) {
      stop("The input data.frame is missing at least one attribute: id, event, time_to_event, exposure, time_to_exposure, ced_window. Did you remove these or try to make a custom data.frame?")
   }

   id <- attributes(df)$id
   event <- attributes(df)$event
   exposure <- attributes(df)$exposure
   time_to_event <- attributes(df)$time_to_event
   time_to_exposure <- attributes(df)$time_to_exposure
   ced_window <- attributes(df)$ced_window

   # Now convert to long
   event_times <- sort(unique(c(df[,time_to_event], df[,time_to_exposure], ced_window)))
   event_times_df <- data.frame(
      t_event = event_times,
      time_id = 1:NROW(event_times)
   )

   # Exposed
   ## Outcome
   df_1 <- df[df[, "clone"] == 1L, ]
   
   df_1$t_start <- 0.

   ## To accomodate multiple outcomes (eg competing risks), do this iteratively
   df_1_long_outcome_list <- vector("list", length = NROW(unique(df_1$outcome)))

   for (i in 1:NROW(unique(df_1$outcome))) {
      df_outcome <- df_1[df_1$outcome == unique(df_1$outcome)[i], ]
      df_outcome$outcome <- 1L
      df_1_long_outcome_list[[i]] <- survival::survSplit(
         data = df_outcome,
         cut = event_times, 
         end = "fup_outcome",
         start = "t_start",
         event = "outcome"
      )
      df_1_long_outcome_list[[i]]$outcome <- ifelse(df_1_long_outcome_list[[i]]$outcome == 1L, unique(df_1$outcome)[i], 0L)
   }
   df_1_long_outcome <- do.call(rbind, df_1_long_outcome_list)

   df_1_long_outcome <- df_1_long_outcome[order(df_1_long_outcome[, id], df_1_long_outcome[, "fup_outcome"]), ]

   ## Censor
   df_1_long_censor <- survival::survSplit(
      df_1, 
      cut = event_times, 
      end = "fup_outcome", # Not a typo, we want to expand at the outcome time still
      start = "t_start",
      event = "censor"
   )

   df_1_long_censor <- df_1_long_censor[order(df_1_long_censor[, id], df_1_long_censor[, "fup_outcome"]), ]
   
   ## Replace censoring variable in df_1_long_outcome
   df_1_long_outcome$censor <- df_1_long_censor$censor

   df_1_long_outcome$t_stop <- df_1_long_outcome$fup_outcome

   df_1_long <- merge(
      x = df_1_long_outcome,
      y = event_times_df, 
      by.x = "t_start",
      by.y = "t_event",
      all.x = TRUE)

   df_1_long <- df_1_long[order(df_1_long[,id], df_1_long[, "fup_outcome"]), ]
   df_1_long$time_id[is.na(df_1_long$time_id)] <- 0
   rownames(df_1_long) <- NULL

   # Unexposed
   ## Outcome
   df_0 <- df[df[, "clone"] == 0L, ]
   
   df_0$t_start <- 0.

   ## To accomodate multiple outcomes (eg competing risks), do this iteratively
   df_0_long_outcome_list <- vector("list", length = NROW(unique(df_0$outcome)))

   for (i in 1:NROW(unique(df_0$outcome))) {
      df_outcome <- df_0[df_0$outcome == unique(df_0$outcome)[i], ]
      df_outcome$outcome <- 1L
      df_0_long_outcome_list[[i]] <- survival::survSplit(
         data = df_outcome,
         cut = event_times, 
         end = "fup_outcome",
         start = "t_start",
         event = "outcome"
      )
      df_0_long_outcome_list[[i]]$outcome <- ifelse(df_0_long_outcome_list[[i]]$outcome == 1L, unique(df_0$outcome)[i], 0L)
   }
   df_0_long_outcome <- do.call(rbind, df_0_long_outcome_list)

   df_0_long_outcome <- df_0_long_outcome[order(df_0_long_outcome[, id], df_0_long_outcome[, "fup_outcome"]), ]

   ## Censor
   df_0_long_censor <- survival::survSplit(
      df_0, 
      cut = event_times, 
      end = "fup_outcome", # Not a typo, we want to expand at the outcome time still
      start = "t_start",
      event = "censor"
   )

   df_0_long_censor <- df_0_long_censor[order(df_0_long_censor[, id], df_0_long_censor[, "fup_outcome"]), ]
   
   ## Replace censoring variable in df_1_long_outcome
   df_0_long_outcome$censor <- df_0_long_censor$censor

   df_0_long_outcome$t_stop <- df_0_long_outcome$fup_outcome

   df_0_long <- merge(
      x = df_0_long_outcome,
      y = event_times_df, 
      by.x = "t_start",
      by.y = "t_event",
      all.x = TRUE)

   df_0_long <- df_0_long[order(df_0_long[,id], df_0_long[, "fup_outcome"]), ]
   df_0_long$time_id[is.na(df_0_long$time_id)] <- 0
   rownames(df_0_long) <- NULL 

   # Combine
   df_long <- rbind(
      df_1_long,
      df_0_long
   )

   df_long <- merge( 
      x = df_long,
      y = event_times_df,
      by = "time_id",
      all.x = TRUE
   )
   
   df_long <- df_long[order(df_long[,id], df_long[, "clone"], df_long[, "fup_outcome"]), ]

   # Add attributes and return
   class(df_long) <- c("ccw_clones_long", class(df_long))
   attributes(df_long)$id <- id
   attributes(df_long)$event <- event
   attributes(df_long)$time_to_event <- time_to_event
   attributes(df_long)$exposure <- exposure
   attributes(df_long)$time_to_exposure <- time_to_exposure
   attributes(df_long)$ced_window <- ced_window
   attributes(df_long)$event_times_df <- event_times_df

   # Remove rownames
   rownames(df_long) <- NULL

   return(df_long)
}
