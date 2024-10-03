#' Check inputs to create clones
#' 
#' @param df A data.frame with one row per patient.
#' @param id The name of the column in `df` that contains the patient identifier.
#' @param event The name of the column in `df` that contains the event of interest.
#' @param time_to_event The name of the column in `df` that contains the time to event.
#' @param exposure The name of the column in `df` that contains the exposure.
#' @param time_to_exposure The name of the column in `df` that contains the time to exposure.
#' @param ced_window The date at which the clinical eligibility window closes. 
#' `exposure` and `time_to_exposure`
#' 
#' @return TRUE if inputs are valid else false
create_clones_check_inputs <- function(
   df, 
   id,
   event,
   time_to_event,
   exposure,
   time_to_exposure,
   ced_window
) {

   inputs_good <- FALSE 

   # Check all input types 
   checkmate::assert_class(df, "data.frame")
   checkmate::assert_class(id, "character")
   checkmate::assert_class(event, "character")
   checkmate::assert_class(time_to_event, "character")
   checkmate::assert_class(exposure, "character")
   checkmate::assert_class(time_to_exposure, "character")
   checkmate::assert_class(ced_window, "numeric")

   # Check that all columns are in data
   checkmate::assert_subset(c(id, event, time_to_event, exposure, time_to_exposure), names(df))

   # Check that there are no missing data in the study columns (except time to exposure)
   cc_sum <- sum(stats::complete.cases(df[, c(id, event, time_to_event, exposure)]))
   if (cc_sum != NROW(df)) {
      stop(paste0("There are missing data in one of the study columns: ", id, ", ", event, ", ", time_to_event, ", ", exposure))
   }

   # Check time to exposure is missing just for when exposure is 0/F
   if (any(!is.na(df[df[, exposure] == 0L, time_to_exposure]))) {
      stop("Time to exposure should only be for patients who received the exposure at some time")
   }

   if (any(is.na(df[df[, exposure] == 1L, time_to_exposure]))) {
      stop("Time to exposure should be complete for patients who have exposure = 1")
   }

   # Check exposure is just 0/1 or T/F
   if (any(df[, exposure] != 0L & df[, exposure] != 1L)) {
      stop("Exposure should be 0/1 or T/F")
   }

   # Make sure the user did not pass the same column name twice
   if (NROW(unique(c(id, event, time_to_event, exposure, time_to_exposure))) != NROW(c(id, event, time_to_event, exposure, time_to_exposure))) {
      stop("You passed the same column name twice")
   }

   # Check that the respective columns are numeric and appropriately ranged
   checkmate::assert_numeric(df[, time_to_event])
   checkmate::assert_numeric(df[, time_to_exposure])
   checkmate::assert_true(class(df[,event]) %in% c("integer", "logical"))
   checkmate::assert_true(class(df[,exposure]) %in% c("integer", "logical"))
   checkmate::assert_true(class(df[,id]) %in% c("integer", "numeric", "character"))
   if (any(df[, time_to_event] < 0)) {
      stop("Time to event cannot be negative")
   }
   if (any(df[, time_to_exposure] < 0, na.rm = TRUE)) {
      stop("Time to exposure cannot be negative")
   }

   # Some protected names
   protected_names <- c("clone", "outcome", "fup_outcome", "censor", "fup_censor", 
                        "t_start", "t_stop", "time_id", "t_event", "weight_cox",
                        "p_uncens", "hazard", "lp")
   for (name in protected_names) {
      if (name %in% names(df)) {
         stop("'", name, "' is a protected column name and will be used by the function. Please rename this column")
      }
   }

   # Check no outcomes are before exposure dates
   if (any(df[!is.na(df[,time_to_exposure]), time_to_event] < df[!is.na(df[,time_to_exposure]), time_to_exposure])) {
      stop("There are outcomes before exposure dates")
   }

   inputs_good <- TRUE 
   
   return(inputs_good)

}

