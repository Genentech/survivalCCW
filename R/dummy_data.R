#' Dummy dataset for testing
#'
#' A dataset containing 200 patients with time-to-exposure, time-to-event, and covariates.
#'
#' @format A data frame with 200 rows and 6 variables:
#' \describe{
#'   \item{id}{Patient identifier}
#'   \item{timetoexposure}{Time to exposure, continuous, NA for non-exposed patients}
#'   \item{exposure}{Binary, 1 = exposed, 0 = not exposed}
#'   \item{event}{Event occurrence, binary, 1 = event, 0 = censored}
#'   \item{timetoevent}{Time to event, continuous, capped at 365}
#'   \item{cov1}{Binary covariate - positively associated with both time-to-exposure and time-to-event}
#'   \item{cov2}{Continuous covariate - positively associated with both time-to-exposure and time-to-event}
#' }
"dummy_data"