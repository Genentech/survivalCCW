set.seed(123)  # For reproducibility

n <- 200  # Number of patients

# Patient IDs
id <- paste0("pat", sample(1000:9999, n, replace = TRUE))

# Covariates
cov1 <- rbinom(n, 1, 0.5)  # Binary covariate, independent of cov2
cov2 <- rnorm(n, mean = 50, sd = 10)  # Continuous covariate, independent of cov1

# Time to event (continuous), can be any time between 1 and 365
timetoevent <- round(runif(n, min = 1, max = 365) + cov2 * 0.2)

# Set exposure to be approximately 50/50
exposure <- as.integer(rbinom(n, 1, 0.5))

# Time to exposure (continuous), generated only for exposed patients, capped at 100
timetoexposure <- ifelse(exposure == 1, round(runif(n, min = 1, max = 100) + cov2 * 0.1), NA)

# Ensure that timetoevent is always greater than timetoexposure for exposed patients
timetoevent <- ifelse(!is.na(timetoexposure) & timetoevent <= timetoexposure, timetoexposure + 1, timetoevent)

# Random censoring for 50/50 uncensored/censored ratio
censoring_indicator <- rbinom(n, 1, 0.5)  # 50% censoring
event <- ifelse(censoring_indicator == 1 & timetoevent <= 365, 1L, 0L)  # Censoring applied
timetoevent <- ifelse(event == 0, pmin(timetoevent, 365), timetoevent)  # Cap timetoevent at 365 for censored

# Create the dataset
dummy_data <- data.frame(id, timetoexposure, exposure, event, timetoevent, cov1, cov2)
