#' Generates SDTM ie test data
#'
#' This script generates the ie (Inclusion/Exclusion) dataset and saves it to the data folder

# Load required libraries
library(dplyr)
library(labelled)

# Source helper functions
source("data-raw/helpers.R")

# Generate ie dataset
gen_ie <- function(seed = 123) {
  set.seed(seed)

  # Get source data directly from ADSL to avoid dependency on dv
  raw <- select(pharmaverseadamjnj::adsl, STUDYID, USUBJID) |>
    distinct()

  # Sample subjects to have IE deviations
  cand_ids <- unique(raw$USUBJID)
  n_records <- 60

  gen <- tibble(
    STUDYID = raw$STUDYID[1],
    USUBJID = sample(cand_ids, n_records, replace = TRUE)
  )

  # Assign IECAT (INCLUSION/EXCLUSION)
  gen$IECAT <- sample(c("INCLUSION", "EXCLUSION"), n_records, replace = TRUE)

  # Assign IETEST
  ie_levels <- c(
    "Disease criteria",
    "Medication criteria",
    "Laboratory criteria",
    "Medical history criteria",
    "Other"
  )
  gen$IETEST <- sample(ie_levels, n_records, replace = TRUE)

  # Convert to factors
  gen$IECAT <- factor(gen$IECAT, levels = c("EXCLUSION", "INCLUSION"))
  gen$IETEST <- factor(gen$IETEST, levels = ie_levels)

  # Add labels
  additional_labels <- list(
    IECAT = "IE Category",
    IETEST = "IE Criterion Test Name"
  )

  # Handle NA values and convert characters to factors
  gen <- df_na(gen, char_as_factor = TRUE)

  # Restore labels
  gen <- restore_labels(
    df = gen,
    orig_df = raw,
    additional_labels = additional_labels
  )

  return(gen)
}

# Generate the dataset
ie <- gen_ie()
