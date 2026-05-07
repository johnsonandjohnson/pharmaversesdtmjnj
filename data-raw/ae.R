#' Generates SDTM AE test data based on pharmaversesdtm::ae
#'
#' This script generates the AE (Adverse Events) dataset
#' and saves it to the data folder

# Load required libraries
library(dplyr)
library(labelled)

# Source helper functions
source("data-raw/helpers.R")

# Generate AE dataset
gen_ae <- function() {
  # Get source data
  raw <- pharmaversesdtm::ae

  gen <- raw |>
    dplyr::left_join(
      pharmaversesdtm::dm[, c("USUBJID", "RFSTDTC", "RFENDTC")],
      by = "USUBJID"
    ) |>
    dplyr::mutate(
      AEENRTPT = dplyr::case_when(
        AEENDTC < RFSTDTC ~ "PRE-TREATMENT",
        AEENDTC >= RFSTDTC & AEENDTC <= RFENDTC ~ "ON-TREATMENT",
        AEENDTC > RFENDTC ~ "POST-TREATMENT",
        .default = NA_character_
      ),
      RFSTDTC = NULL,
      RFENDTC = NULL
    )

  gen <- df_na(gen)

  # Add labels
  additional_labels <- list(
    AEENRTPT = "End Relative to Reference Time Point"
  )

  # Restore labels
  gen <- restore_labels(
    df = gen,
    orig_df = raw,
    additional_labels = additional_labels
  )

  return(gen)
}

# Generate the dataset
ae <- gen_ae()
