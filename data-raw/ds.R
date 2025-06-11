#' Generates SDTM DS test data based on pharmaversesdtm::ds
#'
#' This script generates the DS (Disposition) dataset and saves it to the data folder

# Load required libraries
library(dplyr)
library(labelled)

# Source helper functions
source("data-raw/helpers.R")

# Generate DS dataset
gen_ds <- function() {
  # Get source data
  raw <- pharmaversesdtm::ds
  gen <- dplyr::mutate(raw, DSSCAT = factor("TREATMENT"))
  gen <- df_na(gen)

  # Add labels
  additional_labels <- list(
    DSSCAT = "Subcategory for Disposition Event"
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
ds <- gen_ds()
