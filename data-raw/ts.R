#' Generates SDTM TS test data based on pharmaversesdtm::ts
#'
#' This script generates the TS (Trial Design) dataset
#' and saves it to the data folder

# Load required libraries
library(dplyr)
library(labelled)

# Source helper functions
source("data-raw/helpers.R")

# Generate TS dataset
gen_ts <- function() {
  # Get source data
  raw <- pharmaversesdtm::ts

  gen <- raw |>
    dplyr::mutate(
      # Fix special symbols
      TSVAL = iconv(TSVAL, from = "WINDOWS-1252", to = "UTF-8"),
      TSVAL = gsub("\u2019", "'", TSVAL)
    )

  gen <- df_na(gen)

  # Restore labels
  gen <- restore_labels(
    df = gen,
    orig_df = raw
  )

  return(gen)
}

# Generate the dataset
ts <- gen_ts()
