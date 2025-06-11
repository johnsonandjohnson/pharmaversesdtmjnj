#' Generates SDTM MH test data based on pharmaversesdtm::mh
#'
#' This script generates the MH (Medical History) dataset and saves it to the data folder

# Load required libraries
library(dplyr)
library(labelled)

# Source helper functions
source("data-raw/helpers.R")

# Generate MH dataset
gen_mh <- function() {
  #Get source data
  raw <- pharmaversesdtm::mh

  gen <- dplyr::mutate(
    raw,
    MHTOXGR = factor(
      sample(
        c(1:5, NA),
        size = dplyr::n(),
        replace = TRUE,
        prob = c(rep(0.2, 5), 0.1)
      )
    )
  )
  gen <- df_na(gen)

  # Add labels
  additional_labels <- list(
    MHTOXGR = "Standard Toxicity Grade"
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
mh <- gen_mh()
