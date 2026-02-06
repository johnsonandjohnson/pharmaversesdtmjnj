#' Generates SDTM DV test data based on random.cdisc.data::raddv
#'
#' This script generates the DV (Deviations) dataset and saves it to the data folder

# Load required libraries
library(dplyr)
library(labelled)

# Source helper functions
source("data-raw/helpers.R")


# Generate DV dataset
gen_dv <- function() {
  set.seed(123)

  # Get source data
  raw <- dplyr::filter(
    dplyr::select(
      pharmaverseadamjnj::adsl,
      STUDYID, USUBJID, TRTSDT, TRTSDTM, TRTEDTM
    ),
    !is.na(TRTSDTM), !is.na(TRTEDTM)
  )

  gen <- df_na(raw)

  attr(gen, "study_duration_secs") <- 365 * 2
  gen <- random.cdisc.data::raddv(gen, seed = 2)

  gen$DVSTDTC <- gen$TRTSDT + sample.int(7, nrow(gen), replace = TRUE)

  # Restore labels
  gen <- restore_labels(
    df = gen,
    orig_df = raw
  )

  return(gen)
}

# Generate the dataset
dv <- gen_dv()