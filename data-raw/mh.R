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
  # Get source data
  raw <- pharmaversesdtm::mh

  gen <- dplyr::mutate(
    raw,
    MHOCCUR = "Y",
    MHTOXGR = factor(
      sample(
        c(1:5, NA),
        size = dplyr::n(),
        replace = TRUE,
        prob = c(rep(0.2, 5), 0.1)
      )
    )
  )

  gen <- gen |>
    dplyr::left_join(
      pharmaversesdtm::dm[, c("USUBJID", "RFSTDTC")],
      by = "USUBJID"
    ) |>
    dplyr::mutate(
      MHSTDY = as.integer(difftime(
        dplyr::if_else(grepl("^\\d{4}-\\d{2}-\\d{2}$", MHSTDTC), MHSTDTC, NA),
        RFSTDTC,
        units = "days"
      )),
      MHENDY = as.integer(substr(MHENDTC, 1, 4)),
      RFSTDTC = NULL
    )

  gen <- gen |>
    dplyr::mutate(
      MHCAT = sample(
        c("PRIMARY DIAGNOSIS", "SIGNIFICANT PRE-EXISTING CONDITION", "HISTORICAL DIAGNOSIS", "GENERAL MEDICAL HISTORY", "MEDICAL HISTORY OF INTEREST"),
        size = dplyr::n(),
        replace = TRUE
      )
    )

  gen <- df_na(gen)

  # Add labels
  additional_labels <- list(
    MHCAT = "Category for Medical History",
    MHTOXGR = "Standard Toxicity Grade",
    MHSTDY = "Study Day of Start of Medical History",
    MHENDY = "Study Day of End of Medical History"
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
