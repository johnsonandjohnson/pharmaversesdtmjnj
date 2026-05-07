#' Generates SDTM SUPPHO test data based on pharmaversesdtm::ae
#'
#' This script generates the SUPPHO (supplemental qualifiers domain for
#' Healthcare Encounters) dataset and saves it to the data folder

# Load required libraries
library(dplyr)
library(tidyr)

# Source helper functions
source("data-raw/helpers.R")

# Generate SUPPHO dataset
gen_suppho <- function() {
  gen <- pharmaversesdtm::ae |>
    dplyr::mutate(
      STUDYID,
      USUBJID,
      HOSEQ = AESEQ,
      HOINDC = AEREL,
      HOINDCO = AEREL,
      HOFREQ = NA_character_,
      HOPRACT = AEACN,
      HOPRACTO = AEACN,
      .keep = "none"
    ) |>

    tidyr::pivot_longer(
      cols = c(HOINDC, HOINDCO, HOFREQ, HOPRACT, HOPRACTO),
      names_to = "QNAM",
      values_to = "QVAL"
    ) |>

    dplyr::mutate(
      RDOMAIN = "HO",
      IDVAR = "HOSEQ",
      IDVARVAL = as.character(HOSEQ),
      QLABEL = QNAM
    ) |>

    dplyr::select(
      STUDYID, RDOMAIN, USUBJID, IDVAR, IDVARVAL, QNAM, QLABEL, QVAL
    )

  gen <- df_na(gen)

  # Add labels
  labels <- c(
    STUDYID = "Study Identifier",
    RDOMAIN = "Related Domain",
    USUBJID = "Unique Subject Identifier",
    IDVAR = "Identifying Variable",
    IDVARVAL = "Identifying Variable Value",
    QNAM = "Qualifier Variable Name",
    QLABEL = "Qualifier Variable Label",
    QVAL = "Qualifier Value"
  )

  for (name in names(labels)) {
    attr(gen[[name]], "label") <- labels[name]
  }

  return(gen)
}

# Generate the dataset
suppho <- gen_suppho()
