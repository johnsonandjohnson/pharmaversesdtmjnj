#' Generates SDTM SUPPPC test data created from scratch
#'
#' This script generates the SUPPPC (Supplemental Pharmacokinetic Concentrations)
#' dataset and saves it to the data folder

library(dplyr)
library(tidyr)
library(labelled)

source("data-raw/helpers.R")

gen_supppc <- function(seed = 123) {
  set.seed(seed)

  reason_pool <- c(
    "SAMPLE LOST",
    "INSUFFICIENT SAMPLE",
    "SAMPLE HEMOLYZED",
    "SAMPLE NOT COLLECTED",
    "BELOW LIMIT OF QUANTIFICATION",
    NA_character_
  )

  gen <- pharmaversesdtm::pc |>
    dplyr::mutate(across(where(is.factor), as.character)) |>
    dplyr::select(STUDYID, USUBJID, PCSEQ) |>
    dplyr::mutate(
      RDOMAIN = "PC",
      IDVAR = "PCSEQ",
      IDVARVAL = as.character(PCSEQ),
      QNAM = "PCREASNC",
      QLABEL = "Reason for Exclusion",
      QVAL = sample(reason_pool, dplyr::n(),
        replace = TRUE,
        prob = c(0.2, 0.2, 0.15, 0.15, 0.2, 0.1)
      )
    ) |>
    dplyr::select(STUDYID, RDOMAIN, USUBJID, IDVAR, IDVARVAL, QNAM, QLABEL, QVAL)

  gen <- df_na(gen)

  additional_labels <- list(
    STUDYID  = "Study Identifier",
    RDOMAIN  = "Related Domain Abbreviation",
    USUBJID  = "Unique Subject Identifier",
    IDVAR    = "Identifying Variable",
    IDVARVAL = "Identifying Variable Value",
    QNAM     = "Qualifier Variable Name",
    QLABEL   = "Qualifier Variable Label",
    QVAL     = "Qualifier Value"
  )

  gen <- restore_labels(
    df = gen,
    orig_df = gen,
    additional_labels = additional_labels
  )

  attr(gen, "label") <- "Suppl. Qualifiers for PK Concentrations"

  return(gen)
}

# Generate the dataset
supppc <- gen_supppc()
