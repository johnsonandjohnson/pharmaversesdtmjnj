#' Generates SDTM SUPPPR test data created from scratch
#'
#' This script generates the SUPPPR (Supplemental Procedures) dataset

library(dplyr)
library(tidyr)
library(labelled)

source("data-raw/helpers.R")

gen_supppr <- function(seed = 457) {
  set.seed(seed)

  # Load pr
  source("data-raw/pr.R")

  findings_pool <- c(
    "No abnormality detected",
    "Mild inflammation observed",
    "Benign polyp removed",
    "Normal tissue architecture",
    NA_character_
  )

  qnam_labels <- c(
    PRPLN    = "Procedure Elective",
    PRFIND   = "Diagnostic Procedure Findings",
    PRAEFIND = "Findings Adverse Event"
  )

  gen <- pr |>
    dplyr::mutate(across(where(is.factor), as.character)) |>
    dplyr::select(STUDYID, USUBJID, PRSEQ) |>
    dplyr::mutate(
      RDOMAIN  = "PR",
      IDVAR    = "PRSEQ",
      IDVARVAL = as.character(PRSEQ),
      PRPLN    = sample(c("Y", "N"), dplyr::n(), replace = TRUE, prob = c(0.4, 0.6)),
      PRFIND   = sample(findings_pool, dplyr::n(), replace = TRUE),
      PRAEFIND = sample(c("Y", "N"), dplyr::n(), replace = TRUE, prob = c(0.3, 0.7))
    ) |>
    tidyr::pivot_longer(
      cols      = c(PRPLN, PRFIND, PRAEFIND),
      names_to  = "QNAM",
      values_to = "QVAL"
    ) |>
    dplyr::mutate(
      QLABEL = qnam_labels[QNAM]
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

  attr(gen, "label") <- "Supplemental Qualifiers for Procedures"

  return(gen)
}

supppr <- gen_supppr()
