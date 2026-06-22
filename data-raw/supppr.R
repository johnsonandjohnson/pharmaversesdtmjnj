#' Generates SDTM SUPPPR test data created from scratch
#'
#' This script generates the SUPPPR (Supplemental Procedures) dataset

library(dplyr)
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

  gen <- pr |>
    dplyr::mutate(across(where(is.factor), as.character)) |>
    dplyr::select(STUDYID, USUBJID, PRSEQ) |>
    dplyr::mutate(
      RDOMAIN = "PR",
      IDVAR = "PRSEQ",
      IDVARVAL = as.character(PRSEQ),
      PRELEC = sample(c("Y", "N"), dplyr::n(), replace = TRUE, prob = c(0.4, 0.6)),
      PRFIND = sample(findings_pool, dplyr::n(), replace = TRUE)
    ) |>
    dplyr::select(STUDYID, RDOMAIN, USUBJID, IDVAR, IDVARVAL, PRELEC, PRFIND) |>
    dplyr::mutate(
      PRELEC = factor(PRELEC),
      PRFIND = factor(PRFIND)
    )

  gen <- df_na(gen)

  additional_labels <- list(
    STUDYID  = "Study Identifier",
    RDOMAIN  = "Related Domain Abbreviation",
    USUBJID  = "Unique Subject Identifier",
    IDVAR    = "Identifying Variable",
    IDVARVAL = "Identifying Variable Value",
    PRELEC   = "Was this procedure elective?",
    PRFIND   = "Diagnostic Procedure Findings"
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
