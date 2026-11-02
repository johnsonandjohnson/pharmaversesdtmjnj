#' Generates SDTM PR test data created from scratch
#'
#' This script generates the PR (Procedures) dataset and saves it to the data folder

library(dplyr)
library(labelled)

source("data-raw/helpers.R")

gen_pr <- function(seed = 123) {
  set.seed(seed)

  dm <- pharmaversesdtm::dm |>
    dplyr::select(STUDYID, USUBJID, RFSTDTC, RFENDTC) |>
    dplyr::filter(
      grepl("^\\d{4}-\\d{2}-\\d{2}$", RFSTDTC),
      grepl("^\\d{4}-\\d{2}-\\d{2}$", RFENDTC)
    )

  n_records <- 80

  prtrt_terms <- c(
    "Appendectomy", "Cholecystectomy", "Coronary Angioplasty",
    "Knee Arthroscopy", "Cardiac Catheterization",
    "Colonoscopy", "Upper Endoscopy", "Biopsy"
  )

  prdecod_pool <- unique(na.omit(as.character(pharmaversesdtm::cm$CMDECOD)))

  prindc_levels <- c(
    "ACC TO PROTOCOL", "ACC TO PROTOCOL 2",
    "ADVERSE EVENT", "MEDICAL HISTORY", "TRIAL INDICATION"
  )

  gen <- tibble::tibble(
    STUDYID = dm$STUDYID[1],
    USUBJID = sample(dm$USUBJID, n_records, replace = TRUE)
  ) |>
    dplyr::left_join(dm[, c("USUBJID", "RFSTDTC", "RFENDTC")], by = "USUBJID") |>
    dplyr::mutate(
      DOMAIN = "PR",
      PRTRT = sample(prtrt_terms, n_records, replace = TRUE),
      PRDECOD = sample(prdecod_pool, n_records, replace = TRUE),
      PRINDC = sample(prindc_levels, n_records, replace = TRUE),
      PREVINTX = sample(
        c("BEFORE INTO THE STUDY", "DURING STUDY", NA_character_),
        n_records,
        replace = TRUE,
        prob = c(0.3, 0.6, 0.1)
      ),
      PRSTDTC = format(
        as.Date(RFSTDTC) + floor(
          runif(n_records) * as.integer(as.Date(RFENDTC) - as.Date(RFSTDTC))
        ),
        "%Y-%m-%d"
      ),
      PRENDTC = format(
        as.Date(PRSTDTC) + sample(1:30, n_records, replace = TRUE),
        "%Y-%m-%d"
      ),
      PRDUR = paste0(
        "P",
        as.integer(as.Date(PRENDTC) - as.Date(PRSTDTC)),
        "D"
      ),
      RFSTDTC = NULL,
      RFENDTC = NULL
    ) |>
    dplyr::group_by(USUBJID) |>
    dplyr::mutate(PRSEQ = dplyr::row_number()) |>
    dplyr::ungroup() |>
    dplyr::select(
      STUDYID, DOMAIN, USUBJID, PRSEQ,
      PRTRT, PRDECOD, PRINDC, PREVINTX,
      PRSTDTC, PRENDTC, PRDUR
    ) |>
    dplyr::mutate(
      PRTRT    = factor(PRTRT),
      PRDECOD  = factor(PRDECOD),
      PRINDC   = factor(PRINDC, levels = prindc_levels),
      PREVINTX = factor(PREVINTX)
    )

  gen <- df_na(gen)

  additional_labels <- list(
    PRSEQ    = "Sequence Number",
    PRTRT    = "Therapeutic or Diagnostic Procedure",
    PRDECOD  = "Standardized Procedure Name",
    PRINDC   = "Indication",
    PREVINTX = "Prior Intervention",
    PRSTDTC  = "Start Date/Time of Procedure",
    PRENDTC  = "End Date/Time of Procedure",
    PRDUR    = "Duration of Procedure"
  )

  gen <- restore_labels(
    df = gen,
    orig_df = gen,
    additional_labels = additional_labels
  )

  attr(gen, "label") <- "Procedures"

  return(gen)
}

pr <- gen_pr()
