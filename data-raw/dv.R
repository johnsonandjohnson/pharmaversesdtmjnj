#' Generates SDTM DV test data based on pharmaversesdtm::dm
#'
#' This script generates the DV (Protocol Deviations) dataset and saves it to the data folder

# Load required libraries
library(dplyr)
library(labelled)

# Source helper functions
source("data-raw/helpers.R")


# Generate DV dataset
gen_dv <- function(seed = 123) {
  set.seed(seed)

  # Get subjects with a full treatment period from DM
  raw <- pharmaversesdtm::dm |>
    dplyr::select(STUDYID, USUBJID, RFSTDTC, RFENDTC) |>
    dplyr::filter(
      grepl("^\\d{4}-\\d{2}-\\d{2}$", RFSTDTC),
      grepl("^\\d{4}-\\d{2}-\\d{2}$", RFENDTC)
    )

  n_records <- 75

  gen <- tibble(
    STUDYID = raw$STUDYID[1],
    USUBJID = sample(raw$USUBJID, n_records, replace = TRUE)
  ) |>
    dplyr::left_join(raw[, c("USUBJID", "RFSTDTC", "RFENDTC")], by = "USUBJID") |>
    dplyr::mutate(
      DOMAIN = "DV",
      DVSTDTC = format(
        as.Date(RFSTDTC) + floor(
          runif(dplyr::n()) * as.integer(as.Date(RFENDTC) - as.Date(RFSTDTC))
        ),
        "%Y-%m-%d"
      ),
      RFSTDTC = NULL,
      RFENDTC = NULL
    )

  gen$DVSEQ <- seq_len(n_records)

  mock_terms <- c(
    "Developed withdrawal criteria but not withdrawn",
    "Entered but did not satisfy criteria",
    "Received a disallowed concomitant treatment",
    "Received wrong treatment or incorrect dose",
    "Other"
  )

  gen$DVTERM <- sample(mock_terms, n_records, replace = TRUE)
  gen$DVDECOD <- gen$DVTERM
  gen$DVCAT <- factor("MAJOR")

  gen <- gen |>
    dplyr::select(STUDYID, USUBJID, DOMAIN, DVSEQ, DVTERM, DVDECOD, DVCAT, DVSTDTC) |>
    dplyr::mutate(
      DVTERM  = factor(DVTERM, levels = mock_terms),
      DVDECOD = factor(DVDECOD, levels = mock_terms)
    )

  gen <- df_na(gen)

  additional_labels <- list(
    DOMAIN  = "Domain Abbreviation",
    DVSEQ   = "Sequence Number",
    DVTERM  = "Protocol Deviation Term",
    DVDECOD = "Standardized Deviation Term",
    DVCAT   = "Protocol Deviation Category",
    DVSTDTC = "Start Date/Time of Protocol Deviation"
  )

  gen <- restore_labels(
    df = gen,
    orig_df = raw,
    additional_labels = additional_labels
  )

  attr(gen, "label") <- "Protocol Deviations"

  return(gen)
}

# Generate the dataset
dv <- gen_dv()
