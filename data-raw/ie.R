#' Generates SDTM ie test data
#'
#' This script generates the ie (Inclusion/Exclusion) dataset and saves it to the data folder

# Load required libraries
library(dplyr)
library(labelled)

# Source helper functions
source("data-raw/helpers.R")

# Generate ie dataset
gen_ie <- function(seed = 123) {
  set.seed(seed)

  # Get source data from SDTM DM
  raw <- dplyr::select(pharmaversesdtm::dm, STUDYID, USUBJID) |>
    dplyr::distinct()

  # Sample subjects to have IE deviations
  cand_ids <- unique(raw$USUBJID)
  n_records <- 60

  gen <- tibble(
    STUDYID = raw$STUDYID[1],
    USUBJID = sample(cand_ids, n_records, replace = TRUE)
  )

  # Assign VISIT/VISITNUM
  visit_names <- c("SCREENING")
  gen$VISITNUM <- 1
  gen$VISIT <- "SCREENING"

  # Assign IECAT (INCLUSION/EXCLUSION)
  gen$IECAT <- sample(c("INCLUSION", "EXCLUSION"), n_records, replace = TRUE)

  # Assign IETEST and IETESTCD based on IECAT
  inc_levels <- c("Disease criteria", "Medication criteria", "Laboratory criteria")
  exc_levels <- c("Medical history criteria", "Other")
  inc_codes  <- c("INC01", "INC02", "INC03")
  exc_codes  <- c("EXC01", "EXC02")

  ie_levels <- c(inc_levels, exc_levels)
  ie_codes  <- c(inc_codes,  exc_codes)

  gen$IETEST <- ifelse(
    gen$IECAT == "INCLUSION",
    sample(inc_levels, n_records, replace = TRUE),
    sample(exc_levels, n_records, replace = TRUE)
  )
  gen$IETESTCD <- ie_codes[match(gen$IETEST, ie_levels)]

  # Assign IEORRES/IESTRESC
  gen$IEORRES <- ifelse(gen$IECAT == "INCLUSION", "N", "Y")
  gen$IESTRESC <- gen$IEORRES


  gen <- gen |>
    dplyr::left_join(
      pharmaversesdtm::dm[, c("USUBJID", "RFSTDTC")],
      by = "USUBJID"
    ) |>
    dplyr::mutate(
      IEDTC = format(as.Date(RFSTDTC) - sample.int(14, dplyr::n(), replace = TRUE), "%Y-%m-%d"),
      RFSTDTC = NULL
    )


  gen$IECAT    <- factor(gen$IECAT,    levels = c("EXCLUSION", "INCLUSION"))
  gen$IETEST   <- factor(gen$IETEST,   levels = ie_levels)
  gen$IETESTCD <- factor(gen$IETESTCD, levels = ie_codes)
  gen$VISIT <- factor(gen$VISIT, levels = visit_names)

  # Add labels
  additional_labels <- list(
    IECAT = "Inclusion/Exclusion Category",
    IETEST = "Inclusion/Exclusion Criterion Test Name",
    IETESTCD = "Inclusion/Exclusion Criterion Short Name",
    IEORRES = "Result or Finding in Original Units",
    IESTRESC = "Character Result/Finding in Standard Format",
    VISIT = "Visit Name",
    VISITNUM = "Visit Number",
    IEDTC = "Date/Time of Collection"
  )

  # Handle NA values and convert characters to factors
  gen <- df_na(gen, char_as_factor = TRUE)

  # Restore labels
  gen <- restore_labels(
    df = gen,
    orig_df = raw,
    additional_labels = additional_labels
  )

  attr(gen, "label") <- "Inclusion/Exclusion Criteria Not Met"

  return(gen)
}

# Generate the dataset
ie <- gen_ie()
