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
  visit_names <- c("SCREENING", "BASELINE", "WEEK 4", "WEEK 8")
  visit_nums <- c(1, 2, 3, 4)
  visit_idx <- sample(seq_along(visit_names), n_records, replace = TRUE)
  gen$VISITNUM <- visit_nums[visit_idx]
  gen$VISIT <- visit_names[visit_idx]

  # Assign IECAT (INCLUSION/EXCLUSION)
  gen$IECAT <- sample(c("INCLUSION", "EXCLUSION"), n_records, replace = TRUE)

  # Assign IETEST
  ie_levels <- c(
    "Disease criteria",
    "Medication criteria",
    "Laboratory criteria",
    "Medical history criteria",
    "Other"
  )
  gen$IETEST <- sample(ie_levels, n_records, replace = TRUE)

  # Derive IETESTCD from IETEST
  ie_codes <- c("DISEASE", "MED", "LAB", "MH", "OTHER")
  gen$IETESTCD <- ie_codes[match(gen$IETEST, ie_levels)]

  # Assign IEORRES/IESTRESC
  gen$IEORRES <- rep("NOT MET", n_records)
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


  gen$IECAT <- factor(gen$IECAT, levels = c("EXCLUSION", "INCLUSION"))
  gen$IETEST <- factor(gen$IETEST, levels = ie_levels)
  gen$IETESTCD <- factor(gen$IETESTCD, levels = ie_codes)
  gen$VISIT <- factor(gen$VISIT, levels = visit_names)

  # Add labels
  additional_labels <- list(
    IECAT = "IE Category",
    IETEST = "IE Criterion Test Name",
    IETESTCD = "IE Criterion Short Name",
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
