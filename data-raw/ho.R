#' Generates SDTM HO test data based on pharmaversesdtm::dm
#' and pharmaversesdtm::ae
#'
#' This script generates the HO (Healthcare Encounters) dataset
#' and saves it to the data folder

# Load required libraries
library(dplyr)
library(labelled)

# Source helper functions
source("data-raw/helpers.R")

# Generate HO dataset
gen_ho <- function() {
  dm <- pharmaversesdtm::dm |>
    dplyr::select(STUDYID, USUBJID, RFSTDTC) |>
    dplyr::mutate(
      RFSTDTC = dplyr::if_else(
        grepl("^\\d{4}-\\d{2}-\\d{2}$", RFSTDTC),
        RFSTDTC,
        NA_character_
      )
    ) |>
    dplyr::distinct()

  ae <- pharmaversesdtm::ae |>
    dplyr::mutate(
      dplyr::across(
        c(AESTDTC, AEDTC, AEENDTC),
        \(date) {
          dplyr::if_else(
            grepl("^\\d{4}-\\d{2}-\\d{2}$", date),
            date,
            NA_character_
          )
        }
      )
    )

  gen <- dplyr::left_join(ae, dm, by = c("STUDYID", "USUBJID")) |>
    dplyr::mutate(
      STUDYID,
      USUBJID,
      DOMAIN = "HO",
      HOSEQ = AESEQ,
      HOTERM = dplyr::coalesce(AEDECOD, AETERM),
      HOSTDTC = dplyr::coalesce(AESTDTC, AEDTC),
      HOSTDY = as.integer(difftime(HOSTDTC, RFSTDTC, units = "days")) + 1L,
      HOENDTC = AEENDTC,
      HOENDY = as.integer(difftime(HOENDTC, RFSTDTC, units = "days")) + 1L,
      HOENRTPT = AEOUT,
      HOENRF = AEREL,
      HODUR = as.integer(difftime(HOENDTC, HOSTDTC, units = "days")) + 1L,
      HOOCCUR = dplyr::if_else(is.na(HOENDTC), "Y", "N"),
      .keep = "none"
    )

  gen <- df_na(gen)

  # Add labels
  labels <- c(
    STUDYID = "Study Identifier",
    USUBJID = "Unique Subject Identifier",
    DOMAIN = "Domain Abbreviation",
    HOSEQ = "Sequence Number",
    HOTERM = "Healthcare Encounter Term",
    HOSTDTC = "Start Date/Time of Healthcare Encounter",
    HOSTDY = "Study Day of Start of Encounter",
    HOENDTC = "End Date/Time of Healthcare Encounter",
    HOENDY = "Study Day of End of Healthcare Encounter",
    HOENRTPT = "End Relative to Reference Time Point",
    HOENRF = "End Relative to Reference Period",
    HODUR = "Duration of Healthcare Encounter",
    HOOCCUR = "Healthcare Encounter Occurrence"
  )

  for (name in names(labels)) {
    attr(gen[[name]], "label") <- labels[name]
  }

  return(gen)
}

# Generate the dataset
ho <- gen_ho()
