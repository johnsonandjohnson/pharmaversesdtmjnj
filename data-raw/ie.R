#' Generates SDTM ie test data based on random.cdisc.data::radie
#'
#' This script generates the ie (Inclusion/Exclusion) dataset and saves it to the data folder

# Load required libraries
library(dplyr)
library(labelled)

# Source helper functions
source("data-raw/helpers.R")
source("data-raw/dv.R")

# Generate ie dataset
gen_ie <- function() {
  set.seed(123)

  # Get source data
  raw <- gen_dv()

  gen <- df_na(raw)

  gen <-   mutate(gen,
    IECAT = case_when(
      DVDECOD == "Inclusion Criteria" ~ "INCLUSION",
      DVDECOD == "Exclusion Criteria" ~ "EXCLUSION’ ",
      TRUE ~ NA_character_
    ),
   IETEST  = ifelse(
      !is.na(IECAT),
      sample(
        c(
          "Disease criteria",
          "Medication criteria",
          "Laboratory criteria",
          "Medical history criteria"
        ),
        nrow(gen),
        replace = TRUE
      ),
      NA_character_
    )
  )
  # Add labels
  additional_labels <- list(
    IECAT = "IE Category",
    IETEST = "IE Criterion Test Name"
  )
  
  gen <- select(
      gen,
      STUDYID, USUBJID, IECAT, IETEST
    )
  
  # Handle NA values and convert characters to factors
  gen <- df_na(gen, char_as_factor = TRUE)

  # Restore labels
  gen <- restore_labels(
    df = gen,
    orig_df = raw,
    additional_labels = additional_labels
  )

  return(gen)
}

# Generate the dataset
ie <- gen_ie()