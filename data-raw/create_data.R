# This script runs all the data-raw scripts to create and document all datasets in one go

# Load required packages
library(purrr)

# Get all dataset scripts (exclude helpers.R and this file)
data_scripts <- list.files(
  path = "data-raw",
  pattern = "\\.R$",
  full.names = TRUE
)

# Filter out helper and create_data scripts
data_scripts <- data_scripts[
  !grepl("(helpers\\.R|create_data\\.R)", data_scripts)
]

# Run each script and handle saving and documentation
run_script <- function(script_path) {
  script_name <- basename(script_path)
  dataset_name <- tools::file_path_sans_ext(script_name) # Extract dataset name (e.g., "adsl" from "adsl.R")
  message(paste0("Running ", script_name, "..."))

  # Create environment to capture the resulting dataset
  script_env <- new.env(parent = globalenv())

  tryCatch(
    {
      # Source the script in the environment
      source(script_path, local = script_env)
      message(paste0("Successfully processed ", script_name))

      # Check if the expected dataset object exists in the environment
      if (exists(dataset_name, envir = script_env)) {
        # Get the dataset object
        dataset <- get(dataset_name, envir = script_env)

        # Save the dataset
        message(paste0("Saving ", dataset_name, " dataset..."))

        assign(dataset_name, dataset, envir = .GlobalEnv)
        do.call(
          usethis::use_data,
          list(as.name(dataset_name), overwrite = TRUE, compress = "xz")
        )

        # Generate documentation
        message(paste0("Generating documentation for ", dataset_name, "..."))
        # Dataset is already in global environment from above
        tryCatch(
          {
            roxygen2_data(dataset_name)
            message(paste0("Successfully documented ", dataset_name))
            cat("---------------------------------\n")
          },
          error = function(e) {
            message(paste0("Error documenting ", dataset_name, ": ", e$message))
          },
          finally = {
            # Clean up global environment
            if (exists(dataset_name, envir = .GlobalEnv)) {
              rm(list = dataset_name, envir = .GlobalEnv)
            }
          }
        )
      } else {
        warning(paste0(
          "Expected dataset '",
          dataset_name,
          "' not found after running ",
          script_name
        ))
      }
    },
    error = function(e) {
      message(paste0("Error processing ", script_name, ": ", e$message))
    }
  )
}

# Run all other data creation scripts
walk(data_scripts, run_script)

message("All datasets have been created and documented.")
