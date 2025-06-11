#' @name restore_labels
#'
#' @title Restore and Apply Labels
#'
#' @description
#' Enhanced function for retrieving labels from one dataframe to restore onto another,
#' and applying additional labels for new variables. This combines the functionality
#' of restoring existing labels and applying new ones in a single call. Additional
#' labels will override any existing labels from other sources.
#'
#' @param df Input dataframe to apply labels to
#' @param orig_df Dataframe from which to retrieve existing labels
#' @param additional_labels Named list of additional labels for new variables
#' @param source_dfs Optional additional source dataframes to extract labels from
#' @param verbose Logical if TRUE: Variables without labels will be shown during execution
#'
#' @return The dataframe df with all labels applied
#'
#' @importFrom formatters var_labels
#' @noRd
restore_labels <- function(
  df,
  orig_df,
  additional_labels = list(),
  source_dfs = NULL,
  verbose = TRUE
) {
  env_verbose <- as.logical(Sys.getenv("GEN_VERBOSE", unset = TRUE))
  # Use function parameter if explicitly provided, otherwise use environment variable
  verbose <- if (missing(verbose)) env_verbose else verbose

  # Get current labels from df and orig_df
  labels_df <- formatters::var_labels(df, fill = TRUE)
  labels_orig_df <- formatters::var_labels(orig_df, fill = TRUE)

  # Ensure labels are character strings
  labels_df <- lapply(labels_df, as.character)
  labels_orig_df <- lapply(labels_orig_df, as.character)

  # Ensure additional_labels are character strings
  additional_labels <- lapply(additional_labels, as.character)

  # Process additional source dataframes if provided
  if (!is.null(source_dfs)) {
    if (!is.list(source_dfs) || is.data.frame(source_dfs)) {
      source_dfs <- list(source_dfs)
    }

    for (src_df in source_dfs) {
      src_labels <- formatters::var_labels(src_df, fill = FALSE)
      src_labels <- lapply(src_labels, as.character)
      # Add source labels to orig_df labels (don't override existing)
      for (var_name in names(src_labels)) {
        if (!(var_name %in% names(labels_orig_df))) {
          labels_orig_df[[var_name]] <- src_labels[[var_name]]
        }
      }
    }
  }

  ## get vars that don't have a label
  miss_label <- labels_df[names(labels_df) == labels_df]
  ### vars with label
  non_miss_label <- labels_df[names(labels_df) != labels_df]

  ### retrieve the ones that are defined in the original label
  miss_label2 <- labels_orig_df[intersect(
    names(miss_label),
    names(labels_orig_df)
  )]

  ### apply additional labels for new variables (now for all variables in additional_labels)
  additional_label_vars <- names(additional_labels)
  additional_label_list <- additional_labels[additional_label_vars]
  names(additional_label_list) <- additional_label_vars

  ### ones that will remain missing after all attempts
  no_label <- setdiff(
    names(labels_df),
    names(c(miss_label2, non_miss_label, additional_label_list))
  )
  names(no_label) <- no_label

  if (verbose && length(no_label) > 0 && !(identical(no_label, character(0)))) {
    message(
      paste(
        "Variables without a label on ",
        toupper(deparse(substitute(df))),
        ":",
        paste(no_label, collapse = ", ")
      )
    )
  }

  ### Combine all labels - ensuring additional_labels have the highest priority
  ### This is the key change: additional_labels will override all other sources
  all_labels <- c(non_miss_label, miss_label2)

  # Now overlay additional_labels (which will override any existing labels with the same name)
  for (name in names(additional_label_list)) {
    all_labels[[name]] <- additional_label_list[[name]]
  }

  # Ensure all labels are character strings
  all_labels <- lapply(all_labels, function(x) {
    if (!is.character(x) || is.na(x)) {
      return(as.character(x))
    }
    return(x)
  })

  ## match with the order of names on df and fill missing ones with NA character
  final_labels <- character(length(names(df)))
  names(final_labels) <- names(df)

  for (col in names(df)) {
    if (col %in% names(all_labels)) {
      final_labels[col] <- all_labels[[col]]
    } else {
      final_labels[col] <- NA_character_
    }
  }

  ## apply the updated labels to the dataframe
  tryCatch(
    {
      formatters::var_labels(df) <- final_labels
    },
    error = function(e) {
      message("Error applying labels: ", e$message)
      problem_cols <- names(which(
        !vapply(final_labels, is.character, logical(1))
      ))
      if (length(problem_cols) > 0) {
        message("Problem columns: ", paste(problem_cols, collapse = ", "))
      }
    }
  )

  attr(df, "label") <- attr(orig_df, "label")

  return(df)
}

#' Helper functions for data generation
#'
#' This file contains utility functions used by the data generation scripts.

#' @title Set missings to NA in Dataframe
#' @description
#' A function used to convert any missing values to NA in the specified dataframe.
#' @param df Input dataframe
#' @param char_as_factor Specify whether or not character variables will automatically
#' be converted to a factor (Default is TRUE)
#' @param verbose logical if TRUE : Variables that have no label defined will be shown during execution.
#' @returns a dataframe
df_na <- function(df, char_as_factor = TRUE, verbose = TRUE) {
  env_verbose <- as.logical(Sys.getenv("GEN_VERBOSE", unset = TRUE))
  # Use function parameter if explicitly provided, otherwise use environment variable
  verbose <- if (missing(verbose)) env_verbose else verbose

  if (!is.data.frame(df)) {
    stop("Input must be a dataframe")
  }

  target_vars <- names(df)[sapply(df, is.character)]

  if (length(target_vars) == 0) {
    return(df)
  }

  if (verbose) {
    message(paste(
      "For variables",
      paste(target_vars, collapse = ", "),
      "blank will be replaced by NA"
    ))

    if (char_as_factor) {
      message("and the variables will be converted into a factor")
    }
  }

  for (x in target_vars) {
    xi <- df[[x]]
    xi_label <- attr(xi, "label")

    xi <- dplyr::na_if(xi, "")

    if (char_as_factor) {
      xi <- factor(xi)
    }

    df[, x] <- xi
    attr(df[, x], "label") <- xi_label
  }

  return(df)
}


#' Generate roxygen2 documentation for a dataset
#'
#' Creates a documentation file in R/ directory for a given dataset.
#' @param df_name Name of the dataset to document
#' @return Nothing, writes documentation file to R/ directory
#' @noRd
roxygen2_data <- function(df_name) {
  # Get the dataset from the parent environment
  df <- get(df_name, envir = parent.frame())

  title <- paste0("#' @title ", df_name)

  descr <- paste0("#' @description ", df_name, " modified from pharmaversesdtm")
  src <-
    "#' @source data from pharmaversesdtm."

  fmt <-
    paste0(
      "#' @format A data frame with ",
      nrow(df),
      " rows and ",
      ncol(df),
      " variables:"
    )

  itemize <- data.frame(
    colname = colnames(df),
    label = purrr::map_chr(seq_along(colnames(df)), function(x) {
      if (is.null(attr(df[[x]], "label"))) {
        names(df)[[x]]
      } else {
        attr(df[[x]], "label")
      }
    }),
    stringsAsFactors = FALSE
  )

  item <-
    paste0("#'  \\item{", itemize$colname, "}{", itemize$label, "}")
  item <- c("#' \\describe{", item, "#' }")
  seealso <-
    paste0(
      "#' @seealso ",
      paste0(
        "\\code{\\link{",
        noquote(gsub(
          ".rda",
          "",
          list.files(path = paste0(getwd(), "/data"))
        )),
        "}}",
        collapse = " "
      )
    )
  key <-
    paste0(
      "#' @keywords datasets ",
      tolower(stringr::word(gsub("_", " ", df_name), 1))
    )
  atname <- paste0("#' @name ", tolower(df_name))
  examp <-
    c(
      "#' @examples",
      "#' \\dontrun{",
      paste0("#'  data(\"", df_name, "\")"),
      "#' }",
      paste0("\"", df_name, "\"")
    )

  description <-
    c(
      title,
      "#'",
      descr,
      src,
      "#'",
      fmt,
      item,
      seealso,
      key,
      atname,
      examp,
      ""
    )

  writeLines(description, file.path("R", paste0(df_name, ".R")))
}
