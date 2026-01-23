
rm(list = ls())

df <- read.csv("C:/Users/kth200/regdata/ethirgast/accord/ACCORD_DATA_2025-11-21_1034.csv",
               header = TRUE,
               sep = ";",
               stringsAsFactors = FALSE)

dict_df <- read.csv("C:/Users/kth200/regdata/ethirgast/accord/ACCORD_DataDictionary_2025-11-27.csv",
                    header = TRUE,
                    sep = ",",
                    stringsAsFactors = FALSE)




#' Extract codes and labels from a REDCap choices string where options are
#' separated by "|" and code/label are separated by ",".
#'
#' @param dict_df A data frame/tibble with the REDCap data dictionary.
#' @param variable_name Value in the "Variable / Field Name" column to parse.
#' @param var_col Column name that holds variable names.
#' @param choice_col Column name that holds choices.
#' @return A tibble with columns `code` and `label` (0 rows if no choices).
extract_codes_labels_piped <- function(
    dict_df, variable_name,
    var_col    = "Variable...Field.Name",
    choice_col = "Choices..Calculations..OR.Slider.Labels") {
  # Ensure required pkgs exist
  if (!requireNamespace("dplyr", quietly = TRUE) ||
      !requireNamespace("stringr", quietly = TRUE) ||
      !requireNamespace("purrr", quietly = TRUE) ||
      !requireNamespace("tibble", quietly = TRUE)) {
    stop("Please install the packages: dplyr, stringr, purrr, tibble.", call. = FALSE)
  }

  # 1) Pull the raw choices string for the variable
  choices_str <- dict_df |>
    dplyr::filter(.data[[var_col]] == variable_name) |>
    dplyr::pull(.data[[choice_col]]) |>
    {\(x) x[1]}()

  # Guard: no choices
  if (is.null(choices_str) || is.na(choices_str) || choices_str == "") {
    return(tibble::tibble(code = character(), label = character()))
  }

  # Normalize line endings (in case the choices span lines)
  choices_str <- base::gsub("\\r\\n|\\r", "\n", choices_str)

  # If choices are accidentally separated by newlines instead of pipes,
  # convert newlines to pipes so we can handle uniformly
  # (comment this out if you *only* want pipe-based splitting)
  # choices_str <- stringr::str_replace_all(choices_str, "\\n+", " | ")

  # 2) Split into chunks by pipe
  chunks <- stringr::str_split(choices_str, "\\|")[[1]]

  # 3) Clean and drop empty chunks, then split each chunk once at the first comma
  purrr::map(chunks, \(chunk) stringr::str_trim(chunk)) |>
    purrr::discard(\(chunk) chunk == "") |>
    purrr::map(\(chunk) {
      parts <- stringr::str_split_fixed(chunk, "\\s*,\\s*", n = 2)
      tibble::tibble(
        code  = stringr::str_trim(parts[, 1]),
        label = stringr::str_trim(parts[, 2])
      )
    }) |>
    dplyr::bind_rows() |>
    # Optional: strip surrounding quotes if present
    dplyr::mutate(
      code  = stringr::str_remove_all(.data$code,  '^"|"$'),
      label = stringr::str_remove_all(.data$label, '^"|"$')
    )
}


diagnose <- extract_codes_labels_piped(dict_df, "adm_diagn")
diagnose$label[match(df$adm_diagn, diagnose$code)]









