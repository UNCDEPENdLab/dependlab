

#' Retrieve medication list for NeuroMAP
#'
#' @param med_info_path The local path to the medication list Excel sheet containing a sheet called 'med_info'
#' @param survey_name The name of the survey that contains medication information in the Qualtrics Survey
#' @param id_column The name of the column in the Qualtrics form corresponding to the participant ID number
#' @param common_dose_units A character vector of common dosage units that might appear in the Qualtrics form (keep lower case)
#' @param med_question A character vector of the name of the medication question in Qualtrics
#'
#' @returns A data.frame with the raw data and additional columns (medication class, dose amount, etc.)
#'
#' @export
#' @author Zach Vig
#'
#'
#' @importFrom cli cli_alert_danger cli_alert_info
#' @importFrom magrittr %>%
#' @importFrom tidyr separate gather
#' @importFrom dplyr mutate transmute select arrange filter bind_rows
get_neuromap_med_list <- function(med_info_path = "Studies/NeuroMAP/Data/Clinical/NMAP Clinical Interview Medication List.xlsx",
                                  survey_name = "NeuroMAP S3 - Pre-Session Survey", id_column = "Intro_ID", med_question = "Q11",
                                  common_dose_units = c("mg", "mcg", "pill", "pills", "tablet", "tablets", "ml", "grams", "g", "puff", "puffs")) {

  if(!checkmate::test_file_exists(med_info_path)) {
    cli::cli_abort(
      c("x" = "Medication information path does not exist: {.path {med_info_path}}")
      )
  }

  # checks if Qualtrics API Key/Base URL are defined in environment
  if(Sys.getenv("QUALTRICS_API_KEY") <= 1 || Sys.getenv("QUALTRICS_BASE_URL") <= 1) {
    cli::cli_abort(
      c("x" = "Qualtrics API Key and/or Base URL has/have not been set up",
      "i" = "Use 'qualtrics_api_credentials()' to configure.")
    )
  }

  # validates survey name
  all_surveys <- qualtRics::all_surveys()
  if(!(survey_name %in% all_surveys$name)) {
    cli::cli_abort(
      c("Survey name ('{survey_name}') not found in Qualtrics")
      )
  }

  # get survey
  survey_id <- with(all_surveys, id[name == survey_name])
  survey <- suppressMessages(
    qualtRics::fetch_survey(survey_id, verbose = FALSE, convert = FALSE)
  )

  med_columns <- names(survey)[grep("Q11_", names(survey))]
  med_list <- survey[ ,c(id_column, med_columns)]

  # get number of med inputs
  inputs <- unique(
    stringr::str_extract(
      med_columns,
      paste0("^", med_question, "_(\\d+)_\\d+"),
      group = 1)
  )

  df0 <- list()

  for(input in inputs) {
    df0[[input]] <- med_list[ ,c(id_column, sprintf("Q11_%s_%s", input, 1:4))]
    names(df0[[input]]) <- c("id", "med_raw", "dose_raw", "date_raw", "time_raw")
  }

  # build long-form data.frame
  df <- df0 %>%
    bind_rows %>%
    arrange(id) %>%
    filter(if_any(`med_raw`:`date_raw`, complete.cases))

  med_info <- readxl::read_excel(med_info_path, sheet = "med_info") %>% mutate(id = 1:n())

  # make sure there aren't any med names that are too similar
  no_vowel <- gsub("[aeiou]", "_", med_info$med_name)
  dups <- c()
  for (i in seq_along(med_info$med_name)) {
    m <- stringr::str_like(med_info$med_name[i], no_vowel)
    if (sum(m) > 1) {
      if (i %in% dups) next
      other <- setdiff(which(m), i)
      dups <- append(dups, other)
      cli::cli_abort(
        c("x" = "Med names '{med_info$med_name[i]}' and '{med_info$med_name[other]}' are too similar!",
        "i" = "Check the medication information list & remove duplicate entries: {.path {med_info_path}}")
      )
    }
  }

  # function for matching information
  match_med <- function(in_med_name, col = "med_class", is_exact_match = F) {

    out <- c()

    this_col <- med_info %>% select(all_of(col), id)

    med_names <- med_info %>%
      select(med_name, id) %>%
      transmute(id = id, ref_med_name = gsub("[aeiouy]", "_", med_name))

    nmax <- max(stringr::str_count(med_info$alt_names, "\\,"), na.rm = TRUE) + 1
    alt_med_names <- med_info %>%
      select(alt_names, id) %>%
      mutate(alt_names = gsub("\\, ", "\\,", alt_names)) %>%
      separate(alt_names, into = paste0("alt", 1:nmax), sep = "\\,", fill = "right") %>%
      gather("alt", "alt_names", - id) %>%
      select(id, alt_names) %>%
      na.omit() %>%
      arrange(id) %>%
      transmute(id = id, ref_alt_names = gsub("[aeiouy]", "_", alt_names))

    for (m in seq_along(in_med_name)) {

      cli::cli_progress_update(
        .envir = .GlobalEnv
      )

      # check for a match
      match_id <- with(med_names, id[stringr::str_like(in_med_name[m], ref_med_name)])

      # if no match, check alternative names
      if (length(match_id) == 0) {
        s_match_id <- with(alt_med_names, id[stringr::str_like(in_med_name[m], ref_alt_names)])

        if (length(s_match_id) > 0) {
          if (isTRUE(is_exact_match)) {
            out[m] <- FALSE
          } else {
            i <- which(this_col$id == s_match_id)
            out[m] <- as.character(this_col[i, 1])
          }
        } else {
          if (isTRUE(is_exact_match)) {
            out[m] <- TRUE
            next
          } else {
            # if no alternative names work, try breaking up the original into separate words
            words <- c(stringr::str_extract_all(in_med_name[m], "\\w+", simplify = TRUE))
            check <- sapply(words, function(w) any(stringr::str_like(w, med_names$ref_med_name)))
            if (any(check)) {
              j <- min(which(check))
              match_id <- with(med_names, id[stringr::str_like(words[j], ref_med_name)])
            } else {
              out[m] <- NA
              next
            }
          }
        }
      }

      if (isTRUE(is_exact_match)) {
        out[m] <- FALSE
      } else {
        i <- which(this_col$id == match_id)
        out[m] <- as.character(this_col[i, 1])
      }
    }

    return(out)

  }

  cli::cli_progress_bar(
    name = "Matching medication names...",
    total = nrow(df) * 4,
    .envir = .GlobalEnv
  )

  df <- df %>%
    mutate(
      med_name = match_med(med_raw, col = "med_name"),
      med_class = match_med(med_raw, col = "med_class"),
      med_psych = match_med(med_raw, col = "med_psych"),
      name_flag = match_med(med_raw, is_exact_match = TRUE)
    )

  cli::cli_progress_done(
    .envir = .GlobalEnv,
    result = "clear"
  )

  get_dose <- function(in_dose, get_amt = TRUE) {

    amt <- sapply(
      in_dose,
      function (d) {
        str <- stringr::str_extract(d, "(^[\\.1-90/]+)([ ]{0,1})(.*)?", group = 1)
        return(ifelse(stringr::str_count(str) > 0, str, NA))
      }
    )

    dose <- sapply(
      in_dose,
      function (d) {
        str <- stringr::str_extract(d, "(^[\\.1-90/]+)([ ]{0,1})(.*)?", group = 3)
        return(ifelse(stringr::str_count(str) > 0, str, NA))
      }
    )

    flag <- as.logical(tolower(dose) %in% common_dose_units)

    if (isTRUE(get_amt)) {
      return(ifelse(flag, amt, NA))
    } else {
      return(ifelse(flag, dose, NA))
    }

  }

  df <- df %>%
    mutate(
      med_dose_amt = get_dose(dose_raw, get_amt = TRUE),
      med_dose_unit = get_dose(dose_raw, get_amt = FALSE)
    ) %>%
    mutate(
      dose_flag = is.na(med_dose_amt)
    )

  return(df)

}
