#' Import and Score NeuroMAP SIDP Survey Data
#'
#' This function imports survey data from the NeuroMAP S1 - SIDP, processes it,
#' and computes scores for different personality disorders based on survey responses.
#' It performs data cleaning, reformatting, column sorting, and calculation of both
#' categorical and dimensional scores for various personality disorders.
#' Additionally, it handles specific diagnostic criteria for certain disorders.
#'
#' @importFrom qualtRics all_surveys fetch_survey
#' @importFrom dplyr rename select filter mutate arrange rowwise ungroup
#' @importFrom janitor clean_names
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_replace str_replace_all str_extract
#' @importFrom magrittr %>%
#'
#' @param ... Additional arguments passed to `qualtRics::fetch_survey`.
#'
#' @return A list containing two elements:
#'         - `summary`: a dataframe with the summary scores for each personality disorder.
#'         - `items`: a list of dataframes, each containing item-level data for a specific disorder.
#'
#' @examples
#' # Example usage:
#' \dontrun{
#' result <- import_score_neuromap_sidp()
#' summary_data <- result$summary
#' item_data <- result$items
#' }
#'
#' @author Nate Hall
#' @export

import_score_neuromap_sidp <- function(...){
  # library(tidyverse)
  # peek at surveys available
  my_surveys <- qualtRics::all_surveys() #%>% print(n = 100)

  sidp_id <- my_surveys %>% filter(name == "NeuroMAP S1 - SIDP (outside of the pipe - use this one)") %>% pull(id)

  sidp_items <- qualtRics::fetch_survey(sidp_id)
  # colnames(sidp_items)

  ##----------------------
  ##  Clean-up dataframe
  ##----------------------

  sidp_df <- sidp_items %>%
    dplyr::rename(id = session_info_4,
                  interviewer = session_info_6) %>%
    select(id, interviewer, StartDate, EndDate, ends_with("_rating")) %>%
    mutate(StartDate = lubridate::as_date(StartDate),
           EndDate = lubridate::as_date(EndDate)) %>%
    janitor::clean_names() %>%
    rename_with(~ str_replace_all(., "_rating", "")) %>%
    rename_with(~ str_replace(., "^[xX]", "")) %>%
    rename_with(~ str_replace(., "^(\\d+)_([a-z]+)$", "\\2_\\1")) %>%
    rename(antso_c = c_antso) %>%
    # for some reason, this is the only item that needs to be manually formatted
    mutate(avoid_7 = case_when(avoid_7 == 0 ~ "1 - Not Present",
                               avoid_7 == 1 ~ "2 - Subthreshold",
                               avoid_7 == 2 ~ "3 - Present",
                               avoid_7 == 3 ~ "4 - Strongly Present",
                               TRUE ~ NA
    ))

  #--------------------------------------------------------
  ##  Explore archival SIDP interviews and attempt to merge
  ##--------------------------------------------------------

  all_sidp <- my_surveys %>% filter(grepl("SIDP", name))
  og_sidp_id <- all_sidp %>% filter(name == "NeuroMAP S1 - SIDP") %>% pull(id)

  og_sidp_items <- qualtRics::fetch_survey(og_sidp_id)

  # missing <- c(92, 137, 169, 261, 264)
  # x <- og_sidp_items %>% dplyr::filter(as.numeric(SubjectID.SIDP) %in% missing) %>% select(SubjectID.SIDP, everything())

  # View(og_sidp_items)

  og_sidp_df <- og_sidp_items %>% select(SubjectID.SIDP, RecipientFirstName, RecipientLastName,  StartDate, EndDate, ends_with("_rating")) %>%
    mutate(id = as.numeric(SubjectID.SIDP), .before = SubjectID.SIDP) %>%
    dplyr::filter(!is.na(id)) %>%
    mutate(interviewer = paste0(substr(RecipientFirstName, 1, 1), substr(RecipientLastName, 1, 1)), .after = id) %>%
    # rename(id = subID) %>%
    select(-RecipientFirstName, - RecipientLastName, -SubjectID.SIDP) %>%
    mutate(StartDate = lubridate::as_date(StartDate),
           EndDate = lubridate::as_date(EndDate)
    ) %>% janitor::clean_names() %>%
    rename_with(~ str_replace_all(., "_rating", "")) %>%
    rename_with(~ str_replace(., "^[xX]", "")) %>%
    rename_with(~ str_replace(., "^(\\d+)_([a-z]+)$", "\\2_\\1")) %>%
    rename(antso_c = c_antso) %>%
    # for some reason, this is the only item that needs to be manually formatted
    mutate(avoid_7 = case_when(avoid_7 == 0 ~ "1 - Not Present",
                               avoid_7 == 1 ~ "2 - Subthreshold",
                               avoid_7 == 2 ~ "3 - Present",
                               avoid_7 == 3 ~ "4 - Strongly Present",
                               TRUE ~ NA
    )) %>% arrange(id)


  btw_sidp_id <- all_sidp %>% filter(name == "NeuroMAP S1 - SIDP (between)") %>% pull(id)

  btw_sidp_items <- qualtRics::fetch_survey(btw_sidp_id)

  btw_sidp_df <- btw_sidp_items %>% select(Q1_1, Q1_3,  StartDate, EndDate, ends_with("_rating")) %>% rename(id = Q1_1, interviewer = Q1_3) %>%
    dplyr::filter(!is.na(id) & !substr(id, 1,3) == 999) %>%
    rename(id = subID) %>%
    mutate(StartDate = lubridate::as_date(StartDate),
           EndDate = lubridate::as_date(EndDate)
    ) %>% janitor::clean_names() %>%
    rename_with(~ str_replace_all(., "_rating", "")) %>%
    rename_with(~ str_replace(., "^[xX]", "")) %>%
    rename_with(~ str_replace(., "^(\\d+)_([a-z]+)$", "\\2_\\1")) %>%
    rename(antso_c = c_antso) %>%
    # for some reason, this is the only item that needs to be manually formatted
    mutate(avoid_7 = case_when(avoid_7 == 0 ~ "1 - Not Present",
                               avoid_7 == 1 ~ "2 - Subthreshold",
                               avoid_7 == 2 ~ "3 - Present",
                               avoid_7 == 3 ~ "4 - Strongly Present",
                               TRUE ~ NA
    ))


  sidp_df <- bind_rows(og_sidp_df, btw_sidp_df, sidp_df) %>% arrange(id)



  # names(sidp_df)
  # sidp_df %>% select(contains("avoid"))

  ##----------------------------------------------------------------------
  ##  Rearrange columns to be grouped by disorder and sorted numerically
  ##----------------------------------------------------------------------

  sort_columns <- function(df, prefix_order) {
    cols <- names(df)
    # Extracting prefix and numeric parts
    prefix <- str_extract(cols, "^[a-z]+")
    num <- str_extract(cols, "\\d+")

    # Creating a data frame for sorting
    sorting_df <- data.frame(column = cols, prefix = prefix, num = as.numeric(num))
    sorting_df$num[is.na(sorting_df$num)] <- Inf # Treat NA as the highest number for sorting

    # Sorting columns based on prefix and then number
    sorted_cols <- sorting_df %>%
      filter(prefix %in% prefix_order) %>%
      arrange(match(prefix, prefix_order), num) %>%
      pull(column)

    # Include columns that did not match any prefix at the end
    non_matching_cols <- setdiff(cols, sorted_cols)
    c(sorted_cols, non_matching_cols)
  }

  prefix_order <- c("bordl", "avoid", "narci", "antso", "obcmp", "histr", "parnd", "depen", "szoid", "stypl")

  sorted_col_names <- sort_columns(sidp_df[,-c(1:4)], prefix_order)

  # perform re-ordering
  sidp_item_df <- sidp_df[, c("id", "interviewer", "start_date", "end_date", sorted_col_names)] %>%
    # remove fake entries if they exist (removed the 6 or so hanging out in Qualtrics on 12/6/23)
    dplyr::filter(!is.na(id), !id %in% c(999, 9999)) %>% arrange(id) %>%
    mutate(across(!c(id, start_date, end_date), ~as.factor(.)))

  ##------------------------------------------------------------------
  ##  Count Symptoms of Each Diagnosis and Compute Dimensional Score
  ##------------------------------------------------------------------

  count_responses <- function(data, prefix) {
    pd_columns <- grep(prefix, names(data), value = TRUE)

    # Count each response type
    data[paste0(prefix, "_sum_not_present")] <- apply(data[pd_columns], 1, function(x) sum(grepl("^1 - Not Present", x), na.rm = TRUE))
    data[paste0(prefix, "_sum_subthreshold")] <- apply(data[pd_columns], 1, function(x) sum(grepl("^2 - Subthreshold", x), na.rm = TRUE))
    data[paste0(prefix, "_sum_present")] <- apply(data[pd_columns], 1, function(x) sum(grepl("^3 - Present", x), na.rm = TRUE))
    data[paste0(prefix, "_sum_strongly_present")] <- apply(data[pd_columns], 1, function(x) sum(grepl("^4 - Strongly Present", x), na.rm = TRUE))
    data[paste0(prefix, "_sum_NA")] <- rowSums(is.na(data[pd_columns]), na.rm = TRUE)

    # Calculate total score
    data[paste0(prefix, "_sum_total")] <- rowSums(data[c(paste0(prefix, "_sum_not_present"),
                                                         paste0(prefix, "_sum_subthreshold"),
                                                         paste0(prefix, "_sum_present"),
                                                         paste0(prefix, "_sum_strongly_present"))],
                                                  na.rm = TRUE)

    # Calculate dimensional score
    data <- data %>% mutate("{prefix}_dimensional_score" :=
                              .data[[paste0(prefix, "_sum_subthreshold")]] * 1 +
                              .data[[paste0(prefix, "_sum_present")]] * 2 +
                              .data[[paste0(prefix, "_sum_strongly_present")]] * 3)

    data <- data %>% rowwise()

    if (prefix == "bordl") {
      data <- data %>% mutate(bordl_dx_dsm = if_else(sum(bordl_sum_present, bordl_sum_strongly_present) >= 5, 1, 0),
                              bordl_dx_neuromap = if_else(sum(bordl_sum_present, bordl_sum_strongly_present) >= 3 & (bordl_6 == "3 - Present" | bordl_6 == "4 - Strongly Present"), 1, 0))
    } else if (prefix == "avoid") {
      data <- data %>% mutate(avoid_dx_dsm = if_else(sum(avoid_sum_present, avoid_sum_strongly_present) >= 4, 1, 0))
    } else if (prefix == "narci") {
      data <- data %>% mutate(narci_dx_dsm = if_else(sum(narci_sum_present, narci_sum_strongly_present) >= 5, 1, 0))
    } else if (prefix == "antso") {
      data <- data %>% mutate(antso_dx_dsm = if_else(sum(antso_sum_present, antso_sum_strongly_present) >= 3 & (grepl("3", antso_c) | grepl("4", antso_c)), 1, 0))
    } else if (prefix == "obcmp") {
      data <- data %>% mutate(obcmp_dx_dsm = if_else(sum(obcmp_sum_present, obcmp_sum_strongly_present) >= 4, 1, 0))
    } else if (prefix == "histr") {
      data <- data %>% mutate(histr_dx_dsm = if_else(sum(histr_sum_present, histr_sum_strongly_present) >= 5, 1, 0))
    } else if (prefix == "parnd") {
      data <- data %>% mutate(parnd_dx_dsm = if_else(sum(parnd_sum_present, parnd_sum_strongly_present) >= 4, 1, 0))
    } else if (prefix == "depen") {
      data <- data %>% mutate(depen_dx_dsm = if_else(sum(depen_sum_present, depen_sum_strongly_present) >= 5, 1, 0))
    } else if (prefix == "szoid") {
      data <- data %>% mutate(szoid_dx_dsm = if_else(sum(szoid_sum_present, szoid_sum_strongly_present) >= 4, 1, 0))
    } else if (prefix == "stypl") {
      data <- data %>% mutate(stypl_dx_dsm = if_else(sum(stypl_sum_present, stypl_sum_strongly_present) >= 5, 1, 0))
    }

    data <- data %>% ungroup()


    # data %>% select(contains(prefix))


    # Remove item columns leaving only summary scores
    data <- data %>% dplyr::select(!all_of(pd_columns))

    return(data)
  }


  # create a copy of item-level data, which we append summaries and then remove
  data <- sidp_summary_df <- sidp_item_df

  item_list <- list()

  for(prefix in prefix_order) {
    item_list[[prefix]] <- sidp_item_df %>% select(id, interviewer, start_date, end_date, contains(prefix))
    sidp_summary_df <- count_responses(sidp_summary_df, prefix)
  }

  return_list <- list(summary = sidp_summary_df,
                      items = item_list)

  return(return_list)
}


