#' Generate a Detailed Report for a Specific Subject from SIDP Survey Data
#'
#' This function generates a detailed report for a specific subject using SIDP survey data.
#' It utilizes the `import_score_neuromap_sidp` function to import and process the data,
#' then filters and summarizes the information for a given subject ID.
#' The report includes counts of symptoms by diagnosis, dimensional scores, and
#' DSM diagnostic codes for various personality disorders.
#' Detailed summaries for each personality disorder are also provided.
#'
#' @param id Numeric or integer. The unique identifier of the subject for whom the report is to be generated.
#'
#' @return The function does not return a value but prints a detailed report to the console.
#'         The report includes general summaries and detailed breakdowns for each personality disorder.
#'
#' @examples
#' # Example usage:
#' # sidp_report(420)
#'
#'
#' @importFrom dplyr filter select
#' @importFrom tibble rownames_to_column
#' @importFrom purrr map_dfr
#' @importFrom stringr str_extract
#' @importFrom magrittr %>%
#' @importFrom tidyr pivot_longer
#'
#' @author Nate Hall
#' @export

sidp_report <- function(id){

  sidp_all <- import_score_neuromap_sidp()

  # Filtering the data for the target subject
  target_id <- id
  this_subject_summary <- sidp_all[["summary"]] %>% dplyr::filter(id == target_id)



  pds <- c("bordl", "avoid", "narci", "antso", "obcmp", "histr", "parnd", "depen", "szoid", "stypl")

  cat("\n--- Symptom Counts By Diagnosis ---\n")
  for (pd in pds) {
    present_count <- this_subject_summary[[paste0(pd, "_sum_present")]]
    strongly_present_count <- this_subject_summary[[paste0(pd, "_sum_strongly_present")]]
    total_symptoms <- present_count + strongly_present_count
    dimensional_score <- this_subject_summary[[paste0(pd, "_dimensional_score")]]
    dx_code <- this_subject_summary[[paste0(pd, "_dx_dsm")]]

    cat(toupper(pd), "|| Symptoms: ", total_symptoms, " | Dimensional Score: ", dimensional_score, " | Meets Criteria: ", as.logical(dx_code) ,"\n", sep = "")
  }

  # Loop over each PD
  for (pd in pds) {
    cat("\n---", toupper(pd), "Detailed Summary ---\n")

    not_present_count <- this_subject_summary[[paste0(pd, "_sum_not_present")]]
    subthresh_count <- this_subject_summary[[paste0(pd, "_sum_subthreshold")]]
    present_count <- this_subject_summary[[paste0(pd, "_sum_present")]]
    strongly_present_count <- this_subject_summary[[paste0(pd, "_sum_strongly_present")]]
    total_symptoms <- present_count + strongly_present_count
    dimensional_score <- this_subject_summary[[paste0(pd, "_dimensional_score")]]
    dx_code <- this_subject_summary[[paste0(pd, "_dx_dsm")]]
    na_count <- this_subject_summary[[paste0(pd, "_sum_NA")]]

    items <- sidp_all[["items"]][[pd]] %>%
      dplyr::filter(id == target_id) %>%
      select(-c(id:end_date)) %>%
      t() %>%
      data.frame() %>%
      rownames_to_column(var = "sx")

    colnames(items)[2] <- "rating"


    # Lead with most general and move downwards towards specifics
    if(pd == "bordl") cat("Neuromap Diagnostic Code: ", this_subject_summary[[paste0(pd, "_dx_neuromap")]], "\n")
    cat("DSM Diagnostic Code: ", dx_code, "\n")
    cat("Dimensional Score: ", dimensional_score, "\n")

    # Print the number of symptoms endorsed at each level
    cat("Strongly Present: ", strongly_present_count, "\n")
    if (strongly_present_count >= 1) {
      cat("          -------------------------\n          ")
      capture.output(items %>% filter(grepl("4", rating)) %>% print() ) %>% cat(sep = "\n          ")
      cat("          -------------------------\n")
    }
    cat("Present: ", present_count, "\n")
    if (present_count >= 1) {
      cat("          -------------------------\n          ")
      capture.output(items %>% filter(grepl("3", rating)) %>% print() ) %>% cat(sep = "\n          ")
      cat("          -------------------------\n")
    }
    cat("Subthreshold: ", subthresh_count, "\n")
    if (subthresh_count >= 1) {
      cat("          -------------------------\n          ")
      capture.output(items %>% filter(grepl("2", rating)) %>% print(row.names = FALSE) ) %>% cat(sep = "\n          ")
      cat("          -------------------------\n")
    }
    cat("Not Present: ", not_present_count, "\n")
    cat("NA/Undetermined: ", na_count, "\n")
    # cat("Total : ", this_subject_summary[[paste0(pd, "_sum_total")]], "\n")
  }

  # return(sidp_all)
}





