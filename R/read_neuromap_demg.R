#' @title read and clean demographic data from Qualtrics
#' @description This function pulls the demographic data and S3 or S5 session report from Qualtrics. Cleans the demog data and adds the sessions date and age when the task was run.
#'    If Qualtrics is not setup on your Rstudio install the qualtRics library and run the following command to connect to the Qualtrics server.
#'    qualtrics_api_credentials(api_key = "<YOUR-QUALTRICS_API_KEY>", base_url = "<YOUR-QUALTRICS_BASE_URL>", install = TRUE)
#'
#' @param session which session should the date the task was run be pulled from: "s3" or "s5"
#' @param task name of the task whose session date will be pulled into the demographic data in s_date column, which is used to calculate age of the participant at the time the session was run.
#'    "N": neighborhood, "K": defend the kingdom, "S": sorting mushroom, "V": vending machine, "W": weather prediction, "B": vanilla  baseline
#' @param all_cols should all columns from the demographic survey be included in the output? Default set to FALSE. Default will only include: ID, gender, date of birth, ethnicity, race, sex_orientation, work, education, house_income, and self_income.
#'
#' @examples
#'  \dontrun{
#'     demg_cleaned <- read_demg("s3", "K")
#'  }
#'
#' @export
#'
#' @author Nidhi Desai

read_neuromap_demg <- function(session = "s3", task = "N", all_cols = FALSE){
  pacman::p_load(dplyr, qualtRics, lubridate)

  # ---- pull demographics data from Qualtrics ----
  demg <- fetch_survey(surveyID = "SV_1X0lNvy1O3QWPNb", verbose = TRUE,
                       breakout_sets = FALSE, force_request = TRUE)

  # ---- clean the demg data table ----
  if (!all_cols) { demg <- demg %>% select(Q1, Q3, Q5, Q7, Q8, Q11, Q16, Q17, Q18, Q19) }
  demg <- demg %>%
    rename(ID = Q1, gender = Q3, dob = Q5, ethnicity = Q7, race = Q8,
           sex_orientation = Q11, work = Q16, education = Q17,
           house_income = Q18, self_income = Q19) %>%
    mutate(ID = as.numeric(ID)) %>%
    arrange(ID) %>%
    mutate(dob = as.Date(dob, format = "%m/%d/%Y")) %>%
    filter(ID != 999) %>% filter(!is.na(ID))


  # duplicate rows for subjectIDs
  warning(paste("duplicate rows in demographic data for",
                paste(demg$ID[duplicated(demg$ID)], collapse = ", ")))

  # ---- pull date of session 3 or session 5 from their respective qualtrics survey ----
  session_qualticsID <- ifelse(session == "s3", "SV_8ixbUxRgNWtxuHH",
                               ifelse(session == "s5", "SV_8bScWE5H4PGBgX4",
                                      stop("session input needs be either s3 or s5")))
  s_report <- fetch_survey(surveyID = session_qualticsID, verbose = TRUE,
                           breakout_sets = FALSE, force_request = TRUE)
  date_colname <- ifelse(session == "s3", "s3_sessioninfo_1", "s5_sessioninfo_1")
  subid_colname <- ifelse(session == "s3", "s3_sessioninfo_4", "s5_sessioninfo_4")
  s_report <- s_report %>%
    rename(s_date = date_colname) %>%
    mutate(s_date = as.Date(s_date, format = "%m/%d/%Y")) %>%
    rename(ID = subid_colname) %>%
    mutate(ID = as.numeric(ID)) %>%
    filter(!is.na(ID)) %>% filter(!(ID %in% c(0, 999)))

  # ---- remove duplicate rows for a subject ID and pull session date from the correct row ----
  if (session == "s3"){
    s_report <- s_report %>% filter(!(S3_visit_occurred == "No-show")) %>% arrange(ID)
    # which column contains the score for the task, have a non-NA score tells us that the task was run during that session
    task_score_colname <- ifelse(task == "N", "s3taskratings.neighborhood_score_3",
                                 ifelse(task == "K", "s3taskratings.DTK_score_3",
                                        ifelse(task == "V", "s3taskratings.vm_score_3",
                                               ifelse(task == "S", "s3taskratings.shrooms_score_3",
                                                      ifelse(task == "B", "s3taskratings.VB_score_3", NA)))))
  } else if (session == "s5") {
    # check task name
    if(!(task %in% c("DtK", "WPT"))) { stop("date is S5 but task is not DtK or WPT") }

    s_report <- s_report %>% arrange(ID) %>% relocate(ID, .before = s_date)
    task_score_colname <- ifelse(task == "K", "Online_Task Notes#1_1_1",
                                 ifelse(task == "W", "Online_Task Notes#1_2_1", NA))
  }

  s_date_df <- data.frame(ID = unique(s_report$ID)) %>%
    mutate(s_date = NA) %>% mutate(s_date = as.Date(s_date))

  for (i in seq_along(s_date_df$ID)){
    indx <- which(s_report$ID == s_date_df$ID[i])
    if (length(indx) > 1) { # multiple rows for a subjectID
      # find which of the multiple S3s was this task run
      temp_scores <- s_report[[task_score_colname]][indx]
      na_or_zero <- is.na(temp_scores) | temp_scores == 0
      if ( sum(!na_or_zero) == 1 ){
        # only one of the rows has a non NA values means that task was run on this date
        s_date_df$s_date[i] <- s_report$s_date[indx[!(na_or_zero)]]
      } else if ( sum(!na_or_zero) == 0 ){
        # this task was not run for any of the S3s
        s_date_df$s_date[i] <- NA
      } else if ( sum(!na_or_zero) > 1 ){
        # task was run during multiple S3s, so choosing the most recent date
        s_date_df$s_date[i] <- max(s_report$s_date[indx[!na_or_zero]])
      }

    } else if (length(indx) == 1) {
      s_date_df$s_date[i] <- s_report$s_date[indx]
    }
  }

  # ---- calculate age based on dob and S3 or S5 date ----
  demg <- demg %>%
    left_join(s_date_df, by = "ID") %>%
    mutate(age = as.period(interval(dob, s_date), unit = "year")$year) %>%
    relocate(s_date, .after = dob) %>% relocate(age, .after = s_date)

  return(demg)
}
