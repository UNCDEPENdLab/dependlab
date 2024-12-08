#' Summarize NMAP Session 5 Self-Report Completion
#'
#' This function fetches and processes self-report survey data from Qualtrics for 
#' NMAP Session 5. It calculates the number of completed, missing, and percentage
#' completion for various self-report measures, as well as total completion metrics
#' per participant.
#'
#' @param ... Additional arguments (currently unused).
#'
#' @return A data frame containing summary statistics for each participant, 
#' including counts of completed items, missing items, and completion percentages 
#' for each measure and overall.
#'
#' @details
#' The function performs the following steps:
#' \itemize{
#'   \item Checks if Qualtrics API credentials are set up in the environment.
#'   \item Fetches survey data from Qualtrics using the specified survey ID.
#'   \item Cleans and organizes the data by renaming columns, converting IDs to numeric,
#'         removing attention check and text response fields, and selecting relevant columns.
#'   \item Calculates the number of completed items, missing items, and completion 
#'         percentages for the following measures:
#'         \itemize{
#'           \item Inventory of Interpersonal Problems (90 item) (IIP-90)
#'           \item Personality Inventory for the DSM-5 Brief Form (PID-5-BF)
#'           \item Adult Self-Report (ASR)
#'           \item Borderline Personality Questionnaire (BPQ)
#'           \item Flourish Scale (FS)
#'           \item Big Five Inventory-2 (BFI-2)
#'           \item NIH EMOTB subscales (Emotional Support, Friend, Perceived Hostility, 
#'                 Perceived Rejection, Perceived Stress)
#'           \item Personality Assessment Inventory-Borderline Scale (PAI-BOR)
#'           \item Mini-Social Phobia Inventory (Mini-SPIN)
#'           \item Patient-Reported Outcomes Measurement Information System (PROMIS) Alcohol Use Short Form and Severity of Substance Use Short Form (3 month)
#'         }
#'   \item Computes total completion statistics across all measures.
#' }
#'
#' @note The function requires a valid Qualtrics API key and base URL, which must 
#' be set using `qualtrics_api_credentials()`. If these credentials are not configured,
#' the function will issue a warning and return `NULL`.
#'
#' @importFrom qualtRics fetch_survey
#' @importFrom dplyr rename mutate select arrange matches
#' @author Rachel Velasquez
#' @export summarize_nmap_s5_self_report
#'
#' @examples
#' \dontrun{
#' # Ensure Qualtrics API credentials are set:
#' qualtrics_api_credentials(api_key = "your_api_key", base_url = "your_base_url")
#'
#' # Fetch and summarize self-report data for NMAP Session 5
#' self_report_summary <- summarize_nmap_s5_self_report()
#' head(self_report_summary)
#' }


summarize_nmap_s5_self_report <- function(...) {

  # checks if Qualtrics API Key is defined in environment
  if (Sys.getenv("QUALTRICS_API_KEY") <= 1 | Sys.getenv("QUALTRICS_BASE_URL") <= 1) {
    warning("Qualtrics API Key and/or Base URL has/have not been set up. \n Use 'qualtrics_api_credentials()' to configure.")
    return(invisible(NULL))
  }

  # import all self report data from Qualtrics
  self_report <- suppressMessages(qualtRics::fetch_survey(surveyID = "SV_1ZkInh40pHM6w3Y", verbose = TRUE, breakout_sets = FALSE))

  self_report <- self_report |>
    dplyr::rename(id = Intro_ID) |>
    dplyr::mutate(id = as.numeric(id)) |>
    dplyr::select(
      -c(Q322, Q323, Q328, Q329_1, Q329_2, Q655_1, Q655_2, matches("_TEXT")) # remove attention checks and text fields
    ) |>
    dplyr::arrange(id) |>
    dplyr::select(RecordedDate, id, 40:581, Q338)


  # calculate # complete, # missing, and % completion per measure and totals per ID
  self_report_complete <- self_report |> dplyr::mutate(
    # count of complete items
    iip_complete            =  90 - rowSums(is.na(self_report[,   3: 92])),
    pid_complete            = 100 - rowSums(is.na(self_report[,  93:192])),
    asr_complete            = 134 - rowSums(is.na(self_report[, 193:326])),
    bpq_complete            =  80 - rowSums(is.na(self_report[, 327:406])),
    fs_complete             =   8 - rowSums(is.na(self_report[, 407:414])),
    bfi_complete            =  60 - rowSums(is.na(self_report[, 415:474])),
    emotb_emosup_complete   =   8 - rowSums(is.na(self_report[, 475:482])),
    emotb_friend_complete   =   8 - rowSums(is.na(self_report[, 483:490])),
    emotb_perchost_complete =   8 - rowSums(is.na(self_report[, 491:498])),
    emotb_percrej_complete  =   8 - rowSums(is.na(self_report[, 499:506])),
    paibor_complete         =  24 - rowSums(is.na(self_report[, 507:530])),
    minispin_complete       =   3 - rowSums(is.na(self_report[, 531:533])),
    emotb_percstrs_complete =  10 - rowSums(is.na(self_report[, 534:543])),
    promis_alc_complete     =   1 - rowSums(is.na(self_report[, 544:544])),
    promis_ssu_complete     =   1 - rowSums(is.na(self_report[, 545:545])),
    total_complete          = 543 - rowSums(is.na(self_report[,   3:545])),
    # count of missing items
    iip_missing             = rowSums(is.na(self_report[,   3: 92])),
    pid_missing             = rowSums(is.na(self_report[,  93:192])),
    asr_missing             = rowSums(is.na(self_report[, 193:326])),
    bpq_missing             = rowSums(is.na(self_report[, 327:406])),
    fs_missing              = rowSums(is.na(self_report[, 407:414])),
    bfi_missing             = rowSums(is.na(self_report[, 415:474])),
    emotb_emosup_missing    = rowSums(is.na(self_report[, 475:482])),
    emotb_friend_missing    = rowSums(is.na(self_report[, 483:490])),
    emotb_perchost_missing  = rowSums(is.na(self_report[, 491:498])),
    emotb_percrej_missing   = rowSums(is.na(self_report[, 499:506])),
    paibor_missing          = rowSums(is.na(self_report[, 507:530])),
    minispin_missing        = rowSums(is.na(self_report[, 531:533])),
    emotb_percstrs_missing  = rowSums(is.na(self_report[, 534:543])),
    promis_alc_missing      = rowSums(is.na(self_report[, 544:544])),
    promis_ssu_missing      = rowSums(is.na(self_report[, 545:545])),
    total_missing           = rowSums(is.na(self_report[,   3:545])),
    # calculate % complete
    iip_percent             = 1 - rowSums(is.na(self_report[,   3: 92])) /  90,
    pid_percent             = 1 - rowSums(is.na(self_report[,  93:192])) / 100,
    asr_percent             = 1 - rowSums(is.na(self_report[, 193:326])) / 134,
    bpq_percent             = 1 - rowSums(is.na(self_report[, 327:406])) /  80,
    fs_percent              = 1 - rowSums(is.na(self_report[, 407:414])) /   8,
    bfi_percent             = 1 - rowSums(is.na(self_report[, 415:474])) /  60,
    emotb_emosup_percent    = 1 - rowSums(is.na(self_report[, 475:482])) /   8,
    emotb_friend_percent    = 1 - rowSums(is.na(self_report[, 483:490])) /   8,
    emotb_perchost_percent  = 1 - rowSums(is.na(self_report[, 491:498])) /   8,
    emotb_percrej_percent   = 1 - rowSums(is.na(self_report[, 499:506])) /   8,
    paibor_percent          = 1 - rowSums(is.na(self_report[, 507:530])) /  24,
    minispin_percent        = 1 - rowSums(is.na(self_report[, 531:533])) /   3,
    emotb_percstrs_percent  = 1 - rowSums(is.na(self_report[, 534:543])) /  10,
    promis_alc_percent      = 1 - rowSums(is.na(self_report[, 544:544])) /   1,
    promis_ssu_percent      = 1 - rowSums(is.na(self_report[, 545:545])) /   1,
    total_percent           = 1 - rowSums(is.na(self_report[,   3:545])) / 543
  ) |>
    dplyr::select(id, 546:593)

  return(self_report_complete)

}
