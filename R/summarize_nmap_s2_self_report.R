#' Summarize Self-Report Completion for NMAP Session 2
#'
#' This function processes and summarizes self-report data for NMAP session 2. It calculates
#' the number of complete items, missing items, and the percentage of completion for each
#' psychological measure per participant. The function integrates data from Qualtrics, 
#' filters based on session-specific criteria, and excludes certain IDs as specified.
#'
#' @details
#' The function performs the following operations:
#' - Checks for the presence of Qualtrics API credentials.
#' - Fetches raw self-report and session 3 data from Qualtrics.
#' - Excludes specific IDs based on predefined criteria (e.g., PSU IDs).
#' - Calculates completion metrics (# complete, # missing, % completion) for various measures 
#'   such as IIP, CTQ, PANAS, PID, and others.
#' - Returns a cleaned dataset with summarized completion metrics per participant.
#'
#' @return
#' A data frame containing the following columns for each participant:
#' - `id`: Participant ID.
#' - Completion metrics for each measure:
#'   - Number of complete items (`*_complete`).
#'   - Number of missing items (`*_missing`).
#'   - Percentage of completion (`*_percent`).
#'
#' @note
#' Ensure that Qualtrics API credentials are set in the environment using `qualtrics_api_credentials()`.
#'
#' @examples
#' \dontrun{
#' # Run the function to summarize self-report completion:
#' results <- summarize_nmap_s2_self_report()
#' head(results)
#' }
#'
#' @importFrom dplyr mutate rename select filter arrange
#' @importFrom qualtRics fetch_survey
#' @importFrom magrittr %>%
#' @author Rachel Velasquez
#' @export summarize_nmap_s2_self_report


summarize_nmap_s2_self_report <- function(...) {

  # checks if Qualtrics API Key is defined in environment
  if (Sys.getenv("QUALTRICS_API_KEY") <= 1 | Sys.getenv("QUALTRICS_BASE_URL") <= 1) {
    warning("Qualtrics API Key and/or Base URL has/have not been set up. \n Use 'qualtrics_api_credentials()' to configure.")
    return(invisible(NULL))
  }

  # import all self report data from Qualtrics
  self_report <- suppressMessages(qualtRics::fetch_survey(surveyID = "SV_2gwGMWFcer4Sl37", verbose = TRUE, breakout_sets = FALSE))

  self_report <- self_report |>
    dplyr::rename(id = Intro_ID, recorded_date = RecordedDate, intro_date = Intro_Date) |> 
    dplyr::mutate(
    id = as.numeric(id),
    recorded_date = as.Date(recorded_date, format = "%m/%d/%Y"),
    intro_date = as.Date(intro_date, format = "%m/%d/%Y"),
    date = dplyr::case_when(
      recorded_date < as.Date("2024-12-05") ~ recorded_date,  # Use RecordedDate for surveys before Dec 5, 2024
      TRUE ~ intro_date  # Otherwise, use Intro_Date
      )
    ) |> 
    dplyr::select(
      -c(Q322, Q323, Q328, Q329_1, Q329_2, Q655_1, Q655_2, Q2_1, Q2_2, Q325_1, Q657_1, Q326_1, Q326_2, matches("_TEXT")) # remove attention checks and text fields
    ) |>
    dplyr::arrange(id) |>
    dplyr::select(date, id, IIP_1:TB_Apathy_007)

  # pull self-reports for only IDs who contributed behavioral data (S3+)
  ## import s3 report
  s3_report <- suppressMessages(qualtRics::fetch_survey(surveyID = "SV_8ixbUxRgNWtxuHH", verbose = TRUE, breakout_sets = FALSE))
  s3_report <- s3_report |> select(RecordedDate, s3_sessioninfo_1:s3neigh_notesOLD) |> rename(id = s3_sessioninfo_4) |> arrange(id)

  ## filter out PSU IDs
  psu_ids <- c(
    1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
    31, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 52, 53, 54, 55, 56, 57, 59, 60,
    61, 62, 63, 65, 66, 68, 69, 70, 71, 73, 75, 77, 80, 81, 82, 83, 87, 90, 93, 94, 97, 98, 103, 107, 108, 109, 111, 112, 123
  )

  s3_report <- s3_report[!s3_report$id %in% psu_ids, ]

  ## further cleaning
  s3_report_clean <- s3_report %>%
    filter(
      !(id %in% c("0", "Invalid Number")),
      !(S3_visit_occurred %in% c("No-show", "Last minute cancellation")),
      is.na(S3_visit_occurred) | S3_visit_occurred != "No-show" & S3_visit_occurred != "Last minute cancellation",
      as.Date(RecordedDate) > as.Date("2021-01-01")
    )

  # calculate # complete, # missing, and % completion per measure and totals per ID ---
  self_report_complete <- self_report |> filter(id %in% unique(s3_report_clean$id))

  self_report_complete <- self_report_complete |> dplyr::mutate(
    # count of complete items
    iip_complete            =  90 - rowSums(is.na(self_report_complete[,   3: 92])),
    ctq_complete            =  28 - rowSums(is.na(self_report_complete[,  93:120])),
    panas_complete          =  20 - rowSums(is.na(self_report_complete[, 121:140])),
    pid_complete            = 100 - rowSums(is.na(self_report_complete[, 141:240])),
    asr_complete            = 134 - rowSums(is.na(self_report_complete[, 241:374])),
    bpq_complete            =  80 - rowSums(is.na(self_report_complete[, 375:454])),
    fs_complete             =   8 - rowSums(is.na(self_report_complete[, 455:462])),
    dusi_complete           =  15 - rowSums(is.na(self_report_complete[, 463:477])),
    isc_complete            =  64 - rowSums(is.na(self_report_complete[, 478:541])),
    uppsp_complete          =  59 - rowSums(is.na(self_report_complete[, 542:600])),
    bfi_complete            =  60 - rowSums(is.na(self_report_complete[, 601:660])),
    cts_complete            =  40 - rowSums(is.na(self_report_complete[, 661:700])),
    emotb_percstrs_complete =  10 - rowSums(is.na(self_report_complete[, 701:710])),
    emotb_self_complete     =  10 - rowSums(is.na(self_report_complete[, 711:720])),
    emotb_mp_complete       =   7 - rowSums(is.na(self_report_complete[, 721:727])),
    emotb_sad_complete      =   8 - rowSums(is.na(self_report_complete[, 728:735])),
    emotb_gls_complete      =   5 - rowSums(is.na(self_report_complete[, 736:740])),
    emotb_posaf_complete    =  15 - rowSums(is.na(self_report_complete[, 741:755])),
    emotb_angaf_complete    =   5 - rowSums(is.na(self_report_complete[, 756:760])),
    emotb_angpa_complete    =   5 - rowSums(is.na(self_report_complete[, 761:765])),
    emotb_anghost_complete  =   5 - rowSums(is.na(self_report_complete[, 766:770])),
    emotb_emosup_complete   =   8 - rowSums(is.na(self_report_complete[, 771:778])),
    emotb_dls_complete      =  13 - rowSums(is.na(self_report_complete[, 779:791])),
    emotb_fearsoma_complete =   6 - rowSums(is.na(self_report_complete[, 792:797])),
    emotb_fearaf_complete   =   7 - rowSums(is.na(self_report_complete[, 798:804])),
    emotb_friend_complete   =   8 - rowSums(is.na(self_report_complete[, 805:812])),
    emotb_instrsup_complete =   8 - rowSums(is.na(self_report_complete[, 813:820])),
    emotb_lone_complete     =   5 - rowSums(is.na(self_report_complete[, 821:825])),
    emotb_perchost_complete =   8 - rowSums(is.na(self_report_complete[, 826:833])),
    emotb_percrej_complete  =   8 - rowSums(is.na(self_report_complete[, 834:841])),
    emotb_apathy_complete   =   7 - rowSums(is.na(self_report_complete[, 842:848])),
    total_complete          = 846 - rowSums(is.na(self_report_complete[,   3:848])),
    # count of missing items
    iip_missing             = rowSums(is.na(self_report_complete[,   3: 92])),
    ctq_missing             = rowSums(is.na(self_report_complete[,  93:120])),
    panas_missing           = rowSums(is.na(self_report_complete[, 121:140])),
    pid_missing             = rowSums(is.na(self_report_complete[, 141:240])),
    asr_missing             = rowSums(is.na(self_report_complete[, 241:374])),
    bpq_missing             = rowSums(is.na(self_report_complete[, 375:454])),
    fs_missing              = rowSums(is.na(self_report_complete[, 455:462])),
    dusi_missing            = rowSums(is.na(self_report_complete[, 463:477])),
    isc_missing             = rowSums(is.na(self_report_complete[, 478:541])),
    uppsp_missing           = rowSums(is.na(self_report_complete[, 542:600])),
    bfi_missing             = rowSums(is.na(self_report_complete[, 601:660])),
    cts_missing             = rowSums(is.na(self_report_complete[, 661:700])),
    emotb_percstrs_missing  = rowSums(is.na(self_report_complete[, 701:710])),
    emotb_self_missing      = rowSums(is.na(self_report_complete[, 711:720])),
    emotb_mp_missing        = rowSums(is.na(self_report_complete[, 721:727])),
    emotb_sad_missing       = rowSums(is.na(self_report_complete[, 728:735])),
    emotb_gls_missing       = rowSums(is.na(self_report_complete[, 736:740])),
    emotb_posaf_missing     = rowSums(is.na(self_report_complete[, 741:755])),
    emotb_angaf_missing     = rowSums(is.na(self_report_complete[, 756:760])),
    emotb_angpa_missing     = rowSums(is.na(self_report_complete[, 761:765])),
    emotb_anghost_missing   = rowSums(is.na(self_report_complete[, 766:770])),
    emotb_emosup_missing    = rowSums(is.na(self_report_complete[, 771:778])),
    emotb_dls_missing       = rowSums(is.na(self_report_complete[, 779:791])),
    emotb_fearsoma_missing  = rowSums(is.na(self_report_complete[, 792:797])),
    emotb_fearaf_missing    = rowSums(is.na(self_report_complete[, 798:804])),
    emotb_friend_missing    = rowSums(is.na(self_report_complete[, 805:812])),
    emotb_instrsup_missing  = rowSums(is.na(self_report_complete[, 813:820])),
    emotb_lone_missing      = rowSums(is.na(self_report_complete[, 821:825])),
    emotb_perchost_missing  = rowSums(is.na(self_report_complete[, 826:833])),
    emotb_percrej_missing   = rowSums(is.na(self_report_complete[, 834:841])),
    emotb_apathy_missing    = rowSums(is.na(self_report_complete[, 842:848])),
    total_missing           = rowSums(is.na(self_report_complete[,   3:848])),
    # calculate % complete
    iip_percent            = 1 - rowSums(is.na(self_report_complete[,   3: 92])) /  90,
    ctq_percent            = 1 - rowSums(is.na(self_report_complete[,  93:120])) /  28,
    panas_percent          = 1 - rowSums(is.na(self_report_complete[, 121:140])) /  20,
    pid_percent            = 1 - rowSums(is.na(self_report_complete[, 141:240])) / 100,
    asr_percent            = 1 - rowSums(is.na(self_report_complete[, 241:374])) / 134,
    bpq_percent            = 1 - rowSums(is.na(self_report_complete[, 375:454])) /  80,
    fs_percent             = 1 - rowSums(is.na(self_report_complete[, 455:462])) /   8,
    dusi_percent           = 1 - rowSums(is.na(self_report_complete[, 463:477])) /  15,
    isc_percent            = 1 - rowSums(is.na(self_report_complete[, 478:541])) /  64,
    uppsp_percent          = 1 - rowSums(is.na(self_report_complete[, 542:600])) /  59,
    bfi_percent            = 1 - rowSums(is.na(self_report_complete[, 601:660])) /  60,
    cts_percent            = 1 - rowSums(is.na(self_report_complete[, 661:700])) /  40,
    emotb_percstrs_percent = 1 - rowSums(is.na(self_report_complete[, 701:710])) /  10,
    emotb_self_percent     = 1 - rowSums(is.na(self_report_complete[, 711:720])) /  10,
    emotb_mp_percent       = 1 - rowSums(is.na(self_report_complete[, 721:727])) /   7,
    emotb_sad_percent      = 1 - rowSums(is.na(self_report_complete[, 728:735])) /   8,
    emotb_gls_percent      = 1 - rowSums(is.na(self_report_complete[, 736:740])) /   5,
    emotb_posaf_percent    = 1 - rowSums(is.na(self_report_complete[, 741:755])) /  15,
    emotb_angaf_percent    = 1 - rowSums(is.na(self_report_complete[, 756:760])) /   5,
    emotb_angpa_percent    = 1 - rowSums(is.na(self_report_complete[, 761:765])) /   5,
    emotb_anghost_percent  = 1 - rowSums(is.na(self_report_complete[, 766:770])) /   5,
    emotb_emosup_percent   = 1 - rowSums(is.na(self_report_complete[, 771:778])) /   8,
    emotb_dls_percent      = 1 - rowSums(is.na(self_report_complete[, 779:791])) /  13,
    emotb_fearsoma_percent = 1 - rowSums(is.na(self_report_complete[, 792:797])) /   6,
    emotb_fearaf_percent   = 1 - rowSums(is.na(self_report_complete[, 798:804])) /   7,
    emotb_friend_percent   = 1 - rowSums(is.na(self_report_complete[, 805:812])) /   8,
    emotb_instrsup_percent = 1 - rowSums(is.na(self_report_complete[, 813:820])) /   8,
    emotb_lone_percent     = 1 - rowSums(is.na(self_report_complete[, 821:825])) /   5,
    emotb_perchost_percent = 1 - rowSums(is.na(self_report_complete[, 826:833])) /   8,
    emotb_percrej_percent  = 1 - rowSums(is.na(self_report_complete[, 834:841])) /   8,
    emotb_apathy_percent   = 1 - rowSums(is.na(self_report_complete[, 842:848])) /   7,
    total_percent          = 1 - rowSums(is.na(self_report_complete[,   3:848])) / 846
  ) |>
    dplyr::select(id, date, 849:944)

  return(self_report_complete)

}
