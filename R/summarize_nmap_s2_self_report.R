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
    dplyr::rename(id = Intro_ID) |>
    dplyr::mutate(id = as.numeric(id)) |>
    dplyr::select(
      -c(Q322, Q323, Q328, Q329_1, Q329_2, Q655_1, Q655_2, Q2_1, Q2_2, Q325_1, Q657_1, Q326_1, Q326_2, matches("_TEXT")) # remove attention checks and text fields
    ) |>
    dplyr::arrange(id) |>
    dplyr::select(RecordedDate, 18:867)

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
    iip_complete            =  90 - rowSums(is.na(self_report_complete[,   6: 95])),
    ctq_complete            =  28 - rowSums(is.na(self_report_complete[,  96:123])),
    panas_complete          =  20 - rowSums(is.na(self_report_complete[, 124:143])),
    pid_complete            = 100 - rowSums(is.na(self_report_complete[, 144:243])),
    asr_complete            = 134 - rowSums(is.na(self_report_complete[, 244:377])),
    bpq_complete            =  80 - rowSums(is.na(self_report_complete[, 378:457])),
    fs_complete             =   8 - rowSums(is.na(self_report_complete[, 458:465])),
    dusi_complete           =  15 - rowSums(is.na(self_report_complete[, 466:480])),
    isc_complete            =  64 - rowSums(is.na(self_report_complete[, 481:544])),
    uppsp_complete          =  59 - rowSums(is.na(self_report_complete[, 545:603])),
    bfi_complete            =  60 - rowSums(is.na(self_report_complete[, 604:663])),
    cts_complete            =  40 - rowSums(is.na(self_report_complete[, 664:703])),
    emotb_percstrs_complete =  10 - rowSums(is.na(self_report_complete[, 704:713])),
    emotb_self_complete     =  10 - rowSums(is.na(self_report_complete[, 714:723])),
    emotb_mp_complete       =   7 - rowSums(is.na(self_report_complete[, 724:730])),
    emotb_sad_complete      =   8 - rowSums(is.na(self_report_complete[, 731:738])),
    emotb_gls_complete      =   5 - rowSums(is.na(self_report_complete[, 739:743])),
    emotb_posaf_complete    =  15 - rowSums(is.na(self_report_complete[, 744:758])),
    emotb_angaf_complete    =   5 - rowSums(is.na(self_report_complete[, 759:763])),
    emotb_angpa_complete    =   5 - rowSums(is.na(self_report_complete[, 764:768])),
    emotb_anghost_complete  =   5 - rowSums(is.na(self_report_complete[, 769:773])),
    emotb_emosup_complete   =   8 - rowSums(is.na(self_report_complete[, 774:781])),
    emotb_dls_complete      =  13 - rowSums(is.na(self_report_complete[, 782:794])),
    emotb_fearsoma_complete =   6 - rowSums(is.na(self_report_complete[, 795:800])),
    emotb_fearaf_complete   =   7 - rowSums(is.na(self_report_complete[, 801:807])),
    emotb_friend_complete   =   8 - rowSums(is.na(self_report_complete[, 808:815])),
    emotb_instrsup_complete =   8 - rowSums(is.na(self_report_complete[, 816:823])),
    emotb_lone_complete     =   5 - rowSums(is.na(self_report_complete[, 824:828])),
    emotb_perchost_complete =   8 - rowSums(is.na(self_report_complete[, 829:836])),
    emotb_percrej_complete  =   8 - rowSums(is.na(self_report_complete[, 837:844])),
    emotb_apathy_complete   =   7 - rowSums(is.na(self_report_complete[, 845:851])),
    total_complete          = 846 - rowSums(is.na(self_report_complete[,   6:851])),
    # count of missing items
    iip_missing             = rowSums(is.na(self_report_complete[,   6: 95])),
    ctq_missing             = rowSums(is.na(self_report_complete[,  96:123])),
    panas_missing           = rowSums(is.na(self_report_complete[, 124:143])),
    pid_missing             = rowSums(is.na(self_report_complete[, 144:243])),
    asr_missing             = rowSums(is.na(self_report_complete[, 244:377])),
    bpq_missing             = rowSums(is.na(self_report_complete[, 378:457])),
    fs_missing              = rowSums(is.na(self_report_complete[, 458:465])),
    dusi_missing            = rowSums(is.na(self_report_complete[, 466:480])),
    isc_missing             = rowSums(is.na(self_report_complete[, 481:544])),
    uppsp_missing           = rowSums(is.na(self_report_complete[, 545:603])),
    bfi_missing             = rowSums(is.na(self_report_complete[, 604:663])),
    cts_missing             = rowSums(is.na(self_report_complete[, 664:703])),
    emotb_percstrs_missing  = rowSums(is.na(self_report_complete[, 704:713])),
    emotb_self_missing      = rowSums(is.na(self_report_complete[, 714:723])),
    emotb_mp_missing        = rowSums(is.na(self_report_complete[, 724:730])),
    emotb_sad_missing       = rowSums(is.na(self_report_complete[, 731:738])),
    emotb_gls_missing       = rowSums(is.na(self_report_complete[, 739:743])),
    emotb_posaf_missing     = rowSums(is.na(self_report_complete[, 744:758])),
    emotb_angaf_missing     = rowSums(is.na(self_report_complete[, 759:763])),
    emotb_angpa_missing     = rowSums(is.na(self_report_complete[, 764:768])),
    emotb_anghost_missing   = rowSums(is.na(self_report_complete[, 769:773])),
    emotb_emosup_missing    = rowSums(is.na(self_report_complete[, 774:781])),
    emotb_dls_missing       = rowSums(is.na(self_report_complete[, 782:794])),
    emotb_fearsoma_missing  = rowSums(is.na(self_report_complete[, 795:800])),
    emotb_fearaf_missing    = rowSums(is.na(self_report_complete[, 801:807])),
    emotb_friend_missing    = rowSums(is.na(self_report_complete[, 808:815])),
    emotb_instrsup_missing  = rowSums(is.na(self_report_complete[, 816:823])),
    emotb_lone_missing      = rowSums(is.na(self_report_complete[, 824:828])),
    emotb_perchost_missing  = rowSums(is.na(self_report_complete[, 829:836])),
    emotb_percrej_missing   = rowSums(is.na(self_report_complete[, 837:844])),
    emotb_apathy_missing    = rowSums(is.na(self_report_complete[, 845:851])),
    total_missing           = rowSums(is.na(self_report_complete[,   6:851])),
    # calculate % complete
    iip_percent            = 1 - rowSums(is.na(self_report_complete[,   6: 95])) /  90,
    ctq_percent            = 1 - rowSums(is.na(self_report_complete[,  96:123])) /  28,
    panas_percent          = 1 - rowSums(is.na(self_report_complete[, 124:143])) /  20,
    pid_percent            = 1 - rowSums(is.na(self_report_complete[, 144:243])) / 100,
    asr_percent            = 1 - rowSums(is.na(self_report_complete[, 244:377])) / 134,
    bpq_percent            = 1 - rowSums(is.na(self_report_complete[, 378:457])) /  80,
    fs_percent             = 1 - rowSums(is.na(self_report_complete[, 458:465])) /   8,
    dusi_percent           = 1 - rowSums(is.na(self_report_complete[, 466:480])) /  15,
    isc_percent            = 1 - rowSums(is.na(self_report_complete[, 481:544])) /  64,
    uppsp_percent          = 1 - rowSums(is.na(self_report_complete[, 545:603])) /  59,
    bfi_percent            = 1 - rowSums(is.na(self_report_complete[, 604:663])) /  60,
    cts_percent            = 1 - rowSums(is.na(self_report_complete[, 664:703])) /  40,
    emotb_percstrs_percent = 1 - rowSums(is.na(self_report_complete[, 704:713])) /  10,
    emotb_self_percent     = 1 - rowSums(is.na(self_report_complete[, 714:723])) /  10,
    emotb_mp_percent       = 1 - rowSums(is.na(self_report_complete[, 724:730])) /   7,
    emotb_sad_percent      = 1 - rowSums(is.na(self_report_complete[, 731:738])) /   8,
    emotb_gls_percent      = 1 - rowSums(is.na(self_report_complete[, 739:743])) /   5,
    emotb_posaf_percent    = 1 - rowSums(is.na(self_report_complete[, 744:758])) /  15,
    emotb_angaf_percent    = 1 - rowSums(is.na(self_report_complete[, 759:763])) /   5,
    emotb_angpa_percent    = 1 - rowSums(is.na(self_report_complete[, 764:768])) /   5,
    emotb_anghost_percent  = 1 - rowSums(is.na(self_report_complete[, 769:773])) /   5,
    emotb_emosup_percent   = 1 - rowSums(is.na(self_report_complete[, 774:781])) /   8,
    emotb_dls_percent      = 1 - rowSums(is.na(self_report_complete[, 782:794])) /  13,
    emotb_fearsoma_percent = 1 - rowSums(is.na(self_report_complete[, 795:800])) /   6,
    emotb_fearaf_percent   = 1 - rowSums(is.na(self_report_complete[, 801:807])) /   7,
    emotb_friend_percent   = 1 - rowSums(is.na(self_report_complete[, 808:815])) /   8,
    emotb_instrsup_percent = 1 - rowSums(is.na(self_report_complete[, 816:823])) /   8,
    emotb_lone_percent     = 1 - rowSums(is.na(self_report_complete[, 824:828])) /   5,
    emotb_perchost_percent = 1 - rowSums(is.na(self_report_complete[, 829:836])) /   8,
    emotb_percrej_percent  = 1 - rowSums(is.na(self_report_complete[, 837:844])) /   8,
    emotb_apathy_percent   = 1 - rowSums(is.na(self_report_complete[, 845:851])) /   7,
    total_percent          = 1 - rowSums(is.na(self_report_complete[,   6:851])) / 846
  ) |>
    dplyr::select(id, 852:947)

  return(self_report_complete)

}
