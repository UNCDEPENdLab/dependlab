#' Check NMAP Self Report Completion
#'
#' This function retrieves self-report data from Qualtrics, processes it to calculate the percentage of completion for various psychological measures, and returns a summary data frame with the calculated completion percentages.
#'
#' @usage check_nmap_self_report_completion(...)
#'
#' @details
#' The function checks if the Qualtrics API key and base URL are set in the environment variables. If they are not set, the function issues a warning and exits. If the credentials are set, the function proceeds to:
#' \itemize{
#'   \item Import all self-report data from Qualtrics.
#'   \item Convert `Intro_ID` to a three-digit format.
#'   \item Rename `Intro_ID` to `id`.
#'   \item Remove specific columns related to attention checks and text fields.
#'   \item Arrange the data by `id` and select relevant columns.
#'   \item Calculate the percentage of completion for each psychological measure.
#'   \item Return a summary data frame with the calculated completion percentages.
#' }
#'
#' @return A data frame containing the `id` and the percentage of completion for various psychological measures.
#'
#' @examples
#' \dontrun{
#' # Set your Qualtrics API credentials
#' Sys.setenv(QUALTRICS_API_KEY = "your_api_key")
#' Sys.setenv(QUALTRICS_BASE_URL = "your_base_url")
#'
#' # Check the self report completion
#' completion_data <- check_nmap_self_report_completion()
#' print(completion_data)
#' }
#'
#' @importFrom qualtRics fetch_survey
#' @importFrom dplyr rename select mutate arrange matches
#' @author
#' Rachel Velasquez
#'
#' @export


check_nmap_self_report_completion <- function(...) {

  # checks if Qualtrics API Key is defined in environment
  if (Sys.getenv("QUALTRICS_API_KEY") <= 1 | Sys.getenv("QUALTRICS_BASE_URL") <= 1) {
    warning("Qualtrics API Key and/or Base URL has/have not been set up. \n Use 'qualtrics_api_credentials()' to configure.")
    return(invisible(NULL))
  }

  # import all self report data from Qualtrics
  self_report <- suppressMessages(qualtRics::fetch_survey(surveyID = "SV_2gwGMWFcer4Sl37", verbose = TRUE, breakout_sets = FALSE))

  self_report$Intro_ID <- sprintf("%03d", as.numeric(self_report$Intro_ID))

  self_report <- self_report |>
    dplyr::rename(id = Intro_ID) |>
    dplyr::select(
      -c(Q322, Q323, Q328, Q329_1, Q329_2, Q655_1, Q655_2, Q2_1, Q2_2, Q325_1, Q657_1, Q326_1, Q326_2, matches("_TEXT")) # remove attention checks and text fields
    ) |>
    dplyr::arrange(id) |>
    dplyr::select(RecordedDate, 18:867)


  # calculate % completion per measure and total # of missing items per ID
  self_report_complete <- self_report |> dplyr::mutate(
    iip_percent      = 1 - rowSums(is.na(self_report[,  6:95])) / 100,
    ctq_percent     = 1 - rowSums(is.na(self_report[, 96:123])) /  28,
    panas_percent  = 1 - rowSums(is.na(self_report[, 124:143])) /  20,
    pid_percent    = 1 - rowSums(is.na(self_report[, 144:243])) / 100,
    asr_percent    = 1 - rowSums(is.na(self_report[, 244:377])) / 134,
    bpq_percent    = 1 - rowSums(is.na(self_report[, 378:457])) /  80,
    fs_percent     = 1 - rowSums(is.na(self_report[, 458:465])) /   8,
    dusi_percent   = 1 - rowSums(is.na(self_report[, 466:480])) /  15,
    isc_percent    = 1 - rowSums(is.na(self_report[, 481:544])) /  64,
    upps_percent   = 1 - rowSums(is.na(self_report[, 545:603])) /  59,
    bfi_percent    = 1 - rowSums(is.na(self_report[, 604:663])) /  60,
    cts_percent    = 1 - rowSums(is.na(self_report[, 664:703])) /  40,
    nih_tb_percent = 1 - rowSums(is.na(self_report[, 704:851])) / 148,
  ) |>
    dplyr::select(id, 852:864)


  return(self_report_complete)

}
