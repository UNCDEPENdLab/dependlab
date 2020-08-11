#' Score the NIH Toolbox Emotion Measures (2019)
#' This is for the raw score to t-score conversions for bank/fixed forms
#'
#' @param df a data.frame containing the TB Emotion Measures items to be scored
#' @param drop_items whether to remove the item-level data from the \code{
#' df}. Default: FALSE
#'
#' @details
#'
#' Adds two columns, \code{_Theta} and \code{_Theta_T}, to \code{df} containing the nih toolbox emotion measures, respectively.
#'
#' Note: scoring is based on the 2019 raw score to t-score conversion tables manual
#'
#' Note: some measures were omitted from the manual, in which case older versions were used
#'
#' See: \url{http://www.healthmeasures.net/images/nihtoolbox/Technical_Manuals/Emotion/NIH_TB_Emotion_Raw_Score_to_T-Score_Conversion_Tables_Manual_3.19.19.pdf}
#'
#' @export
#' @author Michael Hallquist & Melanie Glatz
#'
#' @importFrom dplyr case_when mutate if_else
#'
#' @return A \code{data.frame} containing the scored emotion toolbox scales


# TB_measure_adult = 18+
# TB_measure_teen = 13-17; 8-17
# TB_measure_child = 8-12


# Apathy Fixed Form Ages 18+
apathy_adult <- function(raw) {
  theta <- case_when(
    raw == 7 ~ -1.3,
    raw == 8 ~ -0.76,
    raw == 9 ~ -0.43,
    raw == 10 ~ -0.18,
    raw == 11 ~ 0.03,
    raw == 12 ~ 0.23,
    raw == 13 ~ 0.41,
    raw == 14 ~ 0.58,
    raw == 15 ~ 0.74,
    raw == 16 ~ 0.90,
    raw == 17 ~ 1.06,
    raw == 18 ~ 1.21,
    raw == 19 ~ 1.37,
    raw == 20 ~ 1.52,
    raw == 21 ~ 1.67,
    raw == 22 ~ 1.83,
    raw == 23 ~ 1.99,
    raw == 24 ~ 2.17,
    raw == 25 ~ 2.35,
    raw == 26 ~ 2.55,
    raw == 27 ~ 2.78,
    raw == 28 ~ 3.05
  )
  return(theta)
}

# Perceived Hostility Bank/Fixed Form Ages 18+
#' @export
perchost_adult <- function(raw) {
  theta <- case_when(
    raw == 8 ~ -1.65,
    raw == 9 ~ -1.14,
    raw == 10 ~ -0.83,
    raw == 11 ~ -0.6,
    raw == 12 ~ -0.4,
    raw == 13 ~ -0.22,
    raw == 14 ~ -0.06,
    raw == 15 ~ 0.09,
    raw == 16 ~ 0.24,
    raw == 17 ~ 0.38,
    raw == 18 ~ 0.52,
    raw == 19 ~ 0.65,
    raw == 20 ~ 0.79,
    raw == 21 ~ 0.93,
    raw == 22 ~ 1.07,
    raw == 23 ~ 1.21,
    raw == 24 ~ 1.35,
    raw == 25 ~ 1.48,
    raw == 26 ~ 1.61,
    raw == 27 ~ 1.74,
    raw == 28 ~ 1.87,
    raw == 29 ~ 1.99,
    raw == 30 ~ 2.11,
    raw == 31 ~ 2.23,
    raw == 32 ~ 2.35,
    raw == 33 ~ 2.47,
    raw == 34 ~ 2.59,
    raw == 35 ~ 2.71,
    raw == 36 ~ 2.85,
    raw == 37 ~ 3,
    raw == 38 ~ 3.17,
    raw == 39 ~ 3.35,
    raw == 40 ~ 3.54
  )
  return(theta)
}

# Perceived Hostility Bank/Fixed Form Ages 8-17
#' @export
perchost_teen <- function(raw) {
  theta <- case_when(
    raw == 5 ~ -1.9,
    raw == 6 ~ -1.39,
    raw == 7 ~ -1.09,
    raw == 8 ~ -.83,
    raw == 9 ~ -.61,
    raw == 10 ~ -.4,
    raw == 11 ~ -.19,
    raw == 12 ~ .03,
    raw == 13 ~ .26,
    raw == 14 ~ .49,
    raw == 15 ~ .73,
    raw == 16 ~ .97,
    raw == 17 ~ 1.19,
    raw == 18 ~ 1.39,
    raw == 19 ~ 1.57,
    raw == 20 ~ 1.75,
    raw == 21 ~ 1.92,
    raw == 22 ~ 2.1,
    raw == 23 ~ 2.3,
    raw == 24 ~ 2.52,
    raw == 25 ~ 2.87
  )
  return(theta)
}

# Anger - Physical Aggression Fixed Form Ages 18+
#' @export
angpa_adult <- function(raw) {
  theta <- case_when(
    raw == 5 ~ -0.66,
    raw == 6 ~ 0.08,
    raw == 7 ~ 0.35,
    raw == 8 ~ 0.44,
    raw == 9 ~ 0.6,
    raw == 10 ~ 0.77,
    raw == 11 ~ 0.83,
    raw == 12 ~ 1.03,
    raw == 13 ~ 1.15,
    raw == 14 ~ 1.25,
    raw == 15 ~ 1.35,
    raw == 16 ~ 1.44,
    raw == 17 ~ 1.53,
    raw == 18 ~ 1.62,
    raw == 19 ~ 1.7,
    raw == 20 ~ 1.79,
    raw == 21 ~ 1.87,
    raw == 22 ~ 1.95,
    raw == 23 ~ 2.03,
    raw == 24 ~ 2.11,
    raw == 25 ~ 2.19,
    raw == 26 ~ 2.27,
    raw == 27 ~ 2.36,
    raw == 28 ~ 2.44,
    raw == 29 ~ 2.54,
    raw == 30 ~ 2.63,
    raw == 31 ~ 2.72,
    raw == 32 ~ 2.84,
    raw == 33 ~ 2.95,
    raw == 34 ~ 3.07,
    raw == 35 ~ 3.3
  )
  return(theta)
}

# Anger - Hostility Fixed Form Ages 18+
#' @export
anghost_adult <- function(raw) {
  theta <- case_when(
    raw == 5 ~ -1.34,
    raw == 6 ~ -0.75,
    raw == 7 ~ -0.43,
    raw == 8 ~ -0.21,
    raw == 9 ~ -0.04,
    raw == 10 ~ 0.12,
    raw == 11 ~ 0.26,
    raw == 12 ~ 0.39,
    raw == 13 ~ 0.51,
    raw == 14 ~ 0.63,
    raw == 15 ~ 0.74,
    raw == 16 ~ 0.85,
    raw == 17 ~ 0.96,
    raw == 18 ~ 1.06,
    raw == 19 ~ 1.16,
    raw == 20 ~ 1.25,
    raw == 21 ~ 1.35,
    raw == 22 ~ 1.44,
    raw == 23 ~ 1.54,
    raw == 24 ~ 1.63,
    raw == 25 ~ 1.73,
    raw == 26 ~ 1.82,
    raw == 27 ~ 1.92,
    raw == 28 ~ 2.04,
    raw == 29 ~ 2.11,
    raw == 30 ~ 2.27,
    raw == 31 ~ 2.28,
    raw == 32 ~ 2.5,
    raw == 33 ~ 2.66,
    raw == 34 ~ 2.79,
    raw == 35 ~ 3.02
  )
  return(theta)
}

# Anger - Affect Fixed Form Ages 18+ (Anger FF Ages 18+)
#' @export
angaf_adult <- function(raw) {
  theta <- case_when(
    raw == 5 ~ -1.71,
    raw == 6 ~ -1.19,
    raw == 7 ~ -0.87,
    raw == 8 ~ -0.6,
    raw == 9 ~ -0.37,
    raw == 10 ~ -0.16,
    raw == 11 ~ 0.05,
    raw == 12 ~ 0.26,
    raw == 13 ~ 0.47,
    raw == 14 ~ 0.67,
    raw == 15 ~ 0.88,
    raw == 16 ~ 1.08,
    raw == 17 ~ 1.29,
    raw == 18 ~ 1.5,
    raw == 19 ~ 1.72,
    raw == 20 ~ 1.94,
    raw == 21 ~ 2.17,
    raw == 22 ~ 2.41,
    raw == 23 ~ 2.68,
    raw == 24 ~ 2.96,
    raw == 25 ~ 3.28
  )
  return(theta)
}

# Anger - Affect Bank/Fixed Form Ages 8-17
#' @export
angaf_teen <- function(raw) {
  theta <- case_when(
    raw == 5 ~ -1.85,
    raw == 6 ~ -1.36,
    raw == 7 ~ -1.04,
    raw == 8 ~ -0.75,
    raw == 9 ~ -0.48,
    raw == 10 ~ -0.24,
    raw == 11 ~ -0.01,
    raw == 12 ~ 0.21,
    raw == 13 ~ 0.42,
    raw == 14 ~ 0.63,
    raw == 15 ~ 0.83,
    raw == 16 ~ 1.03,
    raw == 17 ~ 1.23,
    raw == 18 ~ 1.43,
    raw == 19 ~ 1.63,
    raw == 20 ~ 1.84,
    raw == 21 ~ 2.04,
    raw == 22 ~ 2.26,
    raw == 23 ~ 2.49,
    raw == 24 ~ 2.75,
    raw == 25 ~ 3.02
  )
  return(theta)
}

# Emotional Support Bank/Fixed Form Ages 18+
#' @export
emosup_adult <- function(raw) {
  theta <- case_when(
    raw == 8 ~ -3.41,
    raw == 9 ~ -3.17,
    raw == 10 ~ -2.99,
    raw == 11 ~ -2.84,
    raw == 12 ~ -2.72,
    raw == 13 ~ -2.6,
    raw == 14 ~ -2.49,
    raw == 15 ~ -2.38,
    raw == 16 ~ -2.28,
    raw == 17 ~ -2.18,
    raw == 18 ~ -2.08,
    raw == 19 ~ -1.98,
    raw == 20 ~ -1.87,
    raw == 21 ~ -1.76,
    raw == 22 ~ -1.65,
    raw == 23 ~ -1.53,
    raw == 24 ~ -1.42,
    raw == 25 ~ -1.3,
    raw == 26 ~ -1.19,
    raw == 27 ~ -1.07,
    raw == 28 ~ -0.96,
    raw == 29 ~ -0.85,
    raw == 30 ~ -0.73,
    raw == 31 ~ -0.62,
    raw == 32 ~ -0.5,
    raw == 33 ~ -0.39,
    raw == 34 ~ -0.26,
    raw == 35 ~ -0.14,
    raw == 36 ~ 0,
    raw == 37 ~ 0.16,
    raw == 38 ~ 0.36,
    raw == 39 ~ 0.65,
    raw == 40 ~ 1.25
  )
  return(theta)
}

# Emotional Support Bank/Fixed Form Ages 8-17
#' @export
emosup_teen <- function(raw) {
  theta <- case_when(
    raw == 7 ~ -3.01,
    raw == 8 ~ -2.72,
    raw == 9 ~ -2.56,
    raw == 10 ~ -2.41,
    raw == 11 ~ -2.28,
    raw == 12 ~ -2.16,
    raw == 13 ~ -2.05,
    raw == 14 ~ -1.94,
    raw == 15 ~ -1.82,
    raw == 16 ~ -1.7,
    raw == 17 ~ -1.58,
    raw == 18 ~ -1.46,
    raw == 19 ~ -1.33,
    raw == 20 ~ -1.2,
    raw == 21 ~ -1.06,
    raw == 22 ~ -.91,
    raw == 23 ~ -.77,
    raw == 24 ~ -.62,
    raw == 25 ~ -.48,
    raw == 26 ~ -.33,
    raw == 27 ~ -.18,
    raw == 28 ~ -.03,
    raw == 29 ~ .14,
    raw == 30 ~ .32,
    raw == 31 ~ .53,
    raw == 32 ~ .82,
    raw == 33 ~ 1.38
  )
  return(theta)
}

# Fear - Affect Fixed Form Ages 18+
#' @export
fearaf_adult <- function(raw) {
  theta <- case_when(
    raw == 7 ~ -1.37,
    raw == 8 ~ -0.79,
    raw == 9 ~ -0.53,
    raw == 10 ~ -0.33,
    raw == 11 ~ -0.16,
    raw == 12 ~ -0.01,
    raw == 13 ~ 0.13,
    raw == 14 ~ 0.26,
    raw == 15 ~ 0.38,
    raw == 16 ~ 0.51,
    raw == 17 ~ 0.63,
    raw == 18 ~ 0.76,
    raw == 19 ~ 0.88,
    raw == 20 ~ 1,
    raw == 21 ~ 1.13,
    raw == 22 ~ 1.26,
    raw == 23 ~ 1.38,
    raw == 24 ~ 1.51,
    raw == 25 ~ 1.64,
    raw == 26 ~ 1.77,
    raw == 27 ~ 1.89,
    raw == 28 ~ 2.02,
    raw == 29 ~ 2.15,
    raw == 30 ~ 2.29,
    raw == 31 ~ 2.43,
    raw == 32 ~ 2.58,
    raw == 33 ~ 2.74,
    raw == 34 ~ 2.94,
    raw == 35 ~ 3.24
  )
  return(theta)
}

# Fear - Bank/Fixed Form Ages 8-17
#' @export
fearaf_teen <- function(raw) {
  theta <- case_when(
    raw == 8 ~ -1.65,
    raw == 9 ~ -1.2,
    raw == 10 ~ -0.94,
    raw == 11 ~ -0.7,
    raw == 12 ~ -0.51,
    raw == 13 ~ -0.33,
    raw == 14 ~ -0.17,
    raw == 15 ~ -0.02,
    raw == 16 ~ 0.12,
    raw == 17 ~ 0.25,
    raw == 18 ~ 0.38,
    raw == 19 ~ 0.51,
    raw == 20 ~ 0.63,
    raw == 21 ~ 0.75,
    raw == 22 ~ 0.87,
    raw == 23 ~ 0.99,
    raw == 24 ~ 1.1,
    raw == 25 ~ 1.22,
    raw == 26 ~ 1.34,
    raw == 27 ~ 1.45,
    raw == 28 ~ 1.57,
    raw == 29 ~ 1.69,
    raw == 30 ~ 1.81,
    raw == 31 ~ 1.93,
    raw == 32 ~ 2.06,
    raw == 33 ~ 2.18,
    raw == 34 ~ 2.32,
    raw == 35 ~ 2.46,
    raw == 36 ~ 2.6,
    raw == 37 ~ 2.76,
    raw == 38 ~ 2.93,
    raw == 39 ~ 3.11,
    raw == 40 ~ 3.32
  )
  return(theta)
}

# Fear - Somatic Arousal Bank/Fixed Form Ages 18+
#' @export
fearsoma_adult <- function(raw) {
  theta <- case_when(
    raw == 6 ~ -0.99,
    raw == 7 ~ -0.44,
    raw == 8 ~ -0.05,
    raw == 9 ~ 0.26,
    raw == 10 ~ 0.52,
    raw == 11 ~ 0.77,
    raw == 12 ~ 0.99,
    raw == 13 ~ 1.2,
    raw == 14 ~ 1.39,
    raw == 15 ~ 1.58,
    raw == 16 ~ 1.76,
    raw == 17 ~ 1.94,
    raw == 18 ~ 2.11,
    raw == 19 ~ 2.29,
    raw == 20 ~ 2.46,
    raw == 21 ~ 2.63,
    raw == 22 ~ 2.79,
    raw == 23 ~ 2.96,
    raw == 24 ~ 3.12,
    raw == 25 ~ 3.26,
    raw == 26 ~ 3.39,
    raw == 27 ~ 3.5,
    raw == 28 ~ 3.59,
    raw == 29 ~ 3.66,
    raw == 30 ~ 3.71
  )
  return(theta)
}

# Friendship Bank/Fixed Form Ages 18+
#' @export
friend_adult <- function(raw) {
  theta <- case_when(
    raw == 8 ~ -3.2,
    raw == 9 ~ -2.92,
    raw == 10 ~ -2.71,
    raw == 11 ~ -2.53,
    raw == 12 ~ -2.38,
    raw == 13 ~ -2.24,
    raw == 14 ~ -2.11,
    raw == 15 ~ -1.99,
    raw == 16 ~ -1.87,
    raw == 17 ~ -1.75,
    raw == 18 ~ -1.64,
    raw == 19 ~ -1.52,
    raw == 20 ~ -1.41,
    raw == 21 ~ -1.29,
    raw == 22 ~ -1.18,
    raw == 23 ~ -1.07,
    raw == 24 ~ -0.95,
    raw == 25 ~ -0.84,
    raw == 26 ~ -0.72,
    raw == 27 ~ -0.6,
    raw == 28 ~ -0.48,
    raw == 29 ~ -0.36,
    raw == 30 ~ -0.24,
    raw == 31 ~ -0.12,
    raw == 32 ~ 0,
    raw == 33 ~ 0.12,
    raw == 34 ~ 0.25,
    raw == 35 ~ 0.39,
    raw == 36 ~ 0.54,
    raw == 37 ~ 0.71,
    raw == 38 ~ 0.9,
    raw == 39 ~ 1.16,
    raw == 40 ~ 1.65
  )
  return(theta)
}

# Friendship Fixed Form Ages 8-17
#' @export
friend_teen <- function(raw) {
  theta <- case_when(
    raw == 5 ~ -3.34,
    raw == 6 ~ -3.1,
    raw == 7 ~ -2.9,
    raw == 8 ~ -2.71,
    raw == 9 ~ -2.52,
    raw == 10 ~ -2.35,
    raw == 11 ~ -2.18,
    raw == 12 ~ -2.01,
    raw == 13 ~ -1.83,
    raw == 14 ~ -1.66,
    raw == 15 ~ -1.48,
    raw == 16 ~ -1.29,
    raw == 17 ~ -1.1,
    raw == 18 ~ -0.89,
    raw == 19 ~ -0.67,
    raw == 20 ~ -0.44,
    raw == 21 ~ -0.2,
    raw == 22 ~ 0.07,
    raw == 23 ~ 0.39,
    raw == 24 ~ 0.77,
    raw == 25 ~ 1.33
  )
  return(theta)
}

# Instrumental Support Bank/Fixed Form Ages 18+
#' @export
instrsup_adult <- function(raw) {
  theta <- case_when(
    raw == 8 ~ -2.79,
    raw == 9 ~ -2.45,
    raw == 10 ~ -2.27,
    raw == 11 ~ -2.12,
    raw == 12 ~ -2,
    raw == 13 ~ -1.9,
    raw == 14 ~ -1.8,
    raw == 15 ~ -1.71,
    raw == 16 ~ -1.62,
    raw == 17 ~ -1.54,
    raw == 18 ~ -1.46,
    raw == 19 ~ -1.38,
    raw == 20 ~ -1.3,
    raw == 21 ~ -1.22,
    raw == 22 ~ -1.14,
    raw == 23 ~ -1.06,
    raw == 24 ~ -0.98,
    raw == 25 ~ -0.9,
    raw == 26 ~ -0.82,
    raw == 27 ~ -0.73,
    raw == 28 ~ -0.65,
    raw == 29 ~ -0.56,
    raw == 30 ~ -0.47,
    raw == 31 ~ -0.38,
    raw == 32 ~ -0.29,
    raw == 33 ~ -0.19,
    raw == 34 ~ -0.08,
    raw == 35 ~ 0.03,
    raw == 36 ~ 0.15,
    raw == 37 ~ 0.3,
    raw == 38 ~ 0.47,
    raw == 39 ~ 0.72,
    raw == 40 ~ 1.29
  )
  return(theta)
}

# Loneliness Bank/Fixed Form Ages 18+
#' @export
lone_adult <- function(raw) {
  theta <- case_when(
    raw == 5 ~ -1.24,
    raw == 6 ~ -0.62,
    raw == 7 ~ -0.3,
    raw == 8 ~ -0.05,
    raw == 9 ~ 0.17,
    raw == 10 ~ 0.37,
    raw == 11 ~ 0.57,
    raw == 12 ~ 0.77,
    raw == 13 ~ 0.97,
    raw == 14 ~ 1.17,
    raw == 15 ~ 1.37,
    raw == 16 ~ 1.56,
    raw == 17 ~ 1.75,
    raw == 18 ~ 1.93,
    raw == 19 ~ 2.1,
    raw == 20 ~ 2.27,
    raw == 21 ~ 2.43,
    raw == 22 ~ 2.6,
    raw == 23 ~ 2.79,
    raw == 24 ~ 3,
    raw == 25 ~ 3.28
  )
  return(theta)
}

# Loneliness Fixed Form Ages 8-17
#' @export
lone_teen <- function(raw) {
  theta <- case_when(
    raw == 7 ~ -1.12,
    raw == 8 ~ -0.49,
    raw == 9 ~ -0.2,
    raw == 10 ~ 0.02,
    raw == 11 ~ 0.2,
    raw == 12 ~ 0.37,
    raw == 13 ~ 0.52,
    raw == 14 ~ 0.67,
    raw == 15 ~ 0.81,
    raw == 16 ~ 0.97,
    raw == 17 ~ 1.12,
    raw == 18 ~ 1.27,
    raw == 19 ~ 1.42,
    raw == 20 ~ 1.56,
    raw == 21 ~ 1.71,
    raw == 22 ~ 1.85,
    raw == 23 ~ 1.99,
    raw == 24 ~ 2.13,
    raw == 25 ~ 2.25,
    raw == 26 ~ 2.38,
    raw == 27 ~ 2.49,
    raw == 28 ~ 2.6,
    raw == 29 ~ 2.71,
    raw == 30 ~ 2.82,
    raw == 31 ~ 2.94,
    raw == 32 ~ 3.07,
    raw == 33 ~ 3.21,
    raw == 34 ~ 3.34,
    raw == 35 ~ 3.52
  )
  return(theta)
}

# Perceived Rejection Bank/Fixed Form Ages 18+
#' @export
percrej_adult <- function(raw) {
  theta <- case_when(
    raw == 8 ~ -1.41,
    raw == 9 ~ -0.88,
    raw == 10 ~ -0.59,
    raw == 11 ~ -0.37,
    raw == 12 ~ -0.19,
    raw == 13 ~ -0.03,
    raw == 14 ~ 0.12,
    raw == 15 ~ 0.26,
    raw == 16 ~ 0.39,
    raw == 17 ~ 0.53,
    raw == 18 ~ 0.66,
    raw == 19 ~ 0.79,
    raw == 20 ~ 0.93,
    raw == 21 ~ 1.06,
    raw == 22 ~ 1.19,
    raw == 23 ~ 1.32,
    raw == 24 ~ 1.45,
    raw == 25 ~ 1.58,
    raw == 26 ~ 1.7,
    raw == 27 ~ 1.83,
    raw == 28 ~ 1.95,
    raw == 29 ~ 2.07,
    raw == 30 ~ 2.18,
    raw == 31 ~ 2.29,
    raw == 32 ~ 2.4,
    raw == 33 ~ 2.52,
    raw == 34 ~ 2.63,
    raw == 35 ~ 2.75,
    raw == 36 ~ 2.87,
    raw == 37 ~ 3.01,
    raw == 38 ~ 3.16,
    raw == 39 ~ 3.33,
    raw == 40 ~ 3.52
  )
  return(theta)
}

# Perceived Rejection Bank/Fixed Form Ages 8-17
#' @export
percrej_teen <- function(raw) {
  theta <- case_when(
    raw == 5 ~ -1.2,
    raw == 6 ~ -0.57,
    raw == 7 ~ -0.27,
    raw == 8 ~ -0.03,
    raw == 9 ~ 0.18,
    raw == 10 ~ 0.37,
    raw == 11 ~ 0.55,
    raw == 12 ~ 0.74,
    raw == 13 ~ 0.92,
    raw == 14 ~ 1.12,
    raw == 15 ~ 1.31,
    raw == 16 ~ 1.49,
    raw == 17 ~ 1.66,
    raw == 18 ~ 1.83,
    raw == 19 ~ 1.99,
    raw == 20 ~ 2.15,
    raw == 21 ~ 2.3,
    raw == 22 ~ 2.46,
    raw == 23 ~ 2.63,
    raw == 24 ~ 2.83,
    raw == 25 ~ 3.14
  )
  return(theta)
}

# Perceived Stress Bank/Fixed Form Ages 18+
#' @export
percstrs_adult <- function(raw) {
  theta <- case_when(
    raw == 10 ~ -2.76,
    raw == 11 ~ -2.45,
    raw == 12 ~ -2.19,
    raw == 13 ~ -1.95,
    raw == 14 ~ -1.74,
    raw == 15 ~ -1.54,
    raw == 16 ~ -1.36,
    raw == 17 ~ -1.18,
    raw == 18 ~ -1.01,
    raw == 19 ~ -0.85,
    raw == 20 ~ -0.69,
    raw == 21 ~ -0.53,
    raw == 22 ~ -0.37,
    raw == 23 ~ -0.22,
    raw == 24 ~ -0.07,
    raw == 25 ~ 0.09,
    raw == 26 ~ 0.24,
    raw == 27 ~ 0.39,
    raw == 28 ~ 0.54,
    raw == 29 ~ 0.69,
    raw == 30 ~ 0.83,
    raw == 31 ~ 0.98,
    raw == 32 ~ 1.12,
    raw == 33 ~ 1.26,
    raw == 34 ~ 1.4,
    raw == 35 ~ 1.54,
    raw == 36 ~ 1.68,
    raw == 37 ~ 1.82,
    raw == 38 ~ 1.96,
    raw == 39 ~ 2.09,
    raw == 40 ~ 2.23,
    raw == 41 ~ 2.37,
    raw == 42 ~ 2.51,
    raw == 43 ~ 2.65,
    raw == 44 ~ 2.8,
    raw == 45 ~ 2.95,
    raw == 46 ~ 3.1,
    raw == 47 ~ 3.25,
    raw == 48 ~ 3.38,
    raw == 49 ~ 3.51,
    raw == 50 ~ 3.61
  )
  return(theta)
}

# Perceived Stress Bank/Fixed Form Ages 13-17
#' @export
percstrs_teen <- function(raw) {
  theta <- case_when(
    raw == 10 ~ -3.04,
    raw == 11 ~ -2.75,
    raw == 12 ~ -2.5,
    raw == 13 ~ -2.27,
    raw == 14 ~ -2.06,
    raw == 15 ~ -1.87,
    raw == 16 ~ -1.69,
    raw == 17 ~ -1.51,
    raw == 18 ~ -1.34,
    raw == 19 ~ -1.18,
    raw == 20 ~ -1.01,
    raw == 21 ~ -0.85,
    raw == 22 ~ -0.69,
    raw == 23 ~ -0.53,
    raw == 24 ~ -0.38,
    raw == 25 ~ -0.22,
    raw == 26 ~ -0.06,
    raw == 27 ~ 0.09,
    raw == 28 ~ 0.25,
    raw == 29 ~ 0.4,
    raw == 30 ~ 0.55,
    raw == 31 ~ 0.7,
    raw == 32 ~ 0.85,
    raw == 33 ~ 1,
    raw == 34 ~ 1.14,
    raw == 35 ~ 1.29,
    raw == 36 ~ 1.43,
    raw == 37 ~ 1.57,
    raw == 38 ~ 1.71,
    raw == 39 ~ 1.85,
    raw == 40 ~ 1.99,
    raw == 41 ~ 2.13,
    raw == 42 ~ 2.27,
    raw == 43 ~ 2.43,
    raw == 44 ~ 2.58,
    raw == 45 ~ 2.75,
    raw == 46 ~ 2.92,
    raw == 47 ~ 3.1,
    raw == 48 ~ 3.28,
    raw == 49 ~ 3.44,
    raw == 50 ~ 3.57
  )
  return(theta)
}

# Self-Efficacy Bank/Fixed Form Ages 18+
#' @export
self_adult <- function(raw) {
  theta <- case_when(
    raw == 10 ~ -3.26,
    raw == 11 ~ -3.02,
    raw == 12 ~ -2.81,
    raw == 13 ~ -2.63,
    raw == 14 ~ -2.47,
    raw == 15 ~ -2.32,
    raw == 16 ~ -2.17,
    raw == 17 ~ -2.01,
    raw == 18 ~ -1.85,
    raw == 19 ~ -1.69,
    raw == 20 ~ -1.54,
    raw == 21 ~ -1.39,
    raw == 22 ~ -1.25,
    raw == 23 ~ -1.11,
    raw == 24 ~ -0.97,
    raw == 25 ~ -0.83,
    raw == 26 ~ -0.7,
    raw == 27 ~ -0.56,
    raw == 28 ~ -0.42,
    raw == 29 ~ -0.28,
    raw == 30 ~ -0.14,
    raw == 31 ~ 0,
    raw == 32 ~ 0.14,
    raw == 33 ~ 0.28,
    raw == 34 ~ 0.42,
    raw == 35 ~ 0.58,
    raw == 36 ~ 0.74,
    raw == 37 ~ 0.94,
    raw == 38 ~ 1.2,
    raw == 39 ~ 1.5,
    raw == 40 ~ 1.84
  )
  return(theta)
}

# Self-Efficacy Bank/Fixed Form Ages 13-17
#' @export
self_teen <- function(raw) {
  theta <- case_when(
    raw == 10 ~ - 3.71,
    raw == 11 ~ -3.63,
    raw == 12 ~ -3.52,
    raw == 13 ~ -3.4,
    raw == 14 ~ -3.27,
    raw == 15 ~ -3.14,
    raw == 16 ~ -3.01,
    raw == 17 ~ -2.88,
    raw == 18 ~ -2.76,
    raw == 19 ~ -2.64,
    raw == 20 ~ -2.52,
    raw == 21 ~ -2.4,
    raw == 22 ~ -2.28,
    raw == 23 ~ -2.15,
    raw == 24 ~ -2.03,
    raw == 25 ~ -1.9,
    raw == 26 ~ -1.77,
    raw == 27 ~ -1.64,
    raw == 28 ~ -1.5,
    raw == 29 ~ -1.37,
    raw == 30 ~ -1.23,
    raw == 31 ~ -1.09,
    raw == 32 ~ -0.95,
    raw == 33 ~ -0.81,
    raw == 34 ~ -0.67,
    raw == 35 ~ -0.54,
    raw == 36 ~ -0.4,
    raw == 37 ~ -0.26,
    raw == 38 ~ -0.13,
    raw == 39 ~ 0.01,
    raw == 40 ~ 0.14,
    raw == 41 ~ 0.28,
    raw == 42 ~ 0.42,
    raw == 43 ~ 0.57,
    raw == 44 ~ 0.72,
    raw == 45 ~ 0.88,
    raw == 46 ~ 1.06,
    raw == 47 ~ 1.26,
    raw == 48 ~ 1.48,
    raw == 49 ~ 1.75,
    raw == 50 ~ 2.11
  )
  return(theta)
}

# Self-Efficacy Bank/Fixed Form Child 8-12
#' @export
self_child <- function(raw) {
  theta <- case_when(
    raw == 10 ~ -3.23,
    raw == 11 ~ -3.01,
    raw == 12 ~ -2.85,
    raw == 13 ~ -2.69,
    raw == 14 ~ -2.54,
    raw == 15 ~ -2.41,
    raw == 16 ~ -2.28,
    raw == 17 ~ -2.16,
    raw == 18 ~ -2.04,
    raw == 19 ~ -1.93,
    raw == 20 ~ -1.82,
    raw == 21 ~ -1.71,
    raw == 22 ~ -1.6,
    raw == 23 ~ -1.49,
    raw == 24 ~ -1.38,
    raw == 25 ~ -1.27,
    raw == 26 ~ -1.16,
    raw == 27 ~ -1.04,
    raw == 28 ~ -0.93,
    raw == 29 ~ -0.82,
    raw == 30 ~ -0.71,
    raw == 31 ~ -0.59,
    raw == 32 ~ -0.48,
    raw == 33 ~ -0.37,
    raw == 34 ~ -0.25,
    raw == 35 ~ -0.14,
    raw == 36 ~ -0.03,
    raw == 37 ~ 0.09,
    raw == 38 ~ 0.2,
    raw == 39 ~ 0.32,
    raw == 40 ~ 0.44,
    raw == 41 ~ 0.56,
    raw == 42 ~ 0.69,
    raw == 43 ~ 0.82,
    raw == 44 ~ 0.96,
    raw == 45 ~ 1.11,
    raw == 46 ~ 1.27,
    raw == 47 ~ 1.45,
    raw == 48 ~ 1.66,
    raw == 49 ~ 1.88,
    raw == 50 ~ 2.21
  )
  return(theta)
}

# Meaning and Purpose Fixed Form Ages 18+
#' @export
mp_adult <- function(raw) {
  theta <- case_when(
    raw == 7 ~ -3.57,
    raw == 8 ~ -3.48,
    raw == 9 ~ -3.32,
    raw == 10 ~ -3.13,
    raw == 11 ~ -2.94,
    raw == 12 ~ -2.74,
    raw == 13 ~ -2.56,
    raw == 14 ~ -2.39,
    raw == 15 ~ -2.22,
    raw == 16 ~ -2.06,
    raw == 17 ~ -1.9,
    raw == 18 ~ -1.75,
    raw == 19 ~ -1.59,
    raw == 20 ~ -1.43,
    raw == 21 ~ -1.27,
    raw == 22 ~ -1.11,
    raw == 23 ~ -0.95,
    raw == 24 ~ -0.78,
    raw == 25 ~ -0.6,
    raw == 26 ~ -0.41,
    raw == 27 ~ -0.22,
    raw == 28 ~ -0.01,
    raw == 29 ~ 0.2,
    raw == 30 ~ 0.42,
    raw == 31 ~ 0.66,
    raw == 32 ~ 0.92,
    raw == 33 ~ 1.2,
    raw == 34 ~ 1.53,
    raw == 35 ~ 1.94
  )
  return(theta)
}

# Sadness Fixed Form Ages 18+
#' @export
sad_adult <- function(raw) {
  theta <- case_when(
    raw == 8 ~ -1.29,
    raw == 9 ~ -0.67,
    raw == 10 ~ -0.38,
    raw == 11 ~ -0.18,
    raw == 12 ~ -0.02,
    raw == 13 ~ 0.12,
    raw == 14 ~ 0.23,
    raw == 15 ~ 0.34,
    raw == 16 ~ 0.43,
    raw == 17 ~ 0.53,
    raw == 18 ~ 0.62,
    raw == 19 ~ 0.71,
    raw == 20 ~ 0.79,
    raw == 21 ~ 0.88,
    raw == 22 ~ 0.97,
    raw == 23 ~ 1.07,
    raw == 24 ~ 1.16,
    raw == 25 ~ 1.25,
    raw == 26 ~ 1.35,
    raw == 27 ~ 1.44,
    raw == 28 ~ 1.54,
    raw == 29 ~ 1.64,
    raw == 30 ~ 1.74,
    raw == 31 ~ 1.83,
    raw == 32 ~ 1.93,
    raw == 33 ~ 2.04,
    raw == 34 ~ 2.14,
    raw == 35 ~ 2.25,
    raw == 36 ~ 2.36,
    raw == 37 ~ 2.48,
    raw == 38 ~ 2.62,
    raw == 39 ~ 2.79,
    raw == 40 ~ 3.09
  )
  return(theta)
}

# Sadness Fixed Form Ages 8-17
#' @export
sad_teen <- function(raw) {
  theta <- case_when(
    raw == 8 ~ -1.48,
    raw == 9 ~ -0.96,
    raw == 10 ~ -0.68,
    raw == 11 ~ -0.45,
    raw == 12 ~ -0.26,
    raw == 13 ~ -0.09,
    raw == 14 ~ 0.06,
    raw == 15 ~.2,
    raw == 16 ~.33,
    raw == 17 ~.45,
    raw == 18 ~.57,
    raw == 19 ~.68,
    raw == 20 ~ 0.79,
    raw == 21 ~ 0.9,
    raw == 22 ~ 1,
    raw == 23 ~ 1.11,
    raw == 24 ~ 1.21,
    raw == 25 ~ 1.31,
    raw == 26 ~ 1.41,
    raw == 27 ~ 1.51,
    raw == 28 ~ 1.61,
    raw == 29 ~ 1.72,
    raw == 30 ~ 1.82,
    raw == 31 ~ 1.93,
    raw == 32 ~ 2.03,
    raw == 33 ~ 2.14,
    raw == 34 ~ 2.26,
    raw == 35 ~ 2.38,
    raw == 36 ~ 2.51,
    raw == 37 ~ 2.65,
    raw == 38 ~ 2.81,
    raw == 39 ~ 2.99,
    raw == 40 ~ 3.23
  )
  return(theta)
}

# General Life Satisfaction Fixed Form A Ages 18+
#' @export
gls_adult <- function(raw) {
  theta <- case_when(
    raw == 5 ~ -2.7,
    raw == 6 ~ -2.31,
    raw == 7 ~ -2.06,
    raw == 8 ~ -1.86,
    raw == 9 ~ -1.69,
    raw == 10 ~ -1.55,
    raw == 11 ~ -1.42,
    raw == 12 ~ -1.29,
    raw == 13 ~ -1.18,
    raw == 14 ~ -1.07,
    raw == 15 ~ -0.96,
    raw == 16 ~ -0.86,
    raw == 17 ~ -0.76,
    raw == 18 ~ -0.65,
    raw == 19 ~ -0.55,
    raw == 20 ~ -0.45,
    raw == 21 ~ -0.34,
    raw == 22 ~ -0.23,
    raw == 23 ~ -0.12,
    raw == 24 ~ 0,
    raw == 25 ~ 0.13,
    raw == 26 ~ 0.25,
    raw == 27 ~ 0.38,
    raw == 28 ~ 0.51,
    raw == 29 ~ 0.67,
    raw == 30 ~ 0.86,
    raw == 31 ~ 1.07,
    raw == 32 ~ 1.31,
    raw == 33 ~ 1.6,
    raw == 34 ~ 1.93,
    raw == 35 ~ 2.34
  )
  return(theta)
}

# General Life Satisfaction Bank/Fixed Form Ages 13-17
#' @export
gls_teen <- function(raw) {
  theta <- case_when(
    raw == 10 ~ -3.46,
    raw == 11 ~ -3.27,
    raw == 12 ~ -3.08,
    raw == 13 ~ -2.91,
    raw == 14 ~ -2.76,
    raw == 15 ~ -2.63,
    raw == 16 ~ -2.5,
    raw == 17 ~ -2.39,
    raw == 18 ~ -2.28,
    raw == 19 ~ -2.18,
    raw == 20 ~ -2.08,
    raw == 21 ~ -1.98,
    raw == 22 ~ -1.89,
    raw == 23 ~ -1.8,
    raw == 24 ~ -1.71,
    raw == 25 ~ -1.62,
    raw == 26 ~ -1.54,
    raw == 27 ~ -1.45,
    raw == 28 ~ -1.37,
    raw == 29 ~ -1.29,
    raw == 30 ~ -1.2,
    raw == 31 ~ -1.12,
    raw == 32 ~ -1.04,
    raw == 33 ~ -0.96,
    raw == 34 ~ -0.88,
    raw == 35 ~ -0.8,
    raw == 36 ~ -0.71,
    raw == 37 ~ -0.63,
    raw == 38 ~ -0.54,
    raw == 39 ~ -0.46,
    raw == 40 ~ -0.37,
    raw == 41 ~ -0.28,
    raw == 42 ~ -0.19,
    raw == 43 ~ -0.1,
    raw == 44 ~ 0,
    raw == 45 ~ 0.1,
    raw == 46 ~ 0.19,
    raw == 47 ~ 0.29,
    raw == 48 ~ 0.4,
    raw == 49 ~ 0.51,
    raw == 50 ~ 0.62,
    raw == 51 ~ 0.74,
    raw == 52 ~ 0.86,
    raw == 53 ~ 0.99,
    raw == 54 ~ 1.12,
    raw == 55 ~ 1.27,
    raw == 56 ~ 1.43,
    raw == 57 ~ 1.6,
    raw == 58 ~ 1.82,
    raw == 59 ~ 2.08,
    raw == 60 ~ 2.42
  )
  return(theta)
}

# General Life Satisfaction Fixed Form Ages 8-12
#' @export
gls_child <- function(raw) {
  theta <- case_when(
    raw == 5 ~ -3.07,
    raw ==6 ~ -2.8,
    raw == 7 ~ -2.57,
    raw == 8 ~ -2.38,
    raw == 9 ~ -2.2,
    raw == 10 ~ -2.03,
    raw == 11 ~ -1.86,
    raw == 12 ~ -1.7,
    raw == 13 ~ -1.53,
    raw == 14 ~ -1.36,
    raw == 15 ~ -1.18,
    raw == 16 ~ -1,
    raw == 17 ~ -0.8,
    raw == 18 ~ -0.61,
    raw == 19 ~ -0.41,
    raw == 20 ~ -0.19,
    raw == 21 ~ 0.06,
    raw == 22 ~ 0.34,
    raw == 23 ~ 0.66,
    raw == 24 ~ 1.03,
    raw == 25 ~ 1.54
  )
  return(theta)
}

# Positive Affect Fixed Form Ages 18+
#' @export
posaf_adult <- function(raw) {
  theta <- case_when(
    raw == 15 ~ -3.56,
    raw == 16 ~ -3.43,
    raw == 17 ~ -3.27,
    raw == 18 ~ -3.13,
    raw == 19 ~ -2.99,
    raw == 20 ~ -2.87,
    raw == 21 ~ -2.75,
    raw == 22 ~ -2.65,
    raw == 23 ~ -2.55,
    raw == 24 ~ -2.46,
    raw == 25 ~ -2.37,
    raw == 26 ~ -2.29,
    raw == 27 ~ -2.21,
    raw == 28 ~ -2.13,
    raw == 29 ~ -2.06,
    raw == 30 ~ -1.98,
    raw == 31 ~ -1.91,
    raw == 32 ~ -1.84,
    raw == 33 ~ -1.77,
    raw == 34 ~ -1.7,
    raw == 35 ~ -1.63,
    raw == 36 ~ -1.56,
    raw == 37 ~ -1.5,
    raw == 38 ~ -1.43,
    raw == 39 ~ -1.36,
    raw == 40 ~ -1.3,
    raw == 41 ~ -1.23,
    raw == 42 ~ -1.17,
    raw == 43 ~ -1.1,
    raw == 44 ~ -1.04,
    raw == 45 ~ -0.97,
    raw == 46 ~ -0.91,
    raw == 47 ~ -0.84,
    raw == 48 ~ -0.78,
    raw == 49 ~ -0.71,
    raw == 50 ~ -0.65,
    raw == 51 ~ -0.58,
    raw == 52 ~ -0.52,
    raw == 53 ~ -0.45,
    raw == 54 ~ -0.38,
    raw == 55 ~ -0.31,
    raw == 56 ~ -0.25,
    raw == 57 ~ -0.18,
    raw == 58 ~ -0.11,
    raw == 59 ~ -0.04,
    raw == 60 ~ 0.03,
    raw == 61 ~ 0.11,
    raw == 62 ~ 0.18,
    raw == 63 ~ 0.26,
    raw == 64 ~ 0.34,
    raw == 65 ~ 0.42,
    raw == 66 ~ 0.5,
    raw == 67 ~ 0.59,
    raw == 68 ~ 0.69,
    raw == 69 ~ 0.8,
    raw == 70 ~ 0.92,
    raw == 71 ~ 1.05,
    raw == 72 ~ 1.21,
    raw == 73 ~ 1.39,
    raw == 74 ~ 1.63,
    raw == 75 ~ 1.99
  )
  return(theta)
}

# Positive Affect Fixed Form Ages 13-17
#' @export
posaf_teen <- function(raw) {
  theta <- case_when(
    raw == 10 ~ -3.41,
    raw == 11 ~ -3.18,
    raw == 12 ~ -2.98,
    raw == 13 ~ -2.82,
    raw == 14 ~ -2.68,
    raw == 15 ~ -2.56,
    raw == 16 ~ -2.44,
    raw == 17 ~ -2.33,
    raw == 18 ~ -2.22,
    raw == 19 ~ -2.11,
    raw == 20 ~ -2.01,
    raw == 21 ~ -1.9,
    raw == 22 ~ -1.8,
    raw == 23 ~ -1.7,
    raw == 24 ~ -1.6,
    raw == 25 ~ -1.5,
    raw == 26 ~ -1.4,
    raw == 27 ~ -1.3,
    raw == 28 ~ -1.19,
    raw == 29 ~ -1.09,
    raw == 30 ~ -0.98,
    raw == 31 ~ -0.88,
    raw == 32 ~ -0.77,
    raw == 33 ~ -0.67,
    raw == 34 ~ -0.56,
    raw == 35 ~ -0.46,
    raw == 36 ~ -0.35,
    raw == 37 ~ -0.24,
    raw == 38 ~ -0.12,
    raw == 39 ~ -0.01,
    raw == 40 ~ 0.11,
    raw == 41 ~ 0.23,
    raw == 42 ~ 0.35,
    raw == 43 ~ 0.48,
    raw == 44 ~ 0.6,
    raw == 45 ~ 0.72,
    raw == 46 ~ 0.85,
    raw == 47 ~ 1,
    raw == 48 ~ 1.18,
    raw == 49 ~ 1.42,
    raw == 50 ~ 1.86
  )
  return(theta)
}

# Positive Affect Fixed Form Ages 8-12
#' @export
posaf_child <- function(raw) {
  theta <- case_when(
    raw == 9 ~ -3.15,
    raw == 10 ~ -2.9,
    raw == 11 ~ -2.72,
    raw == 12 ~ -2.56,
    raw == 13 ~ -2.41,
    raw == 14 ~ -2.28,
    raw == 15 ~ -2.16,
    raw == 16 ~ -2.05,
    raw == 17 ~ -1.94,
    raw == 18 ~ -1.84,
    raw == 19 ~ -1.74,
    raw == 20 ~ -1.64,
    raw == 21 ~ -1.54,
    raw == 22 ~ -1.45,
    raw == 23 ~ -1.35,
    raw == 24 ~ -1.26,
    raw == 25 ~ -1.17,
    raw == 26 ~ -1.07,
    raw == 27 ~ -0.98,
    raw == 28 ~ -0.89,
    raw == 29 ~ -0.79,
    raw == 30 ~ -0.7,
    raw == 31 ~ -0.6,
    raw == 32 ~ -0.5,
    raw == 33 ~ -0.4,
    raw == 34 ~ -0.29,
    raw == 35 ~ -0.18,
    raw == 36 ~ -0.07,
    raw == 37 ~ 0.05,
    raw == 38 ~ 0.17,
    raw == 39 ~ 0.3,
    raw == 40 ~ 0.45,
    raw == 41 ~ 0.61,
    raw == 42 ~ 0.79,
    raw == 43 ~ 1,
    raw == 44 ~ 1.26,
    raw == 45 ~ 1.7
  )
  return(theta)
}



# Parent function to call sub functions and create variables (Theta and T)
#' @export
score_emotb <- function(df, drop_items=FALSE) {
  df <- df %>% mutate(Apathy_Theta = if_else(Age >= 18, apathy_adult(Apathy_Raw), NA_real_),
                      PercStrs_Theta = if_else(Age >= 18, percstrs_adult(PercStrs_Raw), percstrs_teen(PercStrs_Raw)),
                      Self_Theta = if_else(Age >= 18, self_adult(Self_Raw), if_else(Age <= 12, self_child(Self_Raw), self_teen(Self_Raw))),
                      MP_Theta = if_else(Age >= 18, mp_adult(MP_Raw), NA_real_),
                      Sad_Theta = if_else(Age >= 18, sad_adult(Sad_Raw), sad_teen(Sad_Raw)),
                      GLS_Theta = if_else(Age >= 18, gls_adult(GLS_Raw), if_else(Age <= 12, gls_child(GLS_Raw), gls_teen(GLS_Raw))),
                      PosAf_Theta = if_else(Age >= 18, posaf_adult(PosAf_Raw), if_else(Age <= 12, posaf_child(PosAf_Raw), posaf_teen(PosAf_Raw))),
                      AngAf_Theta = if_else(Age >= 18, angaf_adult(AngAf_Raw), angaf_teen(AngAf_Raw)),
                      AngPA_Theta = if_else(Age >= 18, angpa_adult(AngPA_Raw), NA_real_),
                      AngHost_Theta = if_else(Age >= 18, anghost_adult(AngHost_Raw), NA_real_),
                      EmoSup_Theta = if_else(Age >= 18, emosup_adult(EmoSup_Raw), emosup_teen(EmoSup_Raw)),
                      FearAf_Theta = if_else(Age >= 18, fearaf_adult(FearAf_Raw), fearaf_teen(FearAf_Raw)),
                      FearSoma_Theta = if_else(Age >= 18, fearsoma_adult(FearSoma_Raw), NA_real_),
                      Friend_Theta = if_else(Age >= 18, friend_adult(Friend_Raw), friend_teen(Friend_Raw)),
                      InstrSup_Theta = if_else(Age >= 18, instrsup_adult(InstrSup_Raw), NA_real_),
                      Lone_Theta = if_else(Age >= 18, lone_adult(Lone_Raw), lone_teen(Lone_Raw)),
                      PercHost_Theta = if_else(Age >= 18, perchost_adult(PercHost_Raw), perchost_teen(PercHost_Raw)),
                      PercRej_Theta = if_else(Age >= 18, percrej_adult(PercRej_Raw), percrej_teen(PercRej_Raw)))
  theta_to_t <- function(x) { 50 + 10*x }
  df <- df %>% mutate_at(vars(ends_with("_Theta")), list(T=~theta_to_t(.)))

  if (drop_items) { df <- df %>% select(-starts_with("TB_")) }
  return(df)
}




### INCOMPLETE #Domain Specific Life Satisfaction Fixed Form Ages 18+
# NB. NIH indicates there is no scoring for this measure. Items are evaluated individually
#  dls_adult <- function(df) {
#   df <- df %>% mutate(DLS_Theta_A= case_when (
#     TW_DLS_Raw == 13 ~ xxx,
#     TW_DLS_Raw == 14 ~ xxx,
#     TW_DLS_Raw == 15 ~ -3.56,
#     TW_DLS_Raw == 16 ~ -3.43,
#     TW_DLS_Raw == 17 ~ -3.27,
#     TW_DLS_Raw == 18 ~ -3.13,
#     TW_DLS_Raw == 19 ~ -2.99,
#     TW_DLS_Raw == 20 ~ -2.87,
#     TW_DLS_Raw == 21 ~ -2.75,
#     TW_DLS_Raw == 22 ~ -2.65,
#     TW_DLS_Raw == 23 ~ -2.55,
#     TW_DLS_Raw == 24 ~ -2.46,
#     TW_DLS_Raw == 25 ~ -2.37,
#     TW_DLS_Raw == 26 ~ -2.29,
#     TW_DLS_Raw == 27 ~ -2.21,
#     TW_DLS_Raw == 28 ~ -2.13,
#     TW_DLS_Raw == 29 ~ -2.06,
#     TW_DLS_Raw == 30 ~ -1.98,
#     TW_DLS_Raw == 31 ~ -1.91,
#     TW_DLS_Raw == 32 ~ -1.84,
#     TW_DLS_Raw == 33 ~ -1.77,
#     TW_DLS_Raw == 34 ~ -1.7,
#     TW_DLS_Raw == 35 ~ -1.63,
#     TW_DLS_Raw == 36 ~ -1.56,
#     TW_DLS_Raw == 37 ~ -1.5,
#     TW_DLS_Raw == 38 ~ -1.43,
#     TW_DLS_Raw == 39 ~ -1.36,
#     TW_DLS_Raw == 40 ~ -1.3,
#     TW_DLS_Raw == 41 ~ -1.23,
#     TW_DLS_Raw == 42 ~ -1.17,
#     TW_DLS_Raw == 43 ~ -1.1,
#     TW_DLS_Raw == 44 ~ -1.04,
#     TW_DLS_Raw == 45 ~ -0.97,
#     TW_DLS_Raw == 46 ~ -0.91,
#     TW_DLS_Raw == 47 ~ -0.84,
#     TW_DLS_Raw == 48 ~ -0.78,
#     TW_DLS_Raw == 49 ~ -0.71,
#     TW_DLS_Raw == 50 ~ -0.65,
#     TW_DLS_Raw == 51 ~ -0.58,
#     TW_DLS_Raw == 52 ~ -0.52,
#     TW_DLS_Raw == 53 ~ -0.45,
#     TW_DLS_Raw == 54 ~ -0.38,
#     TW_DLS_Raw == 55 ~ -0.31,
#     TW_DLS_Raw == 56 ~ -0.25,
#     TW_DLS_Raw == 57 ~ -0.18,
#     TW_DLS_Raw == 58 ~ -0.11,
#     TW_DLS_Raw == 59 ~ -0.04,
#     TW_DLS_Raw == 60 ~ 0.03,
#     TW_DLS_Raw == 61 ~ 0.11,
#     TW_DLS_Raw == 62 ~ 0.18,
#     TW_DLS_Raw == 63 ~ 0.26,
#     TW_DLS_Raw == 64 ~ 0.34,
#     TW_DLS_Raw == 65 ~ 0.42
#   ))
#   return(df)
# }
