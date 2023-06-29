#' Score the NIH Toolbox Emotion Measures (2019)
#'
#' This function calculates EMOTB scores based on the provided data frame and parameters.
#'
#' @param df The input data frame containing item responses.
#' @param drop_items Logical indicating whether to drop item-level data from the output. Default is FALSE.
#'
#' @return The modified data frame with EMOTB scores.
#'
#' @examples
#' df <- data.frame(
#'   Age = c(20, 15, 25),
#'   Apathy_Raw = c(1, 2, 3),
#'   PercStrs_Raw = c(2, 1, 3),
#'   Self_Raw = c(3, 2, 1)
#' )
#' score_emotb(df)
#'
#' @importFrom dplyr mutate if_else starts_with select mutate_at
#'
#' @author Michael Hallquist, Melanie Glatz
#'
#' @details This function adds two columns, `_Theta` and `_Theta_T`, to `df` containing the NIH Toolbox emotion measures, respectively.
#'
#' @note Scoring is based on the 2019 raw score to t-score conversion tables manual.
#'
#' @note Some measures were omitted from the manual, in which case older versions were used.
#'
#' @note The following age group definitions are used:
#' - TB_measure_adult: 18 years and older
#' - TB_measure_teen: 13-17 years and 8-17 years
#' - TB_measure_child: 8-12 years
#'
#' @seealso For T-score conversion tables, see the [NIH TB Emotion Raw Score to T-Score Conversion Tables Manual](http://www.healthmeasures.net/images/nihtoolbox/Technical_Manuals/Emotion/NIH_TB_Emotion_Raw_Score_to_T-Score_Conversion_Tables_Manual_3.19.19.pdf).
#'
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
