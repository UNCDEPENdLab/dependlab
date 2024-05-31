#' Fetch and Process SCID Survey Data
#'
#' This function fetches survey data from the SCID (Structured Clinical Interview for DSM-5) using the Qualtrics API, processes it for analysis by selecting relevant columns, renaming them for clarity, binding old and new survey data, and recoding the responses. It also allows the option to save the processed dataset to a specified path.
#'
#' @param scid_path Optional; a character string specifying the path to save the processed SCID data frame. If `NULL`, the data frame is not saved. Defaults to `NULL`.
#' @param ... Additional arguments passed to `qualtRics::fetch_survey`.
#'
#' @details
#' The function first fetches SCID-5 survey data from two survey IDs, representing current and old data. It then processes this data by:
#' - Selecting relevant columns for analysis.
#' - Renaming columns to more descriptive names based on the survey questions they represent.
#' - Combining the old and new datasets.
#' - Removing entries without a subject ID.
#' - Recoding response values for clarity in analysis.
#' The final dataset includes a wide range of diagnoses according to the DSM-5 criteria, such as schizophrenia spectrum disorders, bipolar and related disorders, depressive disorders, substance use disorders, and other disorders assessed by the SCID.
#'
#' @return A data frame containing the processed SCID survey responses, with subjects' diagnoses coded for analysis. If `scid_path` is provided, the data frame is also saved to the specified file path.
#'
#' @export
#' @author Rachel Velasquez
#'
#' @examples
#' # Assuming you have set up Qualtrics API credentials
#' \dontrun{
#' scid_data <- get_scid_dx()
#' }
#' # To save the processed data to a specific path
#' \dontrun{
#' scid_data <- get_scid_dx(scid_path = "path/to/save/scid_data.RData")
#' }

get_scid_dx <- function(scid_path = NULL, ...) {

  ### check api setup.

  scid5_current <- qualtRics::fetch_survey(surveyID = "SV_0uK8MWvnEMcBgsC", verbose = TRUE, breakout_sets = FALSE)

  scid5_old <- qualtRics::fetch_survey(surveyID = "SV_8iSvOGbpl8VgiAB", verbose = TRUE, breakout_sets = FALSE)

  # select relevant columns
  scid5_old <- scid5_old |> dplyr::select(RecordedDate, subID, 18:140)
  scid5_current <- scid5_current |> dplyr::select(RecordedDate, subID, 19:141)

  scid5_df <- rbind(scid5_old, scid5_current) # bind old and new surveys
  scid5_df <- scid5_df[!is.na(scid5_df$subID), ] # remove blank surveys

  scid5_df <- scid5_df |> rename( # rename columns to be more descriptive
    # schizophrenia spectrum and other psychotic disorders
    schizophrenia_current = DxsSchizoPsychot_1,
    schizophrenia_past = DxsSchizoPsychot_2,
    schizophreniform_current = DxsSchizoPsychot_3,
    schizophreniform_past = DxsSchizoPsychot_4,
    schizoaffective_current = DxsSchizoPsychot_5,
    schizoaffective_past = DxsSchizoPsychot_6,
    delusional_current = DxsSchizoPsychot_7,
    delusional_past = DxsSchizoPsychot_8,
    brief_psychotic_current = DxsSchizoPsychot_9,
    brief_psychotic_past = DxsSchizoPsychot_10,
    psychotic_medical_lifetime = DxsSchizoPsychot_11,
    psychotic_subst_induced_lifetime = DxsSchizoPsychot_12,
    psychotic_subst_induced_text = DxsSchizoPsychot_12_TEXT,
    schizo_os_current = DxsSchizoPsychot_13,
    schizo_os_current_text = DxsSchizoPsychot_13_TEXT,
    schizo_os_past = DxsSchizoPsychot_14,
    schizo_os_past_text = DxsSchizoPsychot_14_TEXT,
    schizo_us_current = DxsSchizoPsychot_15,
    schizo_us_past = DxsSchizoPsychot_16,
    # bipolar and related disorders
    bipolar_i_mre_manic_current = DxsBipolar_1,
    bipolar_i_mre_manic_past = DxsBipolar_2,
    bipolar_i_mre_depressed_current = DxsBipolar_3,
    bipolar_i_mre_depressed_past = DxsBipolar_4,
    bipolar_i_mre_hypomanic_current = DxsBipolar_5,
    bipolar_i_mre_hypomanic_past = DxsBipolar_6,
    bipolar_i_mre_us_current = DxsBipolar_7,
    bipolar_i_mre_us_past = DxsBipolar_8,
    bipolar_ii_mre_hypomanic_current = DxsBipolar_9,
    bipolar_ii_mre_hypomanic_past = DxsBipolar_10,
    bipolar_ii_mre_depressed_current = DxsBipolar_11,
    bipolar_ii_mre_depressed_past = DxsBipolar_12,
    bipolar_medical_lifetime = DxsBipolar_13,
    bipolar_subst_induced_lifetime = DxsBipolar_14,
    bipolar_subst_induced_text = DxsBipolar_14_TEXT,
    bipolar_os_current = DxsBipolar_15,
    bipolar_os_current_text = DxsBipolar_15_TEXT,
    bipolar_os_past = DxsBipolar_16,
    bipolar_os_past_text = DxsBipolar_16_TEXT,
    bipolar_us_current = DxsBipolar_17,
    bipolar_us_past = DxsBipolar_18,
    # depressive disorders
    mdd_single_ep_current = DxsDeprDisorder_1, # major depressive
    mdd_single_ep_past = DxsDeprDisorder_2,
    mdd_recurrent_ep_current = DxsDeprDisorder_3,
    mdd_recurrent_ep_past =  DxsDeprDisorder_4,
    pdd_current = DxsDeprDisorder_5, # persistent depressive
    depressive_medical_lifetime = DxsDeprDisorder_6,
    depressive_subst_induced_lifetime = DxsDeprDisorder_7,
    depressive_subst_induced_text = DxsDeprDisorder_7_TEXT,
    depressive_os_current = DxsDeprDisorder_8,
    depressive_os_current_text = DxsDeprDisorder_8_TEXT,
    depressive_os_past = DxsDeprDisorder_9,
    depressive_os_past_text =  DxsDeprDisorder_9_TEXT,
    depressive_us_current = DxsDeprDisorder_10,
    depressive_us_past = DxsDeprDisorder_11,
    # substance use disorders
    alcohol = DxsSUD_1,
    sedative = DxsSUD_2,
    sedative_text = DxsSUD_2_TEXT,
    cannabis = DxsSUD_3,
    cannabis_text = DxsSUD_3_TEXT,
    stimulant = DxsSUD_4,
    stimulant_text = DxsSUD_4_TEXT,
    amphetamine = DxsSUD_5,
    amphetamine_text = DxsSUD_5_TEXT,
    cocaine = DxsSUD_6,
    cocaine_text = DxsSUD_6_TEXT,
    os_us_stimulant = DxsSUD_7,
    os_us_stimulant_text = DxsSUD_7_TEXT,
    opioid = DxsSUD_8,
    opioid_text = DxsSUD_8_TEXT,
    phencyclidine = DxsSUD_9,
    phencyclidine_text = DxsSUD_9_TEXT,
    other_hallucinogen = DxsSUD_10,
    other_hallucinogen_text = DxsSUD_10_TEXT,
    inhalant = DxsSUD_11,
    inhalant_text = DxsSUD_11_TEXT,
    other_unknown_subst = DxsSUD_12, # other or unknown substance
    other_unknown_subst_text = DxsSUD_12_TEXT,
    # other disorders
    panic_current = DxsOtherDisorders_1,
    panic_past = DxsOtherDisorders_2,
    agoraphobia = DxsOtherDisorders_3,
    social_anxiety = DxsOtherDisorders_4,
    generalized_anxiety = DxsOtherDisorders_5,
    obsessive_compulsive = DxsOtherDisorders_6,
    ptsd_current = DxsOtherDisorders_7,
    ptsd_past = DxsOtherDisorders_8,
    adhd = DxsOtherDisorders_9,
    adjustment = DxsOtherDisorders_10,
    anxiety_medical_lifetime = DxsOtherDisorders_11,
    anxiety_subst_induced_lifetime = DxsOtherDisorders_12,
    anxiety_subst_induced_text = DxsOtherDisorders_12_TEXT,
    ocd_medical_lifetime = DxsOtherDisorders_13,
    ocd_subst_induced_lifetime = DxsOtherDisorders_14,
    ocd_subst_induced_text = DxsOtherDisorders_14_TEXT,
    other_dsm5_current = DxsOtherDisorders_15,
    other_dsm5_current_text = DxsOtherDisorders_15_TEXT,
    other_dsm5_past = DxsOtherDisorders_16,
    other_dsm5_past_text = DxsOtherDisorders_16_TEXT,
    # screened disorders (scored differently; see below)
    premenstrual_dysphoric = DxsScreenedDx_1,
    specific_phobia = DxsScreenedDx_2,
    specific_phobia_text = DxsScreenedDx_2_TEXT,
    separation_anxiety = DxsScreenedDx_3,
    hoarding = DxsScreenedDx_4,
    body_dysmorphic = DxsScreenedDx_5,
    trichotillomania = DxsScreenedDx_6,
    excoriation = DxsScreenedDx_7,
    insomnia = DxsScreenedDx_8,
    hypersomnolence = DxsScreenedDx_9,
    anorexia_nervosa_restricting = DxsScreenedDx_10,
    anorexia_nervosa_binging_purging = DxsScreenedDx_11,
    bulimia_nervosa = DxsScreenedDx_12,
    binge_eating = DxsScreenedDx_13,
    arfid = DxsScreenedDx_14,
    illness_anxiety = DxsScreenedDx_15,
    intermittent_explosive = DxsScreenedDx_16,
    gambling = DxsScreenedDx_17
  )

  for (i in seq_len(nrow(scid5_df))) {
    if (scid5_df$anySchizo[i] == "No") {
      scid5_df[i, 5:23] <- NA
    }
    if (scid5_df$anyBipolar[i] == "No") {
      scid5_df[i, 25:45] <- NA
    }
    if (scid5_df$anyDepression[i] == "No") {
      scid5_df[i, 47:60] <- NA
    }
    if (scid5_df$anySUD[i] == "No") {
      scid5_df[i, 62:84] <- NA
    }
    if (scid5_df$anyOtherDx[i] == "No") {
      scid5_df[i, 86:105] <- NA
    }
    if (scid5_df$anyScreenedDx[i] == "No") {
      scid5_df[i, 107:124] <- NA
    }
  }

  scid5_df <- scid5_df |> dplyr::select(
    -matches("anySchizo|anyBipolar|anyDepression|anySUD|anyOtherDx|anyScreenedDx|checklistyn|_text")
  ) |>
    mutate_all(as.character) |>
    mutate_at(
      vars(4:76),
      ~recode(.,
              "0 Undetermined" = 0,
              "1 Not Present" = 1,
              "2 Subthreshold" = 2,
              "3 Present" = 3,
              "4 Strongly Present" = 4)
    ) |>
    mutate_at(
              vars(77:93), # scoring for screened disorders only
              ~recode(.,
                      "0 Undetermined" = 0,
                      "1 Not Present" = 1,
                      "2 Likely Present" = 2))



  if (!is.null(scid_path)) {
    save(scid5_df, file = scid_path)
  }


  return(scid5_df)
}
