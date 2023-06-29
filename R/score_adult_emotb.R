#' Score Adult EMOTB
#'
#' Computes scores for the NIH Toolbox Emotion Measures, Adult Forms (ages 18-52) (2019).
#'
#' @param df The data frame containing the assessment items.
#' @param scales A character vector specifying the scales to include in the scoring.
#'               The default is "all", which includes all scales. Other possible values are:
#'               "percstrs", "self", "mp", "sad", "gls", "posaf", "angaf", "angpa",
#'               "anghost", "emosup", "fearaf", "fearsoma", "friend", "instrsup",
#'               "lone", "perchost", "percrej", and "apathy".
#' @param max_impute The threshold for mean imputation. Items with missing values
#'                   below this threshold will be mean imputed. The default is 0.2.
#' @param drop_items Logical indicating whether to drop the individual item columns
#'                   from the resulting data frame. The default is FALSE.
#' @param keep_reverse_codes Logical indicating whether to keep the reverse-coded
#'                           items in the resulting data frame. The default is FALSE.
#' @param add_alphas Logical indicating whether to compute Cronbach's alpha coefficients
#'                   for the scales and append them to the resulting data frame. The default is TRUE.
#'
#' @return A data frame with the computed scores and optional alpha coefficients.
#'
#' @details The function scores the Adult EMOTB assessment based on the provided data frame.
#'          It supports various scoring options, including selecting specific scales,
#'          mean imputation of missing values, dropping individual item columns,
#'          keeping reverse-coded items, and computing alpha coefficients.
#'
#'          The function adds three columns per scale to the data frame `df`, containing
#'          the raw scores (\code{_Raw}), the theta scores (\code{_Theta}), and the
#'          t-scores (\code{_T}) for the NIH Toolbox Emotion Measures, respectively.
#'
#'          Note: The scoring is based on the 2019 raw score to t-score conversion tables manual.
#'          Note: Some measures were omitted from the manual, in which case older versions were used.
#'          Note: The function assumes that pasting `TB_`, the subscale, and the item numbers
#'          together yields the items from each of the tests.
#'
#'          See: \url{http://www.healthmeasures.net/images/nihtoolbox/Technical_Manuals/Emotion/NIH_TB_Emotion_Raw_Score_to_T-Score_Conversion_Tables_Manual_3.19.19.pdf}
#'
#' @examples
#' # Score all scales with default options
#' scores <- score_adult_emotb(data_frame)
#'
#' # Score specific scales and keep reverse-coded items
#' scores <- score_adult_emotb(data_frame, scales = c("self", "angaf"), keep_reverse_codes = TRUE)
#'
#' @importFrom dplyr case_when mutate
#'
#' @author Michael Hallquist, Melanie Glatz, Zach Vig
#' @export
score_adult_emotb <- function(df, scales="all", max_impute=0.2,
                              drop_items=FALSE, keep_reverse_codes=FALSE,
                              add_alphas=TRUE) {

  #set-up logicals
  if("percstrs" %in% scales | "all" %in% scales)  percstrs <- TRUE  else  percstrs <- FALSE
  if("self" %in% scales | "all" %in% scales)      self <- TRUE      else  self <- FALSE
  if("mp" %in% scales | "all" %in% scales)        mp <- TRUE        else  mp <- FALSE
  if("sad" %in% scales | "all" %in% scales)       sad <- TRUE       else  sad <- FALSE
  if("gls" %in% scales | "all" %in% scales)       gls <- TRUE       else  gls <- FALSE
  if("posaf" %in% scales | "all" %in% scales)     posaf <- TRUE     else  posaf <- FALSE
  if("angaf" %in% scales | "all" %in% scales)     angaf <- TRUE     else  angaf <- FALSE
  if("angpa" %in% scales | "all" %in% scales)     angpa <<- TRUE    else  angpa <- FALSE
  if("anghost" %in% scales | "all" %in% scales)   anghost <- TRUE   else  anghost <- FALSE
  if("emosup" %in% scales | "all" %in% scales)    emosup <- TRUE    else  emosup <- FALSE
  if("fearaf" %in% scales | "all" %in% scales)    fearaf <- TRUE    else  fearaf <- FALSE
  if("fearsoma" %in% scales | "all" %in% scales)  fearsoma <- TRUE  else  fearsoma <- FALSE
  if("friend" %in% scales | "all" %in% scales)    friend <- TRUE    else  friend <- FALSE
  if("instrsup" %in% scales | "all" %in% scales)  instrsup <- TRUE  else  instrsup <- FALSE
  if("lone" %in% scales | "all" %in% scales)      lone <- TRUE      else  lone <- FALSE
  if("perchost" %in% scales | "all" %in% scales)  perchost <- TRUE  else  perchost <- FALSE
  if("percrej" %in% scales | "all" %in% scales)   percrej <- TRUE   else  percrej <- FALSE
  if("apathy" %in% scales | "all" %in% scales)    apathy <- TRUE    else  apathy <- FALSE

  #set-up items
  if(percstrs)  PercStrs_items <- paste0("TB_PercStrs_",1:10) else  PercStrs_items <- NULL
  if(self)      Self_items <- paste0("TB_Self_",1:10)         else  Self_items <- NULL
  if(mp)        MP_items <- paste0("TB_MP_",1:7)              else  MP_items <- NULL
  if(sad)       Sad_items <- paste0("TB_Sad_",1:8)            else  Sad_items <- NULL
  if(gls)       GLS_items <- paste0("TB_GLS_",1:5)            else  GLS_items <- NULL
  if(posaf)     PosAf_items <- paste0("TB_PosAf_",1:15)       else  PosAf_items <- NULL
  if(angaf)     AngAf_items <- paste0("TB_AngAf_",1:5)        else  AngAf_items <- NULL
  if(angpa)     AngPA_items <- paste0("TB_AngPA_",1:5)        else  AngPA_items <- NULL
  if(anghost)   AngHost_items <- paste0("TB_AngHost_",1:5)    else  AngHost_items <- NULL
  if(emosup)    EmoSup_items <- paste0("TB_EmoSup_",1:8)      else  EmoSup_items <- NULL
  if(fearaf)    FearAf_items <- paste0("TB_FearAf_",1:7)      else  FearAf_items <- NULL
  if(fearsoma)  FearSoma_items <- paste0("TB_FearSoma_",1:6)  else  FearSoma_items <- NULL
  if(friend)    Friend_items <- paste0("TB_Friend_",1:8)      else  Friend_items <- NULL
  if(instrsup)  InstrSup_items <- paste0("TB_InstrSup_",1:8)  else  InstrSup_items <- NULL
  if(lone)      Lone_items <- paste0("TB_Lone_",1:5)          else  Lone_items <- NULL
  if(perchost)  PercHost_items <- paste0("TB_PercHost_",1:8)  else  PercHost_items <- NULL
  if(percrej)   PercRej_items <- paste0("TB_PercRej_",1:8)    else  PercRej_items <- NULL
  if(apathy)    Apathy_items <- paste0("TB_Apathy_",1:7)      else  Apathy_items <- NULL

  total_items <- c(PercStrs_items, Self_items, MP_items, Sad_items, GLS_items, PosAf_items,
                  AngAf_items, AngPA_items, AngHost_items, EmoSup_items, FearAf_items, FearSoma_items,
                  Friend_items, InstrSup_items, Lone_items, PercHost_items, PercRej_items, Apathy_items)

  #validate data.frame and items
  stopifnot(is.data.frame(df))
  stopifnot(all(total_items %in% names(df)))

  #reverse coding
  if(percstrs) {
    for(i in c(4,5,7,8)){ PercStrs_items[i] <- paste0(PercStrs_items[i],"r") }
    reverse_items <- paste0("TB_PercStrs_",c(4,5,7,8))
    reverse_items_recode <- paste0(reverse_items,"r")
    df[,reverse_items_recode] <- lapply(df[,reverse_items], function(x) { 6 - x }) #1-5 scoring by default
  }

  #mean impute, if requested
  if (max_impute > 0) {
    if(percstrs) df <- mean_impute_items(df, PercStrs_items, thresh=max_impute)
    if(self) df <- mean_impute_items(df, Self_items, thresh=max_impute)
    if(mp) df <- mean_impute_items(df, MP_items, thresh=max_impute)
    if(sad) df <- mean_impute_items(df, Sad_items, thresh=max_impute)
    if(gls) df <- mean_impute_items(df, GLS_items, thresh=max_impute)
    if(posaf) df <- mean_impute_items(df, PosAf_items, thresh=max_impute)
    if(angaf) df <- mean_impute_items(df, AngAf_items, thresh=max_impute)
    if(angpa) df <- mean_impute_items(df, AngPA_items, thresh=max_impute)
    if(anghost) df <- mean_impute_items(df, AngHost_items, thresh=max_impute)
    if(emosup) df <- mean_impute_items(df, EmoSup_items, thresh=max_impute)
    if(fearaf) df <- mean_impute_items(df, FearAf_items, thresh=max_impute)
    if(fearsoma) df <- mean_impute_items(df, FearSoma_items, thresh=max_impute)
    if(friend) df <- mean_impute_items(df, Friend_items, thresh=max_impute)
    if(instrsup) df <- mean_impute_items(df, InstrSup_items, thresh=max_impute)
    if(lone)  df <- mean_impute_items(df, Lone_items, thresh=max_impute)
    if(perchost) df <- mean_impute_items(df, PercHost_items, thresh=max_impute)
    if(percrej) df <- mean_impute_items(df, PercRej_items, thresh=max_impute)
    if(apathy) df <- mean_impute_items(df, Apathy_items, thresh=max_impute)
  }

  #compute row sums
  if(percstrs) df <- df %>% mutate(PercStrs_Raw = rowSums(across(all_of(PercStrs_items))))
  if(self) df <- df %>% mutate(Self_Raw = rowSums(across(all_of(Self_items))))
  if(mp) df <- df %>% mutate(MP_Raw = rowSums(across(all_of(MP_items))))
  if(sad)  df <- df %>% mutate(Sad_Raw = rowSums(across(all_of(Sad_items))))
  if(gls) df <- df %>% mutate(GLS_Raw = rowSums(across(all_of(GLS_items))))
  if(posaf) df <- df %>% mutate(PosAf_Raw = rowSums(across(all_of(PosAf_items))))
  if(angaf) df <- df %>% mutate(AngAf_Raw = rowSums(across(all_of(AngAf_items))))
  if(angpa) df <- df %>% mutate(AngPA_Raw = rowSums(across(all_of(AngPA_items))))
  if(anghost) df <- df %>% mutate(AngHost_Raw = rowSums(across(all_of(AngHost_items))))
  if(emosup) df <- df %>% mutate(EmoSup_Raw = rowSums(across(all_of(EmoSup_items))))
  if(fearaf) df <- df %>% mutate(FearAf_Raw = rowSums(across(all_of(FearAf_items))))
  if(fearsoma) df <- df %>% mutate(FearSoma_Raw = rowSums(across(all_of(FearSoma_items))))
  if(friend) df <- df %>% mutate(Friend_Raw = rowSums(across(all_of(Friend_items))))
  if(instrsup) df <- df %>% mutate(InstrSup_Raw = rowSums(across(all_of(InstrSup_items))))
  if(lone) df <- df %>% mutate(Lone_Raw = rowSums(across(all_of(Lone_items))))
  if(perchost) df <- df %>% mutate(PercHost_Raw = rowSums(across(all_of(PercHost_items))))
  if(percrej) df <- df %>% mutate(PercRej_Raw = rowSums(across(all_of(PercRej_items))))
  if(apathy) df <- df %>% mutate(Apathy_Raw = rowSums(across(all_of(Apathy_items))))

  #compute Theta scores
  if(percstrs) df <- df %>% mutate(PercStrs_Theta = case_when(PercStrs_Raw%%1==0~percstrs_adult(PercStrs_Raw), PercStrs_Raw%%1!=0~percstrs_adult_approx(PercStrs_Raw)))
  if(self) df <- df %>% mutate(Self_Theta = case_when(Self_Raw%%1==0~self_adult(Self_Raw),Self_Raw%%1!=0~self_adult_approx(Self_Raw)))
  if(mp) df <- df %>% mutate(MP_Theta = case_when(MP_Raw%%1==0~mp_adult(MP_Raw),MP_Raw%%1!=0~mp_adult_approx(MP_Raw)))
  if(sad) df <- df %>% mutate(Sad_Theta = case_when(Sad_Raw%%1==0~sad_adult(Sad_Raw),Sad_Raw%%1!=0~sad_adult_approx(Sad_Raw)))
  if(gls) df <- df %>% mutate(GLS_Theta = case_when(GLS_Raw%%1==0~gls_adult(GLS_Raw),GLS_Raw%%1!=0~gls_adult_approx(GLS_Raw)))
  if(posaf) df <- df %>% mutate(PosAf_Theta = case_when(PosAf_Raw%%1==0~posaf_adult(PosAf_Raw),PosAf_Raw%%1!=0~posaf_adult_approx(PosAf_Raw)))
  if(angaf) df <- df %>% mutate(AngAf_Theta = case_when(AngAf_Raw%%1==0~angaf_adult(AngAf_Raw),AngAf_Raw%%1!=0~angaf_adult_approx(AngAf_Raw)))
  if(angpa) df <- df %>% mutate(AngPA_Theta = case_when(AngPA_Raw%%1==0~angpa_adult(AngPA_Raw),AngPA_Raw%%1!=0~angpa_adult_approx(AngPA_Raw)))
  if(anghost) df <- df %>% mutate(AngHost_Theta = case_when(AngHost_Raw%%1==0~anghost_adult(AngHost_Raw),AngHost_Raw%%1!=0~anghost_adult_approx(AngHost_Raw)))
  if(emosup) df <- df %>% mutate(EmoSup_Theta = case_when(EmoSup_Raw%%1==0~emosup_adult(EmoSup_Raw),EmoSup_Raw%%1!=0~emosup_adult_approx(EmoSup_Raw)))
  if(fearaf) df <- df %>% mutate(FearAf_Theta = case_when(FearAf_Raw%%1==0~fearaf_adult(FearAf_Raw),FearAf_Raw%%1!=0~fearaf_adult_approx(FearAf_Raw)))
  if(fearsoma) df <- df %>% mutate(FearSoma_Theta = case_when(FearSoma_Raw%%1==0~fearsoma_adult(FearSoma_Raw),FearSoma_Raw%%1!=0~fearsoma_adult_approx(FearSoma_Raw)))
  if(friend) df <- df %>% mutate(Friend_Theta = case_when(Friend_Raw%%1==0~friend_adult(Friend_Raw),Friend_Raw%%1!=0~friend_adult_approx(Friend_Raw)))
  if(instrsup) df <- df %>% mutate(InstrSup_Theta = case_when(InstrSup_Raw%%1==0~instrsup_adult(InstrSup_Raw),InstrSup_Raw%%1!=0~instrsup_adult_approx(InstrSup_Raw)))
  if(lone) df <- df %>% mutate(Lone_Theta = case_when(Lone_Raw%%1==0~lone_adult(Lone_Raw),Lone_Raw%%1!=0~lone_adult_approx(Lone_Raw)))
  if(perchost) df <- df %>% mutate(PercHost_Theta = case_when(PercHost_Raw%%1==0~perchost_adult(PercHost_Raw),PercHost_Raw%%1!=0~perchost_adult_approx(PercHost_Raw)))
  if(percrej) df <- df %>% mutate(PercRej_Theta = case_when(PercRej_Raw%%1==0~percrej_adult(PercRej_Raw),PercRej_Raw%%1!=0~percrej_adult_approx(PercRej_Raw)))
  if(apathy) df <- df %>% mutate(Apathy_Theta = case_when(Apathy_Raw%%1==0~apathy_adult(Apathy_Raw),Apathy_Raw%%1!=0~apathy_adult_approx(Apathy_Raw)))

  #compute T scores
  theta_to_t <- function(x) { 50 + 10*x }
  df <- df %>% mutate(across(ends_with("_Theta"), .fns = list(T=~theta_to_t(.)), .names = "{gsub('_Theta','_T',{col})}"))

  #compute alphas
  if (add_alphas) {
    if(percstrs) attr(df[["PercStrs_Raw"]],"alpha") <- psych::alpha(df[,PercStrs_items])
    if(self) attr(df[["Self_Raw"]],"alpha") <- psych::alpha(df[,Self_items])
    if(mp) attr(df[["MP_Raw"]],"alpha") <- psych::alpha(df[,MP_items])
    if(sad) attr(df[["Sad_Raw"]],"alpha") <- psych::alpha(df[,Sad_items])
    if(gls) attr(df[["GLS_Raw"]],"alpha") <- psych::alpha(df[,GLS_items])
    if(posaf) attr(df[["PosAf_Raw"]],"alpha") <- psych::alpha(df[,PosAf_items])
    if(angaf) attr(df[["AngAf_Raw"]],"alpha") <- psych::alpha(df[,AngAf_items])
    if(angpa) attr(df[["AngPA_Raw"]],"alpha") <- psych::alpha(df[,AngPA_items])
    if(anghost) attr(df[["AngHost_Raw"]],"alpha") <- psych::alpha(df[,AngHost_items])
    if(emosup) attr(df[["EmoSup_Raw"]],"alpha") <- psych::alpha(df[,EmoSup_items])
    if(fearaf) attr(df[["FearAf_Raw"]],"alpha") <- psych::alpha(df[,FearAf_items])
    if(fearsoma) attr(df[["FearSoma_Raw"]],"alpha") <- psych::alpha(df[,FearSoma_items])
    if(friend) attr(df[["Friend_Raw"]],"alpha") <- psych::alpha(df[,Friend_items])
    if(instrsup) attr(df[["InstrSup_Raw"]],"alpha") <- psych::alpha(df[,InstrSup_items])
    if(lone) attr(df[["Lone_Raw"]],"alpha") <- psych::alpha(df[,Lone_items])
    if(perchost) attr(df[["PercHost_Raw"]],"alpha") <- psych::alpha(df[,PercHost_items])
    if(percrej) attr(df[["PercRej_Raw"]],"alpha") <- psych::alpha(df[,PercRej_items])
    if(apathy) attr(df[["Apathy_Raw"]],"alpha") <- psych::alpha(df[,Apathy_items])
  }

  #drop reverse codes and item-level data
  if (!keep_reverse_codes) { df <- df %>% select(-all_of(reverse_items_recode)) }
  if (drop_items) { df <- df %>% select(-starts_with("TB_")) }

  return(df)
}
