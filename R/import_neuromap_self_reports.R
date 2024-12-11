#' Imports NeuroMAP Self Report Data from Qualtrics
#'
#' This function imports self report data for the NeuroMAP Study from Qualtrics and saves it as separate CSV files based on the specified scales.
#'
#' @param info Logical. If \code{TRUE}, displays information about the function and the scales that can be extracted.
#'   Additionally, it assigns the global variable \code{scales_to_score} with the names of all the scales available for scoring.
#'   Default is \code{FALSE}.
#' @param stats Logical. If \code{TRUE}, displays basic statistics about the self report data, such as the number of entries,
#'   the number of completed entries, and the date of the latest entry. Default is \code{FALSE}.
#' @param survey_name Character. The name of the survey in Qualtrics. Default is "NeuroMAP S2 - Self Report".
#' @param scales Character or vector of characters. The scales to extract from the self report data. Available scales are:
#'   "iip90" (Inventory for Interpersonal Problems), "ctq" (Childhood Trauma Questionnaire), "panas"
#'   (Positive and Negative Affect Schedule), "pid5" (Personality Inventory for DSM-5), "asr" (Adult Self Report),
#'   "bpq" (Borderline Personality Questionnaire), "fs" (Flourishing Scale), "dusi" (Short Form Revised Drug Use
#'   Screening Inventory), "isc" (Interpersonal Stressors Circumplex), "upps" (Impulsive Behaviors Scale),
#'   "bfi" (Big Five Inventory-2), "cts" (Conflict Tactics Scale), and "emotb" (Emotional Trauma Outcome Measures Battery).
#'   Default is "all" to include all scales.
#' @param include_id Logical. If \code{TRUE}, includes the participant ID in the output files. Default is \code{TRUE}.
#' @param include_dem Logical. If \code{TRUE}, includes demographic information (e.g., sex, age) in the output files.
#'   Default is \code{FALSE}.
#' @param path Character. The path where the output files will be saved. If the specified path does not exist, the function
#'   will create the directories recursively. Default is the current working directory.
#' @param file_suffix Character. A suffix to add to the output file names. Default is an empty string.
#' @param file_date Logical. If \code{TRUE}, appends a timestamp with the format "_%b_%d_%I_%M_%p" to the output file names.
#'   Default is \code{FALSE}.
#' @param add_to_envr Logical. If \code{TRUE}, returns a list of requested data.frames. Default is \code{FALSE}
#'
#'
#' @export
#' @author Zach Vig
#'
#' @importFrom qualtRics all_surveys fetch_survey
#' @importFrom dplyr bind_cols filter mutate rename select starts_with
#'
#' @examples
#' import_neuromap_self_reports(info = TRUE)
#' import_neuromap_self_reports(scales = c("ctq", "asr"), include_dem = TRUE)
#' import_neuromap_self_reports(stats = TRUE)
#' import_neuromap_self_reports(path = "~/my_output_files")
#' import_neuromap_self_reports(file_suffix = "_v2", file_date = TRUE)

import_neuromap_self_reports <- function(info = FALSE, stats = FALSE, survey_name = "NeuroMAP S2 - Self Report",
                                         scales = "all", include_id = TRUE, include_dem = FALSE, path = getwd(),
                                         file_suffix = "", file_date = FALSE, add_to_envr = FALSE){
# browser()
  #function info
  if(info){
    message("Current version extracts the following scales: \n 'iip90','ctq','panas','pid5','asr','bpq','fs','dusi','isc','upps','bfi','cts','emotb' \n Version 0.0.5 -- upd. December 11, 2024")
    scales_to_score<<-c('iip90','ctq','panas','pid5','asr','bpq','fs','dusi','isc','upps','bfi','cts','emotb')
    return(invisible(NULL))
  }

  #checks if Qualtrics API Key is defined in environment
  if(Sys.getenv("QUALTRICS_API_KEY") <= 1 | Sys.getenv("QUALTRICS_BASE_URL") <= 1) {
    warning("Qualtrics API Key and/or Base URL has/have not been set up. \n Use 'qualtrics_api_credentials()' to configure.")
    return(invisible(NULL))
  }

  #validates survey name
  all_surveys <- all_surveys()
  if(!(survey_name %in% all_surveys$name)) {
    warning("Survey name ('", survey_name ,"') not found in Qualtrics. \n Function terminated.")
    return(invisible(NULL))
  }

  #import all self report data from Qualtrics
  survey_info <- all_surveys %>% filter(name == survey_name)
  self_report_data <- suppressMessages(fetch_survey(survey_info$id, convert=FALSE, label=FALSE, verbose = FALSE))

  #self-report data stats
  if(stats){
    entries <- nrow(self_report_data)
    finished <- length(which(self_report_data$Progress == 100))
    date <- date()
    recent <- sort(self_report_data$RecordedDate, decreasing = TRUE)[1]
    message("As of ",date,", ",finished," of ",entries," entries have made 100% progress on ",survey_name,". \n The latest entry was made on ",recent,".")
    return(invisible(NULL))
  }

  #validates path string
  if(!is.character(path)) {
    warning("File path not of class 'character'. \n Function terminated.")
    return(invisible(NULL))
  }

  #validate _scales_ variable
  if(!(scales > 1)){
    warning("`scales` must include a class 'character' object or vector of class 'character' objects. Default: 'all' ")
    return(invisible(NULL))
  }

  #generates date/timestamp
  if(file_date){
    timestamp <- gsub(":","_",format(Sys.time(), "_%d_%b_%I_%M_%p"))
  }

  #creates new directory
  if(!dir.exists(path)){
    dir.create(path, recursive = T)
  }

  #gather id and demographic info
  if(include_id){ ids <- self_report_data %>% select(Intro_ID) %>% rename(ID=Intro_ID)}
  if (include_dem){ dem <- self_report_data %>% select(Intro_Sex,Intro_Sex_2_TEXT,Intro_Age,Intro_Age_0_TEXT) %>%
    mutate(Intro_Age = case_when(Intro_Age == 0 ~ Intro_Age_0_TEXT, Intro_Age != 0 ~ Intro_Age)) %>% select(-Intro_Age_0_TEXT) %>%
    rename(Sex=Intro_Sex,Sex_if_Other=Intro_Sex_2_TEXT,Age=Intro_Age) }

  #function for checking score ranges
  check_range <- function(df,min=1,max=4) {
    return(ifelse(all(df >= min, na.rm = T) & all(df <= max, na.rm = T), FALSE, TRUE))
  }

  #set-up list output
  if(add_to_envr){ data.list<-list() }

  ###########################################################

  #IIP-90 -- Inventory for Interpersonal Problems (90 items)
  if("iip90" %in% scales | "all" %in% scales) {
    iip90 <- self_report_data %>% select(starts_with("IIP_"))
    if(include_dem){ iip90 <- bind_cols(dem,iip90) }
    if(include_id){ iip90 <- bind_cols(ids,iip90) }

    iip90_items <- paste0("IIP_",1:90)
    if(check_range(df=iip90[,iip90_items],min=0,max=4)){
      warning("Some IIP90 scores are outside the expected range. \n The recoding scheme may have been altered in Qualtrics.")
    }

    write.csv(iip90, file = paste0(path,"/IIP90",file_suffix,ifelse(file_date, timestamp, ""),".csv"), row.names=FALSE)
    if(add_to_envr){
      data.list[[length(data.list)+1]] <- read.csv(paste0(path,"/raw_IIP90",file_suffix,ifelse(file_date, timestamp, ""),".csv"))
      names(data.list)[[length(data.list)]] <- "IIP90"
    }
  }

  #CTQ -- Childhood Trauma Questionnaire (28 items)
  if("ctq" %in% scales | "all" %in% scales) {
    ctq <- self_report_data %>% select(starts_with("CTQ_"))
    if(include_dem){ ctq <- bind_cols(dem,ctq) }
    if(include_id){ ctq <- bind_cols(ids,ctq) }

    ctq_items <- paste0("CTQ_",1:28)
    if(check_range(df=ctq[,ctq_items],min=1,max=5)){
      warning("Some CTQ scores are outside the expected range. \n The recoding scheme may have been altered in Qualtrics.")
    }

    write.csv(ctq, file = paste0(path,"/CTQ",file_suffix,ifelse(file_date, timestamp, ""),".csv"), row.names=FALSE)
    if(add_to_envr){
      data.list[[length(data.list)+1]] <- read.csv(paste0(path,"/raw_CTQ",file_suffix,ifelse(file_date, timestamp, ""),".csv"))
      names(data.list)[[length(data.list)]] <- "CTQ"
    }
  }

  #PANAS -- Positive and Negative Affect Schedule (20 items)
  if("panas" %in% scales | "all" %in% scales) {
    panas <- self_report_data %>% select(starts_with("PANAS_"))
    if(include_dem){ panas <- bind_cols(dem,panas) }
    if(include_id){ panas <- bind_cols(ids,panas) }

    panas_items <- paste0("PANAS_",1:20)
    if(check_range(df=panas[,panas_items],min=1,max=5)){
      warning("Some PANAS scores are outside the expected range. \n The recoding scheme may have been altered in Qualtrics.")
    }

    write.csv(panas, file = paste0(path,"/PANAS",file_suffix,ifelse(file_date, timestamp, ""),".csv"), row.names=FALSE)
    if(add_to_envr){
      data.list[[length(data.list)+1]] <- read.csv(paste0(path,"/raw_PANAS",file_suffix,ifelse(file_date, timestamp, ""),".csv"))
      names(data.list)[[length(data.list)]] <- "PANAS"
    }
  }

  #PID-5 -- Personality Inventory for DSM-5 (100 items)
  if("pid5" %in% scales | "all" %in% scales) {
    pid5 <- self_report_data %>% select(starts_with("Q7.")) %>% mutate_all(function(x){ x-1 })
    for(i in 1:100){
      pid5 <- pid5 %>% rename(!!paste0("PID_",i) := paste0("Q7.",i+1))
    }
    if(include_dem){ pid5 <- bind_cols(dem,pid5) }
    if(include_id){ pid5 <- bind_cols(ids,pid5) }

    pid5_items <- paste0("PID_",1:100)
    if(check_range(df=pid5[,pid5_items],min=0,max=3)){
      warning("Some PID5 scores are outside the expected range. \n The recoding scheme may have been altered in Qualtrics.")
    }

    write.csv(pid5, file = paste0(path,"/PID5",file_suffix,ifelse(file_date, timestamp, ""),".csv"), row.names=FALSE)
    if(add_to_envr){
      data.list[[length(data.list)+1]] <- read.csv(paste0(path,"/raw_PID5",file_suffix,ifelse(file_date, timestamp, ""),".csv"))
      names(data.list)[[length(data.list)]] <- "PID5"
    }
  }

  #ASR -- Adult Self Report [Ages 18-59] (126 items)
  if("asr" %in% scales | "all" %in% scales) {
    asr <- self_report_data %>% select(starts_with(c("ASR_")))#,"Q8.2","Q8.3","Q8.4"))) %>% rename(ASR_124=Q8.2) %>% rename(ASR_125=Q8.3) %>% rename(ASR_126=Q8.4)
    if(include_dem){ asr <- bind_cols(dem,asr) }
    if(include_id){ asr <- bind_cols(ids,asr) }

    asr_items <- asr %>% select(starts_with("ASR_"),-ends_with(c("TEXT","124","125","126"))) %>% names()
    if(check_range(df=asr[,asr_items],min=0,max=2)){
      warning("Some ASR scores are outside the expected range. \n The recoding scheme may have been altered in Qualtrics.")
    }

    write.csv(asr, file = paste0(path,"/ASR",file_suffix,ifelse(file_date, timestamp, ""),".csv"), row.names=FALSE)
    if(add_to_envr){
      data.list[[length(data.list)+1]] <- read.csv(paste0(path,"/raw_ASR",file_suffix,ifelse(file_date, timestamp, ""),".csv"))
      names(data.list)[[length(data.list)]] <- "ASR"
    }
  }

  #BPQ -- Borderline Personality Questionnaire (80 items)
  if("bpq" %in% scales | "all" %in% scales) {
    bpq <- self_report_data %>% select(starts_with("BPQ_")) %>% rename_with(~gsub("_0|_00","_",.x))
    if(include_dem){ bpq <- bind_cols(dem,bpq) }
    if(include_id){ bpq <- bind_cols(ids,bpq) }

    bpq_items <- paste0("BPQ_",1:80)
    if(check_range(df=bpq[,bpq_items],min=0,max=1)){
      warning("Some BPQ scores are outside the expected range. \n The recoding scheme may have been altered in Qualtrics.")
    }

    write.csv(bpq, file = paste0(path,"/BPQ",file_suffix,ifelse(file_date, timestamp, ""),".csv"), row.names=FALSE)
    if(add_to_envr){
      data.list[[length(data.list)+1]] <- read.csv(paste0(path,"/raw_BPQ",file_suffix,ifelse(file_date, timestamp, ""),".csv"))
      names(data.list)[[length(data.list)]] <- "BPQ"
    }
  }

  #FS -- Flourishing Scale (8 items)
  if("fs" %in% scales | "all" %in% scales) {
    fs <- self_report_data %>% select(starts_with("FS_")) %>% rename_with(~gsub("_0|_00","_",.x))
    if(include_dem){ fs <- bind_cols(dem,fs) }
    if(include_id){ fs <- bind_cols(ids,fs) }

    fs_items <- paste0("FS_",1:8)
    if(check_range(df=fs[,fs_items],min=1,max=7)){
      warning("Some FS scores are outside the expected range. \n The recoding scheme may have been altered in Qualtrics.")
    }

    write.csv(fs, file = paste0(path,"/FS",file_suffix,ifelse(file_date, timestamp, ""),".csv"), row.names=FALSE)
    if(add_to_envr){
      data.list[[length(data.list)+1]] <- read.csv(paste0(path,"/raw_FS",file_suffix,ifelse(file_date, timestamp, ""),".csv"))
      names(data.list)[[length(data.list)]] <- "FS"
    }
  }

  #DUSI-R -- Short Form Revised Drug Use Screening Inventory (15 items)
  if("dusi" %in% scales | "all" %in% scales) {
    dusi <- self_report_data %>% select(starts_with("DUSI_")) %>% rename_with(~gsub("_0|_00","_",.x))
    if(include_dem){ dusi <- bind_cols(dem,dusi) }
    if(include_id){ dusi <- bind_cols(ids,dusi) }

    dusi_items <- paste0("DUSI_",1:15)
    if(check_range(df=dusi[,dusi_items],min=0,max=1)){
      warning("Some DUSI scores are outside the expected range. \n The recoding scheme may have been altered in Qualtrics.")
    }

    write.csv(dusi, file = paste0(path,"/DUSI",file_suffix,ifelse(file_date, timestamp, ""),".csv"), row.names=FALSE)
    if(add_to_envr){
      data.list[[length(data.list)+1]] <- read.csv(paste0(path,"/raw_DUSI",file_suffix,ifelse(file_date, timestamp, ""),".csv"))
      names(data.list)[[length(data.list)]] <- "DUSI"
    }
  }

  #ISC -- Interpersonal Stressors Circumplex (64 items)
  if("isc" %in% scales | "all" %in% scales) {
    isc <- self_report_data %>% select(starts_with("ISC_")) %>% rename_with(~gsub("_0|_00","_",.x)) %>% mutate_all(function(x){ x+1 })
    if(include_dem){ isc <- bind_cols(dem,isc) }
    if(include_id){ isc <- bind_cols(ids,isc) }

    isc_items <- paste0("ISC_",1:64)
    if(check_range(df=isc[,isc_items],min=1,max=8)){
      warning("Some ISC scores are outside the expected range. \n The recoding scheme may have been altered in Qualtrics.")
    }

    write.csv(isc, file = paste0(path,"/ISC",file_suffix,ifelse(file_date, timestamp, ""),".csv"), row.names=FALSE)
    if(add_to_envr){
      data.list[[length(data.list)+1]] <- read.csv(paste0(path,"/raw_ISC",file_suffix,ifelse(file_date, timestamp, ""),".csv"))
      names(data.list)[[length(data.list)]] <- "ISC"
    }
  }

  #UPPS-P -- Impulsive Behaviors Scale (59 items)
  if("upps" %in% scales | "all" %in% scales) {
    upps <- self_report_data %>% select(starts_with("UPPS_"))
    if(include_dem){ upps <- bind_cols(dem,upps) }
    if(include_id){ upps <- bind_cols(ids,upps) }

    upps <- upps %>% rename(UPPS_55=UPPS_49,UPPS_49=UPPS_50,UPPS_50=UPPS_51,UPPS_51=UPPS_52,
                            UPPS_52=UPPS_53,UPPS_53=UPPS_54,UPPS_54=UPPS_55) %>%
                      select(everything(),paste0("UPPS_",1:59))


    upps_items <- paste0("UPPS_",1:59)
    if(check_range(df=upps[,upps_items],min=1,max=4)){
      warning("Some UPPS scores are outside the expected range. \n The recoding scheme may have been altered in Qualtrics.")
    }

    write.csv(upps, file = paste0(path,"/UPPS",file_suffix,ifelse(file_date, timestamp, ""),".csv"), row.names=FALSE)
    if(add_to_envr){
      data.list[[length(data.list)+1]] <- read.csv(paste0(path,"/raw_UPPS",file_suffix,ifelse(file_date, timestamp, ""),".csv"))
      names(data.list)[[length(data.list)]] <- "UPPS"
    }
  }

  #BFI-2 -- Big Five Inventory-2 (60 items)
  if("bfi" %in% scales | "all" %in% scales) {
    bfi <- self_report_data %>% select(starts_with("BFI_"))
    if(include_dem){ bfi <- bind_cols(dem,bfi) }
    if(include_id){ bfi <- bind_cols(ids,bfi) }

    bfi_items <- paste0("BFI_",1:60)
    if(check_range(df=bfi[,bfi_items],min=1,max=5)){
      warning("Some BFI scores are outside the expected range. \n The recoding scheme may have been altered in Qualtrics.")
    }

    write.csv(bfi, file = paste0(path,"/BFI",file_suffix,ifelse(file_date, timestamp, ""),".csv"), row.names=FALSE)
    if(add_to_envr){
      data.list[[length(data.list)+1]] <- read.csv(paste0(path,"/raw_BFI",file_suffix,ifelse(file_date, timestamp, ""),".csv"))
      names(data.list)[[length(data.list)]] <- "BFI"
    }
  }

  #CTS -- Conflict Tactics Scale (40 items)
  if("cts" %in% scales | "all" %in% scales) {
    cts <- self_report_data %>% select(starts_with("CTS_"))
    if(include_dem){ cts <- bind_cols(dem,cts) }
    if(include_id){ cts <- bind_cols(ids,cts) }

    cts_items <- paste0("CTS_",1:40)
    if(check_range(df=cts[,cts_items],min=1,max=7)){
      warning("Some CTS scores are outside the expected range. \n The recoding scheme may have been altered in Qualtrics.")
    }

    write.csv(cts, file = paste0(path,"/CTS",file_suffix,ifelse(file_date, timestamp, ""),".csv"), row.names=FALSE)
    if(add_to_envr){
      data.list[[length(data.list)+1]] <- read.csv(paste0(path,"/raw_CTS",file_suffix,ifelse(file_date, timestamp, ""),".csv"))
      names(data.list)[[length(data.list)]] <- "CTS"
    }
  }

  #NIH-EMOTB -- NIH Toolbox Emotions Battery (148 items)
  if("emotb" %in% scales | "all" %in% scales) {
    emotb <- self_report_data %>% select(starts_with("TB_")) %>% rename_with(~gsub("_0|_00","_",.x))
    if(include_dem){ emotb <- bind_cols(dem,emotb) }
    if(include_id){ emotb <- bind_cols(ids,emotb) }

    emotb_items <- emotb %>% select(starts_with("TB_")) %>% names()
    if(check_range(df=emotb[,emotb_items],min=1,max=7)){
      warning("Some NIH-EMOTB scores are outside the expected range. \n The recoding scheme may have been altered in Qualtrics.")
    }

    write.csv(emotb, file = paste0(path,"/EMOTB",file_suffix,ifelse(file_date, timestamp, ""),".csv"), row.names=FALSE)
    if(add_to_envr){
      data.list[[length(data.list)+1]] <- read.csv(paste0(path,"/raw_EMOTB",file_suffix,ifelse(file_date, timestamp, ""),".csv"))
      names(data.list)[[length(data.list)]] <- "EMOTB"
    }
  }

  ###########################################################

  #return list object, if requested
  if(add_to_envr){
    if(length(data.list)==1){
      return(data.list[[1]])
    } else {
      return(data.list)
    }
  }
}


########################################################################################################
# ADDING A NEW SCALE:                                                                                  #
# *Copy and paste the code below into the `import_neuromap_self_reports` function                                #
# *Replace any instances of `abc` or `ABC` with the shorthand code for the new scale (case-sensitive)  #
# *Replace the min and max values with the corresponding min and max of the scale                      #
########################################################################################################


# # ABC -- SCALE NAME HERE
# if("abc" %in% scales | "all" %in% scales) {
#   abc <- self_report_data %>% select(starts_with("ABC_")) # add a `mutate` function here if the Qualtrics automatic scoring is incorrect
#   if(include_dem){ abc <- bind_cols(dem,abc) }
#   if(include_id){ abc <- bind_cols(ids,abc) }
#
#   abc_items <- paste0("ABC_",1:40)
#   if(check_range(df=abc[,abc_items],min=000,max=999)){
#     warning("Some ABC scores are outside the expected range. \n The recoding scheme may have been altered in Qualtrics.")
#   }
#
#   write.csv(abc, file = paste0(path,"/ABC",file_suffix,ifelse(file_date, timestamp, ""),".csv"), row.names=FALSE)
#   if(add_to_envr){
#     data.list[[length(data.list)+1]] <- read.csv(paste0(path,"/ABC",file_suffix,ifelse(file_date, timestamp, ""),".csv"))
#     names(data.list)[[length(data.list)]] <- "ABC"
#   }
# }
