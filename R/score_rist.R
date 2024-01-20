#' Calculate RIST T-scores, RIST index, percentile and 90 percent confidence interval
#' @description This function scores the RIST Odd-item-out items and Guess-What for all subject IDs in the RIST Qualtrics survey.
#' 
#' @param qualtrics_api_key Important credentials you need to authenticate with the Qualtrics API. You only need to provide this if your Qualtrics credentials are not already loaded.
#' @param qualtrics_base_url Important credentials you need to authenticate with the Qualtrics API. You only need to provide this if your Qualtrics credentials are not already loaded.
#' @param qualtrics_surveyID surveyID of the Qualtrics survey you are trying to pull data from. Default set to NeuroMAP RIST survey.
#' @param return_dates Set to TRUE if you would like to get the date of RIST session, date of birth, and subject age in the output dataframe. Default: TRUE.
#' 
#' @export
#' @author Nidhi Desai
#' 
#' @importFrom qualtRics qualtrics_api_credentials fetch_survey
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate filter select
#'
#' @examples 
#' score_rist() # Qualtrics API key already setup and using other default input parameters tuned for NeuroMAP RIST Qualtrics survey
#' score_rist(qualtrics_api_key = "cLCPP23vnvkbvkEltXRJCtTttttL13TZswEZZ9Zq") # use your qualtrics API key
#' score_rist(qualtrics_surveyID = "DR_1YvGg9Emrw1cE3Y") # use a different Qualtrics survey than NeuroMAP RIST

score_rist <- function(qualtrics_api_key = "", qualtrics_base_url = "unc.sjc1.qualtrics.com", qualtrics_surveyID = "SV_1YvGg9Emrw1cE3Y", return_dates = TRUE){
    
    # checks if Qualtrics API Key is defined in environment
    if(nchar(qualtrics_api_key) == 0){
        if(Sys.getenv("QUALTRICS_API_KEY") <= 1){ # If api credentials are not already loaded in the environment
            warning("Qualtrics API Key and/or Base URL has/have not been set up. \n Use 'qualtrics_api_credentials()' to configure.")
            return(invisible(NULL))
        }
    }  else {
        qualtrics_api_credentials(qualtrics_api_key, qualtrics_base_url, overwrite = TRUE, install = TRUE)
        readRenviron("~/.Renviron")
    }

    df <- as.data.frame(fetch_survey(surveyID = qualtrics_surveyID, verbose = TRUE, force_request = TRUE))

    # OIO lookup table
    oio_lookup_scores <- data.frame(
        oio_raw = c(0:106), 
        t_score_18 = c(rep(9,61),11,12,13,15,16,18,19,20,22,23,25,26,27,29,30,32,33,34,36,37,39,
        40,41,43,44,46,47,48,50,51,53,54,55,57,58,60,61,62,64,65,67,68,69,71, 72, 75),
        t_score_19 = c(rep(9,61),10,12,13,15,16,18,19,20,22,23,25,26,28,29,30,32,33,35,36,38,39,
        40,42,43,45,46,47,49,50,52,53,55,56,57,59,60,62,63,65,66,67,69,70,72,73,75),
        t_score_20 = c(rep(9,61),10,12,13,15,16,18,19,21,22,23,25,26,28,29,31,32,34,35,37,38,39,
        41,42,44,45,47,48,50,51,52,54,55,57,58,60,61,63,64,65,67,68,70,71,73,74,76)
    )

    # GWH lookup table
    gwh_lookup_scores <- data.frame(
        gwh_raw = c(0:58),
        t_score_18 = c(rep(9,30),10,12,15,17,20,22,25,27,30,32,35,37,40,42,45,47,50,52,55,57,
        60,62,65,67,70,72,75,77,80),
        t_score_19 = c(rep(9,31),12,14,17,19,22,24,27,29,32,35,37,40,42,45,47,50,52,55,57,
        60,62,65,67,70,72,75,77,80),
        t_score_20 = c(rep(9,31),11,14,17,19,22,24,27,29,32,34,37,39,42,45,47,50,52,55,57,
        60,62,65,67,70,72,75,78,80)
    )

    # Create a table (data_df) to fill-in rest of the values
    data_df <-  data.frame(subject_id = as.numeric(df$Q1_1))
    data_df$gwh_raw_score <- as.numeric(df$SC0)
    data_df$oio_raw_score <- as.numeric(df$SC1)
    data_df$temp_dates <- df$Q1_3
    data_df$temp_dob <- df$Q1_4
    
    # remove pilot/test subjects
    data_df <- data_df %>% filter(!is.na(subject_id)) %>% filter(subject_id < 999) %>% filter(subject_id != 0)
    
    # Add test dates
    data_df <- data_df %>% mutate(test_dates = ifelse(nchar(sub(".*/", "",temp_dates)) == 2, 
                                                    format(as.Date(temp_dates, format = "%m/%d/%y"), "%m/%d/%Y"),
                                                    ifelse(nchar(sub(".*/", "",temp_dates)) == 4, 
                                                        format(as.Date(temp_dates, format = "%m/%d/%Y"), "%m/%d/%Y"), NA)))

    data_df <- data_df %>% select(-"temp_dates")

    # Add date of birth
    data_df <- data_df %>% mutate(date_of_birth = ifelse(nchar(sub(".*/", "",temp_dob)) == 2, 
                                                    format(as.Date(temp_dob, format = "%m/%d/%y"), "%m/%d/%Y"),
                                                    ifelse(nchar(sub(".*/", "",temp_dob)) == 4, 
                                                        format(as.Date(temp_dob, format = "%m/%d/%Y"), "%m/%d/%Y"), NA)))

    data_df <- data_df %>% select(-"temp_dob")

    # remove any subjects with no test date
    data_df <- data_df %>% filter(!is.na(test_dates))

    # Calculate subject's age and add a new colum
    data_df$subject_age <- interval(as.Date(data_df$date_of_birth, format = "%m/%d/%Y"), as.Date(data_df$test_dates, format = "%m/%d/%Y"))
    data_df$subject_age <- floor(data_df$subject_age/duration(num = 1, units = "years"))

    # Based on the age, get adjusted T-score from look-up table for GWH and OIO
    data_df$gwh_t_score <- unlist(lapply(1:nrow(data_df), function(i) {
        case_when(data_df$subject_age[i] == 18 ~ gwh_lookup_scores$t_score_18[gwh_lookup_scores$gwh_raw == data_df$gwh_raw_score[i]],
                  data_df$subject_age[i] == 19 ~ gwh_lookup_scores$t_score_19[gwh_lookup_scores$gwh_raw == data_df$gwh_raw_score[i]],
                  data_df$subject_age[i] >= 20 ~ gwh_lookup_scores$t_score_20[gwh_lookup_scores$gwh_raw == data_df$gwh_raw_score[i]],
                  TRUE ~ NA)}))

    data_df$oio_t_score <- unlist(lapply(1:nrow(data_df), function(i) {
        case_when(data_df$subject_age[i] == 18 ~ oio_lookup_scores$t_score_18[oio_lookup_scores$oio_raw == data_df$oio_raw_score[i]],
                  data_df$subject_age[i] == 19 ~ oio_lookup_scores$t_score_19[oio_lookup_scores$oio_raw == data_df$oio_raw_score[i]],
                  data_df$subject_age[i] >= 20 ~ oio_lookup_scores$t_score_20[oio_lookup_scores$oio_raw == data_df$oio_raw_score[i]],
                  TRUE ~ NA)}))

    # Calculate sum of GWH and OIO T-scores
    data_df <- data_df %>% 
                mutate(gwh_t_score = as.numeric(gwh_t_score)) %>%
                mutate(oio_t_score = as.numeric(oio_t_score)) %>%
                mutate(sum_t_score = gwh_t_score + oio_t_score)

    # Look-up RIST-2 Index, Percentile and 90% CI from look-up table
    rist_lookup_table <- data.frame(t_score = 0:200,
        rist_index = c(rep(40,26),41,41:45,45:49,49:53,53:57,57:61,61:65,65:69,69:73,
            73:77,77:81,81:85,85:89,89:93,93:97,97:101,101:105,105:109,109:113, 113:117,
            117:121,121:125,125:129,129:133,133:137,137:141,141:145,145:149,149:153, 
            153:157,157:159,rep(160,26)),
        percentile = c(rep(0,40),rep(0.1,5),rep(0.2,3),0.3,0.3,0.4,0.5,0.5,0.6,0.7,0.8,rep(1,4),
            rep(2,4),3,3,rep(4,3),5,5,6,6,7,8,9,10,10,12:14,16,16,18,19,21,23,23,25,27,30,
            32,32,34,37,39,42,42,45,47,50,53,53,55,58,61,63,63,66,68,70,73,73,75,77,79,81,
            81,82,84,86,87,87,88,90,91,92,92,93,94,95,95,95,96,96,rep(97,3),rep(98,3),
            rep(99,4),99.2,99.3,99.3,99.4,99.5,99.6,rep(99.7,3),99.8,99.8,rep(99.9,5),rep(100,40)),
        ninety_perc_ci = c(rep("38-46",26),"39-47","39-47","40-48","41-49","42-50","43-51","43-51","43-52",
        "44-53","45-54","46-55","46-55","47-56","48-57","49-58","50-58","50-58","51-59","52-60","53-61",
        "54-62","54-62","55-63","56-64","57-65","58-66","58-66","59-67","60-68","61-69","62-70","62-70",
        "63-71","64-72","65-73","66-74","66-74","67-75","68-76","69-77","70-78","70-78","71-79","72-80",
        "73-81","74-82","74-82","75-83","76-84","77-85","78-86","78-86","79-87","80-88","81-89","82-90",
        "82-90","83-91","83-92","84-92","85-93","85-93","86-94","87-95","88-96","89-97","89-97","90-98",
        "91-99","92-100","93-101","93-101","94-102","95-103","96-104","97-105","97-105","98-106","99-107",
        "100-108","101-109","101-109","102-110","103-111","104-112","105-113","105-113","106-114","107-115",
        "108-116","109-117","109-117","110-118","111-119","112-120","113-121","113-121","114-122","115-123",
        "116-124","116-125","116-125","117-125","118-126","119-127","120-128","120-128","121-129","122-130",
        "123-131","124-132","124-132","125-133","126-134","127-135","128-136","128-136","129-137","130-138",
        "131-139","132-140","132-140","133-141","134-142","135-143","136-144","136-144","137-145","138-146",
        "139-147","140-148","140-148","141-149","142-150","142-151","143-152","143-152","144-153","145-154",
        "146-155","147-156","147-156","148-157","149-157","150-158","151-159","151-159","152-160","153-161",
        rep("154-162",26)))

    data_df$rist_index <- unlist(lapply(1:nrow(data_df), function(i){ifelse(is.na(data_df$sum_t_score[i]), NA, rist_lookup_table$rist_index[rist_lookup_table$t_score == data_df$sum_t_score[i]])}))
    data_df$percentile <- unlist(lapply(1:nrow(data_df), function(i){ifelse(is.na(data_df$sum_t_score[i]), NA, rist_lookup_table$percentile[rist_lookup_table$t_score == data_df$sum_t_score[i]])}))
    data_df$ninety_perc_ci <- unlist(lapply(1:nrow(data_df), function(i){ifelse(is.na(data_df$sum_t_score[i]), NA, rist_lookup_table$ninety_perc_ci[rist_lookup_table$t_score == data_df$sum_t_score[i]])}))
    
    if (!return_dates) {data_df <- data_df %>% select(-test_dates, date_of_birth, subject_age)}
    
    return(data_df)
}
