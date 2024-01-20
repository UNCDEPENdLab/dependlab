#' Add subject IDs to data which only contain subject's names by matching names from tracker
#' @description
#' Some data only has subject names like the MasterData tab of the NeuroMAP Master spreadsheet and the Screener.
#' In order to map their screening psychopathology scores like AI, SPIN, PAI-BOR the subject IDs for S3 task analysis, 
#' use this script to match the name in the tracker and pull the corresponding subjectID from there.
#' 
#' @param tracker_df entire filepath to a downloaded copy of NeuroMAP Master UNC.xlsx
#' @param addto_df can be entire filepath to the downloaded coup of screener or if you want to add subjectIDs to the MasterData tab in the Master spreadsheet, simply mention "MasterData". default set to "MasterData".
#' @param col_first_name name of the column which contains the first name of the subject to match between the tracker and master/screener. default set to "FIRSTNAME" (column in MasterData).
#' @param col_last_name name of the column which contains the last name of the subject to match between the tracker and master/screener. default set to "LASTNAME" (column in MasterData).
#' 
#' @return addto_df dataframe with ID column added in by matching names from study tracker
#' 
#' @examples
#' \dontrun{
#'   match_subIDs_names(tracker_df, master_df, col_first_name = "FIRSTNAME", col_last_name = "LASTNAME")
#'   match_subIDs_names(tracker_df, screener_df, col_first_name = "ContactForm_1", col_last_name = "ContactForm_2")
#' }
#' 
#' @export 
#' @author Nidhi Desai
#'
#' @importFrom readxl read_excel
#' @importFrom dplyr select rename filter
#' @importFrom magrittr %>% 
#' 
match_subIDs_names <- function(tracker_df, addto_df, col_first_name = "FIRSTNAME", col_last_name = "LASTNAME"){
  
  # create a list of names for the subIDs -----------------------------------
  subIDs_names <- tracker_df %>% 
    select(ID, `FIRSTLAST NAME                   (signed consent with)`) %>%
    rename(NAME = `FIRSTLAST NAME                   (signed consent with)`)
  
  # extract first and last name from subIDs_names table ---------------------
  subIDs_names$firstName <- NA
  subIDs_names$lastName <- NA
  for (i in c(1:length(subIDs_names$ID))){
    name_string <- strsplit(subIDs_names$NAME[i], " +")[[1]]
    subIDs_names$firstName[i] <- trimws(name_string[1])
    subIDs_names$lastName[i] <- trimws(ifelse(length(name_string) == 2, name_string[2], 
                                              ifelse(length(name_string) == 3, name_string[3], NA)))
  }
  
  # match IDs based on names and add to file --------------------------------
  addto_df$ID <- NA
  firstName_to_match <- trimws(addto_df[[col_first_name]])
  lastName_to_match <- trimws(addto_df[[col_last_name]])
  
  for (j in c(1:length(subIDs_names$ID))){
    # print(toString(j))
    matched_nrow <- intersect(which(firstName_to_match == subIDs_names$firstName[j]), 
                              which(lastName_to_match == subIDs_names$lastName[j]))
    if(length(matched_nrow) > 0){
      addto_df$ID[matched_nrow[1]] <- subIDs_names$ID[j]
    }
  }
  
  return(addto_df)
}
