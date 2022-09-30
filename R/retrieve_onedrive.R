#' retrieve .csv and excel (xls/xlsx) files from OneDrive/Sharepoint site
#'
#'
#' @param item_path path to desired OneDrive sharepoint file. Treat the base directory as root.
#' @param sheet String of sheet to extract (if excel file). Defaults to NULL
#' @param spn Share point number. Can determine by running Microsoft365R::list_sharepoint_sites() and determining which site number is appropriate. Defaults to 1. If you have more than one sharepoint site on your account you may need to adjust this!
#' @param rm_file the function needs to download a copy of the file into your current directory. Leaving this set to TRUE (default) will remove the downloaded file.
#'
#' @return Extracted OneDrive file
#'
#' @author Nate Hall
#'
#' @examples
#'
#' \dontrun{
#'      master <- retrieve_onedrive("Studies/NeuroMAP/Participant Management/Master Spreadsheets/NeuroMAP Master UNC.xlsx") # returns all sheets in a named list
#'      tracker <- retrieve_onedrive("Studies/NeuroMAP/Participant Management/Master Spreadsheets/NeuroMAP Master UNC.xlsx", sheet = "Tracker") # extracts only the Tracker sheet
#' }
#'
#' @import Microsoft365R
#' @import readxl
#' @import furrr
#' @import magrittr
#' @import stringr
#' @import tidyverse
#'
#' @export

retrieve_onedrive <- function(item_path, sheet = NULL, spn = 1, rm_file = TRUE){

  suppressMessages(devtools::install_github("Azure/Microsoft365R")) # if already installed with no updates to repo will skip unless force = TRUE
  require("Microsoft365R")
  require("readxl")

  sps <- list_sharepoint_sites()
  depend_sharepoint <- sps[[spn]]
  cat("------------------\nConnecting to Sharepoint site:\n------------------\n")
  print(depend_sharepoint)

  depend <- depend_sharepoint$get_drive()
  item <- depend$get_item(item_path)
  item$download(overwrite = TRUE)#, dest = dest)


  sort_files_by_date <- function(folder_path = '.', search_pattern = NULL, by = 'mtime'){
    # Retreived from https://stackoverflow.com/questions/13762224/how-to-sort-files-list-by-date
    require(furrr)
    require(magrittr)
    require(stringr)
    require(tidyverse)

    if (!by %>% str_detect('^(m|a|c)time$')) {
      stop('Argument `by` must be one of "mtime", "atime", or "ctime".')
    }

    file_names <-

      # Gets all the file paths for a given path and pattern
      list.files(path = folder_path, pattern = search_pattern, full.names = TRUE) %>%

      # Turns into a one-column tibble (see below)
      tibble(file_names = .)

    files_by_datetime <-

      suppressWarnings(
        future_map_dfr(
          .x = file_names,
          .f = file.info,
          .progress = TRUE,
          extra_cols = FALSE # passed to file.info for performance boost
        )
      ) %>%

      # gets expanded file info, then select the last-modified time and last-accessed time
      select(mtime, atime, ctime) %>%

      # reintroduce our original 'file_names'
      bind_cols(file_names) %>%

      # arrange by descending time (depending on the users choice)
      arrange(
        desc(
          case_when(
            (by == 'mtime') ~ mtime,
            (by == 'atime') ~ atime,
            (by == 'ctime') ~ ctime
          )
        )
      )

    return(files_by_datetime)

  }

  dir_files <- sort_files_by_date()#,search_pattern = "NEUROMAP\ Master UNC") # may want to reintroduce?
  rownames(dir_files) <- NULL

  head(dir_files)

  read_path <- pull(dir_files, file_names)[1] # most recently downloaded file path

  getExtension <- function(file){
    # retrieved from: https://stackoverflow.com/questions/7779037/extract-file-extension-from-file-path
    ex <- strsplit(basename(file), split="\\.")[[1]]
    return(ex[-1])
  }

  ext <- getExtension(read_path)

  if(ext %in% c("xlsx", "xls")){
    if(is.null(sheet)){
      x <- lapply(excel_sheets(read_path), read_excel, path = read_path)
      names(x) <- excel_sheets(read_path)
    } else{
      x <- read_excel(read_path, sheet = sheet)
    }
  } else if(ext == "csv"){
    x <- read.csv(x)
  } else{
    message("Currently only .csv and xls/xlsx files supported")
  }


  if(rm_file) {try(file.remove(read_path))}

  return(x)
}
