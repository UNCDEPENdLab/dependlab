#' setup a study-level directory structure
#'
#' @description Create directory structure for a new study in the DEPENd lab


#' @param name Name of the study, this is the name of the newly created directory as well.
#' @param loc Location to generate the newly created directory
#' @param sessions Either a vector of session numbers or names: c(1,2,3,4,5) | c("clinical_interview", "internet_testing1", "internet_testing_2", etc) OR a single numeric value that designates the number of sessions to count up to. E.g. 5 will create data_collection folders for 1:5.
#' @param data_modalities Named list of data modalities and different instances of those modalities being collected (see example)
#' @param l1_dirs Defaults to "all" to include all level 1 directories, but can specify a subset of c("study_info", "subj_management", "data_collection", "data", "data_curation") if the destination folder only needs some of the l1 directories.
#' @param nda Logical defaults to true. Whether or not to include an nda folder in the data folder
#' @param curation_repo Defaults to empty (just creates empty folder). allows user to enter a vector of git repositories to clone
#'
#' @return path to newly created study directory
#'
#' @author Nate Hall
#'
#' @examples
#'  \dontrun{
#'    setup_study(name = "neuromap",
#'                loc = "~/Desktop",
#'                sessions = 5,
#'                data_modalities = list(overview_forms = c("ses_ladder", "demogs", "contact_info", "payment_apps", "consent"),
#'                                       clinical_interview = c("scid", "sidp", "sis", "mini"),
#'                                       self_report = c("bfi", "upps"), # INCOMPLETE
#'                                       neuro_cog = "rist",
#'                                       behav_s3 = c("sorting_mushrooms", "kingdom", "vending_machine", "vanilla_baseline", "neighborhood"),
#'                                       eye_s3 = c("sorting_mushrooms", "kingdom", "vending_machine", "vanilla_baseline", "neighborhood"),
#'                                       physio_s3 = c("sorting_mushrooms", "kingdom", "vending_machine", "vanilla_baseline", "neighborhood"),
#'                                       mri = c()),
#'                l1_dirs = "all",
#'                nda = TRUE,
#'                curation_repo = c("git@sc.unc.edu:dependlab/nda_submission_neuromap.git",
#'                                  "git@sc.unc.edu:dependlab/s3_datareports.git",
#'                                  "git@sc.unc.edu:dependlab/s3_org.git",
#'                                  "git@sc.unc.edu:dependlab/s4_datareports.git"))
#'
#'  }
#'
#' @importFrom cabinets create_cabinet
#' @importFrom checkmake assert_directory_exists test_directory_exists
#' @import dplyr
#'
#' @export


setup_study <- function(name,
                        loc,
                        sessions = 1,
                        data_modalities = list(),
                        l1_dirs = "all",
                        nda = TRUE,
                        curation_repo = NULL){

  ######## Check for existing study folder in loc and if exists, create new study directory to work from
  assert_directory_exists(loc)
  study_dir <- file.path(loc, name)

  if(!test_directory_exists(study_dir)){
    dir.create(study_dir)
  } else{

    question1 <- readline(paste0("Your specified directory (", study_dir,") already exists, would you like to force overwrite it (BE CAREFUL)?  (y/n)  "))
    if(question1 %in% c("y", "Y", "yes", "Yes", "YES")){
      cat("--------------------\nDeleting old directory:", study_dir)
      unlink(study_dir, recursive = TRUE)
    } else{
      cat(paste0("--------------------\nAborting study setup:", study_dir, "Already exists"))
      stop()
    }

    dir.create(study_dir)
  }

  assert_directory_exists(study_dir) # check that this went off without error.

  ######## Generate sub-directories for all relevant study components
  # 1. Study Setup/Meta-information
  if(l1_dirs == "all" | "study_info" %in% l1_dirs){
    file.path(study_dir, "study_info/irb") %>% dir.create(recursive = TRUE)
    file.path(study_dir, "study_info/nda_info") %>% dir.create(recursive = TRUE)
    file.path(study_dir, "study_info/study_overview") %>% dir.create(recursive = TRUE)
    file.path(study_dir, "study_info/data_index") %>% dir.create(recursive = TRUE)
    file.path(study_dir, "study_info/training") %>% dir.create(recursive = TRUE)
  }

  # 2. Subject Tracking and Recruitment (aka management)
  if(l1_dirs == "all" | "subj_management" %in% l1_dirs){
    file.path(study_dir, "subj_management/tracking") %>% dir.create(recursive = TRUE)
    file.path(study_dir, "subj_management/recruitment") %>% dir.create(recursive = TRUE)
    file.path(study_dir, "subj_management/payment") %>% dir.create(recursive = TRUE)
  }

  # 3. Files for the actual collection of data: protocols and actual materials
  if(l1_dirs == "all" | "data_collection" %in% l1_dirs){

    # create session vector if only passing one number, otherwise just loop over the vector provided.
    if(is.numeric(sessions) & length(sessions) == 1){sessions <- 1:sessions}

    if(length(sessions) == 1){
      file.path(study_dir, "data_collection/protocols") %>% dir.create(recursive = TRUE)
      file.path(study_dir, "data_collection/materials") %>% dir.create(recursive = TRUE)
    } else{
      for(i in sessions){
        # 3.1 Data Collection and transfer
        file.path(study_dir, "data_collection", "protocols", paste0("s", i)) %>% dir.create(recursive = TRUE)
        file.path(study_dir, "data_collection", "materials", paste0("s", i)) %>% dir.create(recursive = TRUE)
      }
    }
  }

  # 4. Data folder
  if(l1_dirs == "all" | "data" %in% l1_dirs){
    for(i in names(data_modalities)){
      if(!i == "mri"){
        instances <- data_modalities[[i]]
        for(j in instances){
          file.path(study_dir, "data", i, j,"raw") %>% dir.create(recursive = TRUE)
          file.path(study_dir, "data", i, j,"preproc") %>% dir.create(recursive = TRUE)
        }
      } else {
        file.path(study_dir, "data", "mri","raw") %>% dir.create(recursive = TRUE)
        file.path(study_dir, "data", "mri", "bids") %>% dir.create(recursive = TRUE)
      }
    }
    ### nda path
    if(nda) file.path(study_dir, "data", "nda") %>% dir.create(recursive = TRUE)
  }

  # 5. data_curation (code) folder
  if(l1_dirs == "all" | "data_curation" %in% l1_dirs){
    if(is.null(curation_repo)){
      file.path(study_dir, "data_curation") %>% dir.create(recursive = TRUE)
    } else{
      if(length(curation_repo) == 1){
        # system(paste0("git clone ", curation_repo, " ", study_dir)) # clones just the content of the repo
        system2(paste0("git -C ", file.path(study_dir, "data_curation"), " clone ", curation_repo))
      } else {
        file.path(study_dir, "data_curation") %>% dir.create(recursive = TRUE)
        for(i in 1:length(curation_repo)){
          system2(paste0("git -C ", file.path(study_dir, "data_curation"), " clone ", curation_repo[i]))
        }
      }
    }
  }

  cat("--------------------\nCreated Study directory:", study_dir,"\n--------------------\n")
  fs::dir_tree(study_dir)

  return(study_dir)
}



## scratch below.

# setwd("~/github_repos/dependlab/")
# setwd("~/Desktop/")
# usethis::create_from_github(curation_repo[2])


# file_str <- list(
#   'study_info' = NULL,
#   'subj_management' = NULL,
#   'subj_management/tracking' = NULL,
#   'subj_management/recruitment' = NULL,
#   'data_collection' = paste0("s",1:sessions),
#   'data_raw' = paste0("s",1:sessions),
#   'data_preproc' = paste0("s",1:sessions),
#   'data_qa' = paste0("s",1:sessions)
# )
#
#
#   #
#   # # paste0("data_collection/s",1:sessions)
#   #
#   #
#   # study_cab <- create_cabinet(name = "study_cab",
#   #                             directory = loc,
#   #                             structure = file_str)
#   #
#   # new_cabinet_proj(cabinet = .study_cab,
#   #                  project_name = name,
#   #                  git = FALSE,
#   #                  git_ignore = "data",
#   #                  renv = TRUE)
#   #
#
#
# file.path(study_dir, i, "data_collection", "transfer") %>% dir.create(recursive = TRUE)
# # 3.2 Raw Data
# file.path(study_dir, i, "data_raw") %>% dir.create(recursive = TRUE)
# # 3.3 Preprocessed Data
# file.path(study_dir, i, "data_preproc") %>% dir.create(recursive = TRUE)
# # 3.4 Data QA and report generation
# file.path(study_dir, i, "data_qa") %>% dir.create(recursive = TRUE)
