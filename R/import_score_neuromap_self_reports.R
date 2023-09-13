#' Import and Score NeuroMAP Self Report Data
#'
#' This is a comprehensive function for importing, scoring, and storing NeuroMAP self report data to be used in a data analysis pipeline.
#' It uses the `import_neuromap_self_reports` and applicable `score_` functions from the dependlab package.
#'
#' @param split_output if true, outputs a list object with "scores" and "items" containing score- and item- level data, respectively. Default: /code{TRUE}
#' @param path string containing path to house csv data exports. Creates a new directory if it doesn't already exist.
#'          Default creates a new folder called "neuromap_self_reports_repo" in the working directory.
#'
#' Note: the embedded `import_neuromap_self_reports` exports new csvs into the data repo with a date/time stamp every time the function is run
#'
#' @export
#' @author Zach Vig

import_score_neuromap_self_reports <- function(split_output = TRUE, path = paste0(getwd(),"/neuromap_self_reports_repo")) {

  self_report_data <- import_neuromap_self_reports(info = FALSE, stats = FALSE, survey_name = "NeuroMAP S2 - Self Report",
                                                   scales = "all", include_id = TRUE, include_dem = FALSE, path = path,
                                                   file_suffix = "_neuromap_self_reports", file_date = TRUE, add_to_envr = TRUE)

  score_all <- function(df, drop_items=logical()) {

    names <- names(df)

    for(name in names){ #cycles through every scale that has a pre-defined scoring function in the dependlab package

      func <- paste0("score_",tolower(name))

      if(exists(func, where='package:dependlab', mode='function')) {
        df[[name]] <- do.call(func,list(df=df[[name]],drop_items=drop_items))
      }

    }

    return(df)
  }

  if(split_output){ #if true, splits output into a two tiered list of scores- and item- level data

    scored_self_report_data <- list()

    scored_self_report_data[["scores"]] <- score_all(self_report_data, drop_items = T)
    scored_self_report_data[["scores"]]$ASR <- NULL #removes ASR since it only has raw data

    scored_self_report_data[["items"]] <- self_report_data

  } else {

    scored_self_report_data <- score_all(self_report_data, drop_items = F)

  }

  return(scored_self_report_data)
}
