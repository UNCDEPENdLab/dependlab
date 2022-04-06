#' Create data frame of messy directory structure
#'
#' @description generate df for transfer to new study structure.
#' @param dir Name of the directory to be transferred.
#'
#' @import tidyverse
#'
#' @export

study_curation_transfer <- function(dir){
  all_files <- list.files("~/Desktop/graph_metrics_roman",all.files = TRUE, full.names = TRUE, recursive = TRUE)

  depth <- strsplit(all_files, "/", fixed = TRUE)  %>% sapply(length)
  min_depth <- min(depth); max_depth = max(depth)

  dir_level_split <- suppressWarnings(strsplit(all_files, "/", fixed = TRUE) %>% do.call(rbind,.) %>% data.frame() %>% select(-X1) %>% tibble())
  colnames(dir_level_split) <- paste0("l", 1:ncol(dir_level_split))

  return(dir_level_split)
}
