#' create a new data analysis project
#'
#' @description automatically creates project directory and creates .Rproj file
#'
#' @param path full path of the project directory to be created
#' @param sub_dirs character vector of high-level directories to add. Defaults to DEPENd Lab standard practice.
#' @param git_setup logical. intitialize new git repository and perform initial commit
#' @param open logical. open newly Rstudio project
#' @param description text to display in README.md
#'
#' @details make sure to install github CLI to use gh commands: https://github.com/cli/cli#installation. Flesh this out at a later date
#'
#' @examples
#' \dontrun{
#'  initialize_project("~/Documents/github_repos/dimt_analysis")
#'  initialize_project("~/Documents/github_repos/dimt_analysis", git_setup = FALSE) # just creates directory structure with no git integration
#' }
#'
#' @importFrom usethis create_project
#' @importFrom fs dir_create
#'
#' @author Nate Hall
#' @export



initialize_project <- function(path,
                               sub_dirs = c("analysis_scripts", "preproc_scripts", "support_fx", "data_raw", "data_preproc", "figures", "results", "reports"),
                               git_setup = TRUE,
                               open = TRUE,
                               readme_text = paste0("Welcome to ", basename(path), "!")) {

  # creates R project in desired path and generates some
  usethis::create_project(path, open = open)

  # remove R/ directory, which is more useful for writing R packages
  x <- suppressWarnings(file.remove(file.path(path, "R")))

  # create all sub-directories and set user and group permissions to rwx
  fs::dir_create(file.path(path, sub_dirs), mode = "u=rwx,go=rwx")


  # If desired, setup a remote git repository and perform an initial commit
  if (git_setup == TRUE) {

    # need to be in the git repo to add, commit, push
    setwd(path)

    # initialize git repository
    system(paste0("git init ", path))

    # generate README.md
    cat(readme_text, file="README.md",sep="\n")

    # setup repo
    system(paste0("gh repo create UNCDEPENdLab/", basename(path), " --private --source=. -d \"", readme_text, "\""))

    # add everything to the staging area
    system("git add .")

    # commit staged changes to remote
    system("git commit -m \"Initial commit\"")

    # push initial commit to remote
    system("git push --set-upstream origin main")

  }

}
