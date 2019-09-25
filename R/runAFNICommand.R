#' Wrapper for running an AFNI command safely within R
#'
#' @param args AFNI command string to be run
#' @param afnidir Location of AFNI installation. If NULL, the function will serach the environment for AFNIDIF
#' @param stdout File target for redirecting stdout. If NULL, stdout will not be captured
#' @param stderr File target for redirecting stderr. If NULL, stderr will not be captured
#' @param echo Whether to print AFNI command to the screen
#'
#' @return The exit status of the executed AFNI command. 0 for success, non-zero for failure
#'
#' @details
#'
#' This command ensures that AFNI comamnds are in the system PATH
#'
#' @author Michael Hallquist
#' @export
#'
#' @examples
#'
#' \dontrun{
#' runAFNICommand("3dcopy test_data copy_data")
#' }

#wrapper for running an AFNI command safely within R
#if AFNI does not have its environment setup properly, commands may not work
runAFNICommand <- function(args, afnidir=NULL, stdout=NULL, stderr=NULL, echo=TRUE, ...) {
  #look for AFNIDIR in system environment if not passed in
  if (is.null(afnidir)) {
    env <- system("env", intern=TRUE)
    if (length(afnidir <- grep("^AFNIDIR=", env, value=TRUE)) > 0L) {
      afnidir <- sub("^AFNIDIR=", "", afnidir)
    } else {
      warning("AFNIDIR not found in environment. Defaulting to ", paste0(normalizePath("~/"), "/afni"))
      afnidir <- paste0(normalizePath("~/"), "/afni")
    }
  }

  Sys.setenv(AFNIDIR=afnidir) #export to R environment
  afnisetup=paste0("AFNIDIR=", afnidir, "; PATH=${AFNIDIR}:${PATH}; DYLD_FALLBACK_LIBRARY_PATH=${AFNIDIR}; ${AFNIDIR}/")
  afnicmd=paste0(afnisetup, args)
  if (!is.null(stdout)) { afnicmd=paste(afnicmd, ">", stdout) }
  if (!is.null(stderr)) { afnicmd=paste(afnicmd, "2>", stderr) }
  if (echo) { cat("AFNI command: ", afnicmd, "\n") }
  retcode <- system(afnicmd, ...)
  return(retcode)
}
