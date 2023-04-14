#' Wrapper for running an fsl command safely within R
#'
#' @param args FSL command string to be run
#' @param fsldir Location of FSL installation. If NULL, the function
#' @param stdout File target for redirecting stdout. If NULL, stdout will not be captured
#' @param stderr File target for redirecting stderr. If NULL, stderr will not be captured
#' @param echo Whether to print FSL command to the screen
#'
#' @return The exit status of the executed FSL command. 0 for success, non-zero for failure
#'
#' @details
#'
#' This command ensures that $FSLDIR/bin is in the PATH and sources $FSLDIR/etc/fslconf/fsl.sh
#'
#' I should probably just use the \code{fslr} package, but didn't know about it prior to writing this...
#'
#' @author Michael Hallquist
#' @export
#'
#' @examples
#'
#' \dontrun{
#' runFSLCommand("fslmaths inp_nif -mul 10 -abs -thr 15 manip_out")
#' }

runFSLCommand <- function(args, fsldir=NULL, stdout=NULL, stderr=NULL, echo=TRUE) {

  #look for FSLDIR in system environment if not passed in
  if (is.null(fsldir)) {
    #check for FSLDIR in user .bashrc
    if (file.exists("~/.bashrc")) {
      bashrc <- scan("~/.bashrc", what="character", sep="\n")
      bashrc_fsldir <- grep("([^\\s]*\\s+)*FSLDIR=", bashrc, value=TRUE)
      if (length(bashrc_fsldir) > 1) {
        message("More than one FSLDIR setting found in ~/.bashrc. Ignoring for now.")
        bashrc_fsldir <- character(0)
      }
      if (!identical(bashrc_fsldir, character(0))) { bashrc_fsldir <- strsplit(bashrc_fsldir, "=")[[1]][2] }
    }

    #check for FSLDIR in current environment
    env <- system("env", intern=TRUE)
    if (length(fsldir <- grep("^FSLDIR=", env, value=TRUE)) > 0L) {
      fsldir <- sub("^FSLDIR=", "", fsldir)
    } else if (!identical(bashrc_fsldir, character(0))) {
      fsldir <- bashrc_fsldir
    } else {
      warning("FSLDIR not found in environment. Defaulting to /usr/local/fsl.")
      fsldir <- "/usr/local/fsl"
    }
  }

  Sys.setenv(FSLDIR=fsldir) #export to R environment
  fslsetup <- paste0("FSLDIR=", fsldir, "; PATH=${FSLDIR}/bin:${PATH}; . ${FSLDIR}/etc/fslconf/fsl.sh; ${FSLDIR}/bin/")
  fslcmd=paste0(fslsetup, args)
  if (!is.null(stdout)) { fslcmd=paste(fslcmd, ">", stdout) }
  if (!is.null(stderr)) { fslcmd=paste(fslcmd, "2>", stderr) }
  if (echo) { cat("FSL command: ", fslcmd, "\n") }
  retcode <- system(fslcmd)
  return(retcode)
}
