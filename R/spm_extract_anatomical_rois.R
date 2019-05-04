#' This is a wrapper around the spm_extract_anatomical_rois.m script in the inst directory
#'
#' @param l1spmdirs character vector of level 1 SPM directories containing SPM.mat files
#' @param masks character vector of NIfTI mask images for each anatomical ROI of interest
#' @param threshold p-value threshold applied to contrast within mask before extraction
#' @param threshdesc multiple comparisons correction on p-value. 'none' or 'FWE'
#' @param session which session (run) to use for extracting time series
#' @param extent exclude clusters having fewer than voxels than extent
#' @param adjust_F_index index of F-test in SPM.mat to adjust for all effects of interest
#' @param contrast_index index of t-test contrast in SPM.mat that is of interest
#' @param ncores number of cores to use in a parallel approach; function parallelizes over \code{l1spmdirs}
#' @param spm_path path to spm12 installation; added to MATLAB path at runtime
#' @param matlab_path location of MATLAB binary; used with matlabr for run_matlab_code()
#' 
#' @importFrom matlabr run_matlab_code
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach registerDoSEQ foreach
#' @importFrom parallel makeCluster stopCluster
#' @importFrom matlabr have_matlab run_matlab_code
#'
#' @author Michael Hallquist
#' 
#' @export
#' 
spm_extract_anatomical_rois <- function(l1spmdirs, masks, threshold=0.2, threshdesc='none', session=1, extent=0,
                                        adjust_F_index=1, contrast_index=2, ncores=1,
                                        spm_path="/gpfs/group/mnh5174/default/lab_resources/spm12",
                                        matlab_path="/opt/aci/sw/matlab/R2017b/bin") {
  
  if (missing(l1spmdirs)) { stop("Need to pass in a character vector of all level 1 directories containing SPM.mat files.") }
  stopifnot(all(dir.exists(l1spmdirs)))
  stopifnot(all(file.exists(masks)))
  if (is.null(names(masks))) {
    names(masks) <- tools::file_path_sans_ext(basename(masks)) #develop basic naming scheme
  }

  stopifnot(dir.exists(spm_path))
  stopifnot(dir.exists(matlab_path))

  #set the matlab path for matlabr
  options(matlab.path=matlab_path)

  if (!have_matlab()) { stop("Unable to find MATLAB installation") }
  
  if (ncores > 1) {
    cl <- makeCluster(ncores)
    on.exit(try(stopCluster(cl)))
    registerDoParallel(cl)
  } else {
    registerDoSEQ()
  }

  #add single quotes around mask strings if not present
  masks <- sub("^'?([^']+)'?", "'\\1'", masks, perl=TRUE)
  mask_names <- sub("^'?([^']+)'?", "'\\1'", names(masks), perl=TRUE)

  #TODO: support multi-session data

  spm_preamble <- c(
    ifelse(is.null(spm_path), "", paste0("addpath('", spm_path, "');")),
    "spm('defaults', 'fmri');",
    "spm_jobman('initcfg');",
    ""    
  )
  
  res <- foreach(dd=iter(l1spmdirs)) %dopar% {
    m_string <- c(spm_preamble,
      "cfg = struct();",
      "masks = {",
      masks,
      "};",
      "names = {",
      mask_names,
      "};",
      "",
      "threshold = ", threshold, ";",
      "threshdesc = ", sub("^'?([^']+)'?", "'\\1'", threshdesc, perl=TRUE), ";",
      "session = ", session, ";",
      "extent = ", extent, ";",
      "adjust_F_index = ", adjust_F_index, "; %adjust time series for all effects of interest",
      "contrast_index = ", contrast_index, "; %the contrast of interest",
      "",
      "for jj = 1 : numel(masks)",
      "  cfg(jj).target_dir = fullfile('", dd, "');",
      "  cfg(jj).mask = masks{jj};",
      "  cfg(jj).adjust = adjust_F_index;",
      "  cfg(jj).session = session;",
      "  cfg(jj).name = names{jj};",
      "  cfg(jj).contrast = contrast_index;",
      "  cfg(jj).threshold = threshold;",
      "  cfg(jj).threshdesc = threshdesc;",
      "  cfg(jj).extent = extent;",
      "end",
      "spm_extract_anatomical_rois(cfg);"
    )

    #contains matlab scripts for extraction
    matlab_scripts <- system.file("inst", "matlab", package = "dependlab")
    
    run_matlab_code(m_string, endlines = FALSE, verbose = TRUE,
      add_clear_all = FALSE, paths_to_add = matlab_scripts)
    
  }
}
