# File: ms_versioning.R
#' @import ggplot2
#' @import grid
#' @import gt
#' @import digest
#' @import tools
#' 
#' @name version_control
#' @title Version control for saved image and object files
#' @description
#' Pipeable version control for image and object files
#'
#' When piped from `multisave()`, this function expects a named vector or list of file paths.
#' For each file, it will:
#' 
#' - Determine the base output directory (assumed to be one level above the format subfolder)
#' - Create or use a subfolder named by `version_dir` (e.g. versions) within that base directory
#' - Within `version_dir`, create a subfolder for each format (e.g. png, pdf, rds)
#' - Compute a hash for each current file and compare it to the previous version's hash
#' - If the hash differs, copy the file into the corresponding format subfolder with a timestamp
#'
#' @param current_files A named vector or list of file paths to the saved files.
#'        For instance, names might be `"png"`, `"pdf"`, `"rds"`.
#' @param run_versioning Logical indicating whether to perform version control.
#'        Defaults to `TRUE`. If `FALSE`, the function simply passes through the input.
#' @param version_dir Character string specifying the subdirectory to store versioned files.
#'        This folder will be created inside the parent of the format subfolders.
#' @param hidden Logical; if `TRUE`, the versioning directory will be created as a hidden folder
#'        (by prepending a dot to the `version_dir` name). Defaults to `FALSE`.
#' @return Invisibly returns the original `current_files`.
#' @examples
#' \dontrun{
#'   multisave(object = myplot,
#'             formats = c("png", "pdf", "rds"),
#'             output_dir = "docmaker",
#'             name = "myplot") |>
#'     version_control(TRUE, "versions")
#' }
#'
#' @export
version_control <- function(current_files, 
                            versioning = TRUE, 
                            vers_dir = "versions", 
                            hidden = TRUE) {
  if (!isTRUE(versioning)) return(invisible(current_files))
  
  if (!requireNamespace("digest", quietly = TRUE)) {
    stop("Please install the 'digest' package.")
  }
  
  # Expecting current_files to be a named vector/list, e.g.
  # list(png = "docmaker/png/myplot.png", pdf = "docmaker/pdf/myplot.pdf", RDS = "docmaker/RDS/myplot.RDS")
  for (fmt in names(current_files)) {
    current_file <- current_files[[fmt]]
    
    if (!file.exists(current_file)) {
      warning("File does not exist: ", current_file)
      next
    }
    
    # Compute hash for the current file
    current_hash <- digest(file = current_file, algo = "sha256")
    
    # Determine the output subdirectory: assume current_file is like docmaker/<format>/myplot.<ext>
    # So the base directory is "docmaker"
    format_dir <- dirname(current_file)             # e.g. "docmaker/png"
    base_dir   <- dirname(format_dir)               # e.g. "docmaker"
    version_base <- file.path(base_dir, vers_dir)  # e.g. "docmaker/versions"
    
    # Create the version base if needed
    if (!dir.exists(version_base)) {
      dir.create(version_base, recursive = TRUE)
    }
    
    # Create a subfolder for this format (use lower case for consistency)
    version_subdir <- file.path(version_base, tolower(fmt))
    if (!dir.exists(version_subdir)) {
      dir.create(version_subdir, recursive = TRUE)
    }
    
    # Define a hash file (to store the hash of the latest version) in the version subfolder
    hash_file <- file.path(version_subdir, "latest_hash.txt")
    last_hash <- if (file.exists(hash_file)) {
      readLines(hash_file, warn = FALSE)
    } else {
      ""
    }
    
    # If the hash has changed, save a new version
    if (current_hash != last_hash) {
      timestamp <- format(Sys.time(), "%Y%m%dT%H%M%S")
      extension <- file_ext(current_file)
      base_name <- file_path_sans_ext(basename(current_file))
      
      # Construct the versioned file name: "myplot_20250414T153000.<ext>"
      new_file_name <- paste0(base_name, "_", timestamp, ".", extension)
      new_file_path <- file.path(version_subdir, new_file_name)
      
      file.copy(current_file, new_file_path)
      writeLines(current_hash, con = hash_file)
      
      message("New version saved for ", fmt, ": ", new_file_path)
    } else {
      message("No changes detected for ", fmt, ". Version not updated.")
    }
  }
  
  invisible(current_files)
}
