#' @import ggplot2
#' @import grid
#' @import gt
#' 
#' @name multisave
#' @title Save multiple objects
#' @description
#' Save an object in multiple formats
#' This function saves a given graphics object into multiple formats. 
#' It creates subdirectories for each format such as PNG, PDF, or RDS 
#' under the specified output directory and saves the object 
#' with the given base name.
#' @param object The object to be saved.
#' @param output_dir Character string specifying the root output directory.
#'                   Subdirectories for each format will be created here.
#' @param name Character string specifying the base file name without extension.
#' @param formats Character vector specifying which formats to save the object as.
#'                For example: `c("png", "pdf", "RDS")`. Format names are case-insensitive.
#' @param scale Numeric scaling factor applied to the width and height. A value of 1 
#'              indicates no scaling.
#' @param width Numeric value specifying the width of the plot. Its value is interpreted
#'              in the units specified by the `units` parameter.
#' @param height Numeric value specifying the height of the plot. Its value is interpreted
#'               in the units specified by the `units` parameter.
#' @param units Character string representing the units for the width and height. Accepted 
#'              values are `"in"`, `"cm"`, `"mm"`, or `"pt"`.
#' @param dpi Numeric value representing the resolution in dots per inch. This is used by 
#'            raster-based output and is ignored for vector outputs such as PDF.
#' @param engine Character string indicating the plotting engine to use. For instance, if set to `"gg"`, 
#'               the function will attempt to use `ggsave()` for ggplot-based objects, and if that fails 
#'               it will fall back to base R graphics.
#' @param verbose Logical; if `TRUE`, enables debugging messages such as the selected engine 
#'                and additional processing information.
#' @param ... Additional arguments passed to the underlying save functions.
#'
#' @return A named list of file paths for each saved format that can be piped to `version_control()`.
#'
#' @examples
#' \dontrun{
#'   # Save 'myplot' in multiple formats in 'mydir' with 'myplot' as the base file name.
#'   my_images <- multisave(object = myplot,
#'                          formats = c("png", "pdf", "RDS"),
#'                          output_dir = "mydir",
#'                          name = "myplot",
#'                          scale = 1,
#'                          width = 6,
#'                          height = 4,
#'                          units = "in",
#'                          dpi = 300,
#'                          engine = "gg",
#'                          verbose = FALSE)
#'
#'   # Optionally, pass the resulting file paths to version_control() for versioning:
#'   my_images |> version_control(TRUE, "versions")
#' }
#' @export
multisave <- function(object,
                      output_dir,
                      name,
                      formats = NULL,
                      scale = 1,
                      width = NULL,
                      height = NULL,
                      units =  c("in", "cm", "mm", "pt"),
                      dpi = 300,
                      engine = c("gg", "rgraphics"),
                      verbose = FALSE,
                      ...) {

  units <- match.arg(units)
  engine <- match.arg(engine)

  # Create the main output directory if it doesn't exist.
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Initialise an empty list to store file paths for each format.
  saved_files <- list()
  
  # Iterate over each requested format.
  # Create a dedicated subdirectory for the format (e.g., "mydocs/png").
  for (fmt in formats) {
    
    fmt_lower <- tolower(fmt)
    
    fmt_dir <- file.path(output_dir, fmt_lower)
    if (!dir.exists(fmt_dir)) {
      dir.create(fmt_dir, recursive = TRUE)
    }
    
    # Construct the file path: <output_dir>/<format>/<name>.<extension>
    file_path <- file.path(fmt_dir, paste0(name, ".", fmt_lower))

    
    
    # Save the object based on the format.
    save_by_format(object = object, 
                   fmt = fmt, 
                   file_path = file_path,
                   scale = scale,
                   width = width,
                   height = height,
                   units = units,
                   dpi = dpi,
                   engine = engine,
                   verbose = verbose,
                   ...)
    
    # Record the file path using the format as the list name.
    saved_files[[fmt_lower]] <- file_path
  }
  
  # Return the named list of saved file paths.
  return(saved_files)
}
