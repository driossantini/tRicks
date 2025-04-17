#' @import ggplot2
#' @import grid
#' @import gt
#' 
#' @name save_by_format
#' @title Save object by format
#' @description
#' Save an object to a specific file format
#' Saves the object in the specified format using the appropriate saving method.
#' Uses basic plotting functions for general objects, and may call specialised
#' functions for recognised object types depending on the chosen engine.
#' @param object The object to save.
#' @param fmt Character string specifying the desired file format. E.g. `"png"`,`"pdf"`, `"rds"`.
#' @param file_path Full file path where the object should be saved.
#' @param scale Numeric scaling factor for width and height. Passed to ggsave when used.
#' @param width Width of the output in units specified by `units`.
#' @param height Height of the output in units specified by `units`.
#' @param units Character string specifying the units for `width` and `height`.
#'              Options include `"in"`, `"cm"`, `"mm"`, or `"pt"`.
#' @param dpi Resolution in dots per inch, relevant for raster output.
#' @param engine Character string indicating the rendering engine to use. For instance,
#'               `"gg"` will attempt to use `ggsave` for ggplot objects.
#' @param ... Additional arguments passed to the underlying save function.
#'
#' @return No return value; the function is called for its side effects.
#'
#' @export
save_by_format <- function(object, 
                           fmt, 
                           file_path,
                           scale, 
                           width,
                           height ,
                           units,
                           dpi,
                           engine,
                           ...) {
  switch(
    fmt,
    pdf = {
      save_image(filename = file_path, 
                 object = object,
                 scale = scale,
                 width = width,
                 height = height,
                 units = units,
                 engine = engine,
                 ...)
    },
    
    png = {
      save_image(filename = file_path, 
                 object = object,
                 scale = scale,
                 width = width,
                 height = height,
                 units = units,
                 engine = engine,
                 ...)
    },

    rds = {
      saveRDS(object, file = file_path)
    },

    svg = {
      save_image(filename = file_path, 
                 object = object,
                 scale = scale,
                 width = width,
                 height = height,
                 units = units,
                 engine = engine,
                 ...)
    },
    
    html = {
      if (!inherits(object, "plotly")) {
        stop("Format 'html' requires a plotly object.")
      }
      htmlwidgets::saveWidget(object, file_path)
    },
    stop("Format not supported for the given object type.")
  )
}


# Helper: Convert dimensions to inches with scaling
convert_dims <- function(width, height, units, scale) {
  units <- tolower(units)
  if (units != "in" && !is.null(width)) {
    width <- grid::convertUnit(grid::unit(width, units), "inches", valueOnly = TRUE) * scale
  }
  if (units != "in" && !is.null(height)) {
   height <- grid::convertUnit(grid::unit(height, units), "inches", valueOnly = TRUE) * scale
  }
  list(width = width, height = height)
}

#' @name save_image
#' @title Save plot image using ggplot2 or base graphics
#' @description
#' Save a plot object as a PDF using ggplot2 or base R
#'
#' This function attempts to use `ggsave()` for objects whose class starts with `"gg"`.
#' If `ggsave()` fails, or if the object is not ggplot-based, it falls back to base R
#' graphics using `pdf()` and `plot()`. Note that in the fallback branch, `pdf()` is
#' only called once and plots a single object.
#'
#' @param filename Character string specifying the file path for the PDF.
#' @param object The object to save.
#' @param scale Numeric scaling factor (used by `ggsave()`).
#' @param width Numeric width of the output.
#' @param height Numeric height of the output.
#' @param units Character string specifying the units for width and height.
#'        Must be one of `"in"`, `"cm"`, `"mm"`, or `"pt"`.
#' @param dpi Numeric DPI used by `ggsave()`. Ignored when using base graphics.
#' @param engine Character string specifying the engine to use for saving.
#'        Either `"gg"` (ggplot2) or `"rgraphics"` (base R). If `"gg"` is selected
#'        but the object is not compatible, base graphics will be used as a fallback.
#' @param ... Additional arguments passed to the underlying plotting function.
#'
#' @return No return value; the function is called for its side effects.
#'
#' @export
save_image <- function(filename, 
                       object,
                       scale = 1,
                       width = NULL,
                       height = NULL,
                       units = c("in", "cm", "mm", "pt"),
                       dpi = 300,
                       engine = c("gg", "rgraphics"),
                       ... ) {
 
  format <- tolower(tools::file_ext(filename))
  units <- match.arg(units)
  engine <- match.arg(engine)
 
  # Initially, assume no fallback is needed.
  fallback <- FALSE
  
  # If any class of the object starts with "gg", try using ggsave().
  if (any(startsWith(class(object), "gg")) && engine == "gg") {
    tryCatch({
      ggsave(filename = filename, 
             plot = object,
             scale = scale,
             width = width,
             height = height,
             units = units,
             dpi = dpi,
             ...)
      print("using ggsave")
    }, error = function(e) {
      message("ggsave() failed: ", conditionMessage(e), 
              "\nFalling back to base R graphics")
      fallback <<- TRUE
    })
  } else {
    fallback <- TRUE
  }
  
  # If fallback is TRUE, use base R's pdf() device.
  if ((fallback || engine == "rgraphics") && 
     (exists(format, where = asNamespace("grDevices"), 
             mode = "function", inherits = FALSE))) {
   
    dims <- convert_dims(width, height, units, scale)

    # Obtain the function from the package namespace
    saveimg_fun <- get(format, envir = asNamespace("grDevices"))
    # Define your arguments in a list
    args <- list(file = filename, filename = filename, width = dims$width, 
                 height = dims$height, res = dpi)
    # Remove unnecessary arguments
    args <- args[names(args) %in% names(formals(saveimg_fun))]
    # Execute the function call
    do.call(saveimg_fun, args)
    plot(object)
    dev.off()
    print("using rgraphics")
  }
}