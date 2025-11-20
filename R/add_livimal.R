#' Add a livimals silhouette to a ggplot!
#'
#' @param animal Name of the image file (without .png)
#' @param size Size of the image (0.1 is small, 0.5 is big)
#' @param x Optional x-position. Used only when no aes() is supplied.
#' @param y Optional y-position. Used only when no aes() is supplied.
#' @param color Optional color to apply to the silhouette (default "black")
#' @param mapping Optional aes() for x, y, or image.
#'
#' @return A ggplot2 layer
#' @export
add_livimal <- function(animal,
                        x = 1,
                        y = 1,
                        color = "black",
                        mapping = NULL,
                        data = NULL, ...) {

  if (!requireNamespace("magick", quietly = TRUE)) {
    stop("Package 'magick' is required")
  }

  img_path <- system.file("extdata", paste0(animal, ".png"), package = "livimals")
  if (img_path == "") stop("oops no image found for that animal!")

  # Read + recolor
  img <- magick::image_read(img_path)
  img_colored <- magick::image_colorize(img, opacity = 100, color = color)

  # Write to temp file
  tmp_file <- tempfile(fileext = ".png")
  magick::image_write(img_colored, tmp_file)

  # User supplied mapping (uses aes)
  if (!is.null(mapping)) {

    if (!is.null(data)) {
      df <- data
      df$image <- rep(tmp_file, nrow(df))
    } else {
      df <- NULL
    }

    return(
      ggimage::geom_image(
        data = df,
        image = tmp_file,
        mapping = mapping,
        by = "width",
        asp = 1,
        inherit.aes = TRUE, ...))
  }
  # No mapping
  if (!is.null(data)) {
    df <- data
    df$image <- rep(tmp_file, nrow(df)) 
  } else {
    if (is.null(x) | is.null(y)) {
      stop("You must provide either x and y or a data frame if mapping is NULL")
    }
    df <- data.frame(x = x, y = y, image = tmp_file)
  }

  ggimage::geom_image(
    data = df,
    image = tmp_file,
    mapping = aes(x = x, y = y, image = image),
    by = "width",
    asp = 1,
    inherit.aes = FALSE, ...)
}
