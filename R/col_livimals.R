#' Add a livimals silhouette to a ggplot!
#'
#' @param animal Name of the image file (without .png)
#' @param data A dataframe
#' @param x Optional x-position. Used only when no aes() is supplied.
#' @param y Optional y-position. Used only when no aes() is supplied.
#' @param color Optional color to apply to the silhouette (default "black")
#' @param mapping Optional aes() for x, y, or image.
#' @param ... Additional arguments passed to geom_image()
#'
#' @return A ggplot2 layer
#' @export
col_livimal <- function(animal,
                        x = 1,
                        y = 1,
                        color = "black",
                        mapping = NULL,
                        data = NULL, ...) {

  if (!requireNamespace("magick", quietly = TRUE)) {
    stop("Package 'magick' is required")
  }

# Names of PNGs that should NEVER be recolored
  protected_colors <- c("hareyb", "harerb", "raptorb", "raptoryb", "tortoiseyb", "tortoiserb", "salmonrb", "salmonyb", "weevilyb", "weevilrb", "frogrb", "frogyb")

  img_path <- system.file("extdata", paste0(animal, ".png"), package = "livimals")
  if (img_path == "") stop("oops no image found for that animal!")
  img <- magick::image_read(img_path)

# Determine whether this specific silhouette should NOT be recolored
  is_protected <- animal %in% protected_colors

if (!is.null(mapping$fill)) {
    fill_var <- rlang::quo_get_expr(mapping$fill)
    unique_vals <- unique(data[[fill_var]])

    for (i in unique_vals) {
      if (is.na(i)) next

      # Only recolor if NOT protected
      if (!is_protected) {
        img_colored <- magick::image_colorize(img, opacity = 100, color = i)
      } else {
        img_colored <- img  
      }

      tmp_file <- tempfile(fileext = ".png")
      magick::image_write(img_colored, tmp_file)
 data[data[[fill_var]] == i, "ColImage"] <- tmp_file
    }
 } else {
	if (!is_protected) {
      img_colored <- magick::image_colorize(img, opacity = 100, color = color)
    } else {
      img_colored <- img  # absolutely no recoloring
    }

tmp_file <- tempfile(fileext = ".png")
    magick::image_write(img_colored, tmp_file)
    data$ColImage <- tmp_file
  }
	
  # User supplied mapping (uses aes)
  if (!is.null(mapping)) {

    if (!is.null(data)) {
      df <- data
      df$image <- df$ColImage
    } else {
      df <- NULL
    }

return(
 	geom_image(data = df,
	mapping = mapping,
	image=df$image,
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
    mapping = aes(x = x, y = y),
    image = tmp_file,
    by = "width",
    asp = 1,
    inherit.aes = FALSE, ...)
}
