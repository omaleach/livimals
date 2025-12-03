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
                        color = NULL,
                        mapping = NULL,
                        data = NULL, ...) {

  if (!requireNamespace("magick", quietly = TRUE)) {
    stop("Package 'magick' is required")
  }

  img_path <- system.file("extdata", paste0(animal, ".png"), package = "livimals")
  if (img_path == "") stop("oops no image found for that animal!")
  img <- magick::image_read(img_path)

 # Read + recolor (optional)
if ("fill" %in% rlang::names2(mapping)) {

  # aes(fill = ...)
  fill_var <- rlang::quo_get_expr(mapping$fill)
  unique_vals <- unique(data[[fill_var]])

  for (i in unique_vals) {
    if (is.na(i)) next

    # Only recolor black silhouettes
    img_colored <-
      if (!is.null(i)) magick::image_colorize(img, opacity = 100, color = i)
      else img

    tmp_file <- tempfile(fileext = ".png")
    magick::image_write(img_colored, tmp_file)

    data[data[[fill_var]] == i, "ColImage"] <- tmp_file
  }

} else {
  # No aes(fill)
  if (!is.null(color)) {
    img_colored <- magick::image_colorize(img, opacity = 100, color = color)
  } else {
    img_colored <- img  # keep original PNG colors (borders + fill)
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
