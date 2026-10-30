#' Discrete ggplot2 scales with the categorical palette
#'
#' @param ... Passed to [ggplot2::discrete_scale()].
#' @param other If `TRUE`, the last level gets [seidl_grey].
#' @examples
#' library(ggplot2)
#' ggplot(mpg, aes(displ, hwy, colour = drv)) +
#'   geom_point() +
#'   scale_colour_seidl_d()
#' @export
scale_colour_seidl_d <- function(..., other = FALSE) {
  ggplot2::discrete_scale("colour", palette = \(n) unname(seidl_pal(n, other = other)), ...)
}

#' @rdname scale_colour_seidl_d
#' @export
scale_fill_seidl_d <- function(..., other = FALSE) {
  ggplot2::discrete_scale("fill", palette = \(n) unname(seidl_pal(n, other = other)), ...)
}

#' Continuous and binned ggplot2 scales with the sequential ramps
#'
#' Low values light, high values dark.
#'
#' @param palette One of `"blue"` (default), `"orange"`, `"forest"`, `"teal"`, `"wine"`,
#'   `"gold"`, `"pink"`, `"sky"`; see [seidl_seq()].
#' @param reverse If `TRUE`, high values light.
#' @param ... Passed to [ggplot2::scale_colour_gradientn()] or
#'   [ggplot2::scale_colour_stepsn()] (and their fill versions).
#' @examples
#' library(ggplot2)
#' ggplot(faithfuld, aes(waiting, eruptions, fill = density)) +
#'   geom_raster() +
#'   scale_fill_seidl_c()
#' @export
scale_colour_seidl_c <- function(palette = "blue", reverse = FALSE, ...) {
  ggplot2::scale_colour_gradientn(colours = seidl_seq(9, palette, reverse), ...)
}

#' @rdname scale_colour_seidl_c
#' @export
scale_fill_seidl_c <- function(palette = "blue", reverse = FALSE, ...) {
  ggplot2::scale_fill_gradientn(colours = seidl_seq(9, palette, reverse), ...)
}

#' @rdname scale_colour_seidl_c
#' @export
scale_colour_seidl_b <- function(palette = "blue", reverse = FALSE, ...) {
  ggplot2::scale_colour_stepsn(colours = seidl_seq(9, palette, reverse)[2:8], ...)
}

#' @rdname scale_colour_seidl_c
#' @export
scale_fill_seidl_b <- function(palette = "blue", reverse = FALSE, ...) {
  ggplot2::scale_fill_stepsn(colours = seidl_seq(9, palette, reverse)[2:8], ...)
}

#' Diverging ggplot2 scales (blue - neutral - orange)
#'
#' @param midpoint Data value mapped to the neutral centre.
#' @param reverse If `TRUE`, run orange to blue.
#' @param ... Passed to [ggplot2::scale_colour_gradientn()].
#' @examples
#' library(ggplot2)
#' df <- data.frame(x = 1:10, y = 1, change = seq(-1, 1.5, length.out = 10))
#' ggplot(df, aes(x, y, fill = change)) +
#'   geom_tile() +
#'   scale_fill_seidl_div()
#' @export
scale_colour_seidl_div <- function(midpoint = 0, reverse = FALSE, ...) {
  ggplot2::scale_colour_gradientn(colours = seidl_div(9, reverse),
                                  rescaler = \(x, ...) scales::rescale_mid(x, mid = midpoint), ...)
}

#' @rdname scale_colour_seidl_div
#' @export
scale_fill_seidl_div <- function(midpoint = 0, reverse = FALSE, ...) {
  ggplot2::scale_fill_gradientn(colours = seidl_div(9, reverse),
                                rescaler = \(x, ...) scales::rescale_mid(x, mid = midpoint), ...)
}

# US spellings
#' @rdname scale_colour_seidl_d
#' @export
scale_color_seidl_d <- scale_colour_seidl_d
#' @rdname scale_colour_seidl_c
#' @export
scale_color_seidl_c <- scale_colour_seidl_c
#' @rdname scale_colour_seidl_c
#' @export
scale_color_seidl_b <- scale_colour_seidl_b
#' @rdname scale_colour_seidl_div
#' @export
scale_color_seidl_div <- scale_colour_seidl_div
