#' The seidlpal categorical palette
#'
#' Eight colours in usage order: blue, orange, forest, teal, wine, gold, pink
#' and sky. The first colours carry most charts, so use them in this order.
#' Blue shares the hue of the house navy (#072140); blue, forest and wine are
#' the dark anchors, orange the one clear warm colour, gold, pink and sky the
#' light accents. The palette was checked under simulated deuteranopia,
#' protanopia and tritanopia: the closest pair stays at a CIEDE2000 difference
#' of about 12.7 for all eight colours, 16 for the first three.
#'
#' @format A named character vector of hex codes.
#' @export
seidl_colours <- c(
  blue   = "#3D649E",
  orange = "#E8772E",
  forest = "#1E452F",
  teal   = "#029581",
  wine   = "#982F5D",
  gold   = "#E8CB48",
  pink   = "#DF97AA",
  sky    = "#A1CCF1"
)

#' Grey for "other" or residual categories
#'
#' A light grey that stays distinct from the first six palette colours under
#' colour-vision deficiency (CIEDE2000 > 11). It collides with pink, the
#' seventh colour, so use it with at most six colours. Darker greys collide
#' with wine, forest or teal.
#'
#' @format A hex code.
#' @export
seidl_grey <- "#A5A6A8"

#' Light tints of the palette colours
#'
#' Hues of [seidl_colours] at lightness 90, for box fills in diagrams (e.g.
#' draw.io) or highlighted areas on slides, with dark text on top. Not meant
#' for data marks. Wine and pink, and forest and teal, are moved apart in hue
#' (wine toward lilac, pink toward peach, forest toward yellow-green) so their
#' tints stay distinguishable.
#'
#' @format A named character vector of hex codes.
#' @export
seidl_tints <- c(
  blue   = "#DAE2F7",
  orange = "#F7DDD6",
  forest = "#D3E8D2",
  teal   = "#C7EAE3",
  wine   = "#F8DBE9",
  gold   = "#EAE2CD",
  pink   = "#F9DCDC",
  sky    = "#D4E4F5"
)

#' Get the first n palette colours
#'
#' @param n Number of colours, 1 to 8.
#' @param other If `TRUE`, the last colour is replaced by [seidl_grey], for an
#'   "other" category. Safe up to `n = 7`; at `n = 8` grey meets pink and a
#'   warning is given.
#' @return A named character vector of hex codes.
#' @examples
#' seidl_pal(3)
#' seidl_pal(5, other = TRUE)
#' @export
seidl_pal <- function(n = 8, other = FALSE) {
  if (n > length(seidl_colours)) {
    stop("seidlpal has 8 categorical colours. For more groups, lump the rest into ",
         "'other' (seidl_grey), label lines directly, or use facets.", call. = FALSE)
  }
  out <- seidl_colours[seq_len(n)]
  if (other && n == 8) {
    warning("seidl_grey is close to pink (colour 7) under colour-vision deficiency; ",
            "with an 'other' category, use at most 7 levels (6 colours + grey).", call. = FALSE)
  }
  if (other) {
    out[n] <- seidl_grey
    names(out)[n] <- "other"
  }
  out
}
