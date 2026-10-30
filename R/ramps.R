# Anchor colours (HCL: lightness L, chroma C, hue H) for the continuous scales.
# All ramps share the same lightness steps, so step k is equally dark in every
# scale; chroma and hue are interpolated linearly between anchors.
ramp_anchors <- list(
  # the anchor at the palette colour's lightness uses its exact HCL values
  blue   = data.frame(L = c(15, 42.1, 95), C = c(25, 55.0, 5), H = c(254, 251.9, 245)),
  orange = data.frame(L = c(15, 39, 62.1, 95), C = c(28, 58, 105.2, 6), H = c(32, 29, 28.8, 70)),
  # light half pushed toward yellow-green, away from the teal ramp
  forest = data.frame(L = c(15, 26.0, 55, 95), C = c(14, 20.1, 42, 6), H = c(148, 142.9, 132, 120)),
  teal   = data.frame(L = c(15, 55.2, 95), C = c(14, 45.2, 6), H = c(182, 170.8, 170)),
  # dark end toward aubergine, light half toward lilac, away from the pink ramp
  wine   = data.frame(L = c(15, 36.9, 70, 95), C = c(30, 65.1, 34, 6), H = c(330, 349.7, 320, 310)),
  # dark end turns bronze (H 52) rather than olive
  gold   = data.frame(L = c(15, 45, 82.0, 95), C = c(22, 48, 80.8, 10), H = c(52, 60, 71.4, 85)),
  # dark half toward brick red, light end toward peach (away from wine);
  # hue written below 0 at the palette colour so interpolation stays near red
  pink   = data.frame(L = c(15, 40, 70.0, 95), C = c(26, 52, 44.9, 8), H = c(22, 12, -4.8, 30)),
  # dark half pushed toward cyan (petrol), away from the blue ramp
  sky    = data.frame(L = c(15, 50, 80.2, 95), C = c(11, 38, 41.9, 6), H = c(210, 215, 237.6, 238))
)

# n colours from dark (l[1]) to light (l[2])
hcl_ramp <- function(n, anchors, l = c(15, 95)) {
  L <- seq(l[1], l[2], length.out = n)
  C <- stats::approx(anchors$L, anchors$C, xout = L, rule = 2)$y
  H <- stats::approx(anchors$L, anchors$H, xout = L, rule = 2)$y
  colorspace::hex(colorspace::polarLUV(L, C, H), fixup = TRUE)
}

#' Sequential colours
#'
#' Light-to-dark ramps, one per categorical colour (not grey), each passing
#' through its palette colour. All share the same lightness steps, so step k is
#' equally dark in every ramp.
#'
#' @param n Number of colours.
#' @param palette One of `"blue"` (default), `"orange"`, `"forest"`, `"teal"`,
#'   `"wine"`, `"gold"`, `"pink"`, `"sky"`.
#' @param reverse If `TRUE`, run dark to light.
#' @return A character vector of hex codes, light to dark.
#' @details For discrete use (bars, lines for ordered groups) skip the lightest
#'   and darkest steps, e.g. `seidl_seq(9)[3:7]`: the lightest fade into light
#'   backgrounds and the darkest read as black.
#'
#'   Under red-green colour blindness some ramps look alike: orange and gold;
#'   forest and pink; teal, blue and sky with wine; blue and sky; teal and
#'   pink. Don't use these as contrasting scales side by side. Blue with orange
#'   or gold stays far apart.
#' @examples
#' seidl_seq(5)
#' seidl_seq(9, "orange")[3:7]
#' @export
seidl_seq <- function(n, palette = c("blue", "orange", "forest", "teal", "wine", "gold", "pink", "sky"), reverse = FALSE) {
  palette <- match.arg(palette)
  out <- rev(hcl_ramp(n, ramp_anchors[[palette]]))
  if (reverse) rev(out) else out
}

#' Diverging colours
#'
#' Blue to a light neutral centre to orange. Both arms are the sequential blue
#' and orange ramps between lightness 38 and 95, so the ends are equally dark.
#'
#' @param n Number of colours; odd `n` gives a true neutral middle colour.
#' @param reverse If `TRUE`, run orange to blue.
#' @return A character vector of hex codes.
#' @examples
#' seidl_div(7)
#' @export
seidl_div <- function(n, reverse = FALSE) {
  half <- ceiling(n / 2)
  left <- hcl_ramp(half, ramp_anchors$blue, l = c(38, 95))
  right <- rev(hcl_ramp(half, ramp_anchors$orange, l = c(38, 95)))
  mid <- if (n %% 2 == 1) colorspace::hex(colorspace::polarLUV(95, 0, 0)) else NULL
  out <- c(utils::head(left, n %/% 2), mid, utils::tail(right, n %/% 2))
  if (reverse) rev(out) else out
}
