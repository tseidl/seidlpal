#' Show all palettes as swatches
#'
#' Draws the categorical colours, the grey, the tints, the eight sequential ramps
#' and the diverging ramp, with hex codes.
#'
#' @return Invisibly `NULL`; called for the plot.
#' @examples
#' seidl_show()
#' @export
seidl_show <- function() {
  rows <- c(
    list(categorical = c(seidl_colours, other = seidl_grey),
         tints = seidl_tints),
    stats::setNames(lapply(names(ramp_anchors), \(p) seidl_seq(9, p)), paste("seq", names(ramp_anchors))),
    list(diverging = seidl_div(9))
  )
  old <- graphics::par(mar = c(0.5, 7, 0.5, 0.5))
  on.exit(graphics::par(old))
  graphics::plot(NULL, xlim = c(0, 9), ylim = c(0, length(rows)), axes = FALSE, xlab = "", ylab = "")
  for (i in seq_along(rows)) {
    x <- rows[[i]]
    y <- length(rows) - i
    graphics::rect(seq_along(x) - 1, y + 0.1, seq_along(x) - 0.05, y + 0.9, col = x, border = NA)
    graphics::text(seq_along(x) - 0.525, y + 0.5, x, cex = 0.55,
                   col = ifelse(colorspace::coords(methods::as(colorspace::hex2RGB(x), "polarLUV"))[, 1] > 60,
                                "#2A2A26", "white"))
    graphics::mtext(names(rows)[i], side = 2, at = y + 0.5, las = 1, line = 0.5, cex = 0.8)
  }
  invisible(NULL)
}
