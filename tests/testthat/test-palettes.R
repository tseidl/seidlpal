test_that("categorical palette has 8 valid colours", {
  expect_length(seidl_colours, 8)
  expect_true(all(grepl("^#[0-9A-F]{6}$", seidl_colours)))
  expect_error(seidl_pal(9), "8 categorical colours")
  expect_equal(unname(seidl_pal(3, other = TRUE)[3]), seidl_grey)
})

test_that("sequential ramps share lightness steps and run light to dark", {
  light <- function(x) colorspace::coords(methods::as(colorspace::hex2RGB(x), "polarLUV"))[, 1]
  ls <- lapply(names(seidlpal:::ramp_anchors), \(p) round(light(seidl_seq(9, p))))
  for (l in ls) expect_equal(l, ls[[1]])
  expect_length(ls, 8)
  expect_true(all(diff(ls[[1]]) < 0))
})

test_that("each ramp passes through its palette colour", {
  for (p in names(seidl_colours)) {
    l <- colorspace::coords(methods::as(colorspace::hex2RGB(seidl_colours[[p]]), "polarLUV"))[, 1]
    on_ramp <- seidlpal:::hcl_ramp(1, seidlpal:::ramp_anchors[[p]], l = c(l, l))
    de <- farver::compare_colour(farver::decode_colour(on_ramp), farver::decode_colour(seidl_colours[[p]]),
                                 "rgb", method = "cie2000")
    expect_lt(de, 1, label = p)
  }
})

test_that("diverging ramp is symmetric in lightness", {
  light <- function(x) round(colorspace::coords(methods::as(colorspace::hex2RGB(x), "polarLUV"))[, 1])
  l <- light(seidl_div(9))
  expect_equal(abs(l - rev(l)) <= 1, rep(TRUE, 9))
})

test_that("ggplot scales build", {
  library(ggplot2)
  p <- ggplot(mpg, aes(displ, hwy, colour = drv)) + geom_point() + scale_colour_seidl_d()
  expect_s3_class(ggplot_build(p), "ggplot_built")
  p2 <- ggplot(faithfuld, aes(waiting, eruptions, fill = density)) + geom_raster() + scale_fill_seidl_c("orange")
  expect_s3_class(ggplot_build(p2), "ggplot_built")
})
