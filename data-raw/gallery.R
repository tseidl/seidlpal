# gallery.R
# Figures for the README: palette swatches (man/figures/seidl-show.png) and
# example plots (man/figures/gallery-*.png).
# They show typical uses and the habits the palette is designed for: direct
# labels, grey for "other", first colours first, a diverging scale for change, skipping the
# extremes of a ramp for ordered groups, and blue against orange for two groups.
# Run from the package root: Rscript data-raw/gallery.R
# Needs (not package dependencies): tidyverse, ggrepel, sf, rnaturalearth, ragg.

devtools::load_all(quiet = TRUE)
library(tidyverse)
library(ggrepel)
library(sf)

bg <- "white"
ink <- "#2A2A26"
set.seed(42)

theme_gallery <- function() {
  theme_minimal(base_size = 11, base_family = "Inter") +
    theme(plot.background = element_rect(fill = bg, colour = NA),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour = alpha("#000000", 0.08), linewidth = 0.3),
          plot.title = element_text(face = "bold"),
          plot.title.position = "plot",
          legend.position = "top", legend.justification = "left",
          plot.margin = margin(10, 10, 10, 10))
}

save_gallery <- function(p, name, width = 7, height = 4.2) {
  ggsave(file.path("man/figures", paste0("gallery-", name, ".png")), p,
         width = width, height = height, dpi = 120, device = ragg::agg_png)
}

# ---- 0. Swatches of all palettes (README header) -------------------------------

ragg::agg_png("man/figures/seidl-show.png", width = 9, height = 8, units = "in", res = 120)
seidl_show()
dev.off()

# ---- 1. Lines with direct labels ------------------------------------------------

parties <- c("Social Democrats", "Conservatives", "Greens", "Liberals")
lines <- expand_grid(party = parties, year = 1990:2024) |>
  mutate(i = match(party, parties)) |>
  group_by(party) |>
  mutate(share = c(38, 34, 6, 9)[i] + c(-0.35, -0.12, 0.4, 0)[i] * (year - 1990) +
           cumsum(rnorm(n(), 0, 0.7))) |>
  ungroup() |>
  mutate(party = factor(party, levels = parties))
ends <- filter(lines, year == max(year))

p1 <- ggplot(lines, aes(year, share, colour = party)) +
  geom_line(linewidth = 1.1) +
  geom_point(data = ends, size = 2.2) +
  geom_text_repel(data = ends, aes(label = party), colour = ink, family = "Inter", size = 3.6,
                  hjust = 0, nudge_x = 1, direction = "y", xlim = c(2025, NA), min.segment.length = Inf) +
  scale_colour_seidl_d(guide = "none") +
  scale_x_continuous(breaks = seq(1990, 2020, 10), expand = expansion(mult = c(0.02, 0.25))) +
  coord_cartesian(clip = "off") +
  labs(title = "Vote shares", subtitle = "scale_colour_seidl_d() with direct labels instead of a legend",
       x = NULL, y = "%") +
  theme_gallery()
save_gallery(p1, "lines")

# ---- 2. Grey for "other" -------------------------------------------------------------

shares <- tibble(source = c("Manufacturing", "Finance", "Public sector", "Construction", "Transport", "Other"),
                 value = c(28, 22, 17, 11, 9, 13)) |>
  mutate(source = fct_inorder(source))

p2 <- ggplot(shares, aes(value, fct_rev(source), fill = source)) +
  geom_col(width = 0.7) +
  scale_fill_seidl_d(other = TRUE, guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(title = "Union members by sector", subtitle = "scale_fill_seidl_d(other = TRUE): last level in grey",
       x = "%", y = NULL) +
  theme_gallery() + theme(panel.grid.major.y = element_blank())
save_gallery(p2, "other")

# ---- 3. Stacked bars, first four colours ---------------------------------------------

sectors <- c("Industry", "Services", "Public", "Agriculture")
stack <- expand_grid(country = c("Germany", "France", "Italy", "Spain", "Poland", "Sweden"), sector = sectors) |>
  mutate(value = runif(n(), 5, 40), sector = factor(sector, levels = sectors))

p3 <- ggplot(stack, aes(value, country, fill = sector)) +
  geom_col(position = position_fill(reverse = TRUE), width = 0.7) +
  scale_fill_seidl_d(name = NULL) +
  scale_x_continuous(labels = scales::percent, expand = c(0, 0)) +
  labs(title = "Employment by sector", subtitle = "Four categories: blue, orange, forest, teal", x = NULL, y = NULL) +
  theme_gallery() + theme(panel.grid.major.y = element_blank())
save_gallery(p3, "stacked")

# ---- 4. Diverging map ---------------------------------------------------------------------
# A diverging scale needs a meaningful midpoint: here change since 2000, where 0 = no
# change. The synthetic change is derived from a level variable, so the random draws
# (and the later plots) stay as they were.

europe <- rnaturalearth::ne_countries(scale = "medium", continent = "Europe", returnclass = "sf") |>
  st_crop(xmin = -12, xmax = 35, ymin = 34, ymax = 71) |>
  mutate(density = runif(n(), 5, 70))

europe <- europe |>
  mutate(change = round((density - 45) / 3 + 3 * sin(seq_along(density)), 1))
lim <- max(abs(europe$change))

p4 <- ggplot(europe) +
  geom_sf(aes(fill = change), colour = bg, linewidth = 0.2) +
  scale_fill_seidl_div(midpoint = 0, limits = c(-lim, lim), name = "pp") +
  coord_sf(crs = st_crs(3035), expand = FALSE) +
  labs(title = "Change in union density, 2000-2024",
       subtitle = "scale_fill_seidl_div(midpoint = 0): blue falls, orange rises") +
  theme_gallery() +
  theme(panel.grid = element_blank(), axis.text = element_blank(),
        legend.position = "right", legend.key.height = unit(0.9, "cm"))
save_gallery(p4, "map-diverging", width = 6, height = 5)

# ---- 5. Ordered groups from a ramp ------------------------------------------------------

quintiles <- paste0("Q", 1:5)
ordered <- expand_grid(q = quintiles, year = 2000:2024) |>
  mutate(i = match(q, quintiles)) |>
  group_by(q) |>
  mutate(income = 20 + 12 * i + (i - 3) * 0.4 * (year - 2000) + cumsum(rnorm(n(), 0, 0.7))) |>
  ungroup() |>
  mutate(q = factor(q, levels = quintiles))
q_ends <- filter(ordered, year == max(year))

p6 <- ggplot(ordered, aes(year, income, colour = q)) +
  geom_line(linewidth = 1.1) +
  geom_point(data = q_ends, size = 2.2) +
  geom_text(data = q_ends, aes(label = q), colour = ink, family = "Inter", size = 3.6, hjust = 0, nudge_x = 0.7) +
  scale_colour_manual(values = seidl_seq(9, "orange")[3:7], guide = "none") +
  scale_x_continuous(breaks = seq(2000, 2020, 10), expand = expansion(mult = c(0.02, 0.1))) +
  coord_cartesian(clip = "off") +
  labs(title = "Income by quintile", subtitle = 'seidl_seq(9, "orange")[3:7]: ordered groups, extremes skipped',
       x = NULL, y = "Index") +
  theme_gallery()
save_gallery(p6, "ordered")

# ---- 6. Dumbbell, first two colours -------------------------------------------------

dumbbell <- tibble(country = c("Sweden", "Denmark", "Finland", "Belgium", "Italy", "Austria",
                               "Ireland", "Netherlands", "Germany", "Spain", "France", "Poland")) |>
  mutate(`2000` = runif(n(), 10, 80), `2024` = `2000` - runif(n(), 2, 18),
         country = fct_reorder(country, `2024`)) |>
  pivot_longer(c(`2000`, `2024`), names_to = "year", values_to = "density")

p7 <- ggplot(dumbbell, aes(density, country)) +
  geom_line(aes(group = country), colour = "grey75", linewidth = 1.2) +
  geom_point(aes(colour = year), size = 3) +
  scale_colour_seidl_d(name = NULL) +
  labs(title = "Union density, 2000 and 2024", subtitle = "Dumbbell plot with the first two colours, blue and orange",
       x = "%", y = NULL) +
  theme_gallery() + theme(panel.grid.major.y = element_blank())
save_gallery(p7, "dumbbell")

message("Wrote ", paste(list.files("man/figures", pattern = "^gallery-"), collapse = ", "))
