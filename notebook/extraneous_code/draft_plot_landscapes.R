targets::tar_load("movementData_BUFA_binary_H1")
targets::tar_load("movementData_BUFA_continuous_H1")
targets::tar_source()

paletteVec <- get_palette()

landscapeBinary <- unwrap(movementData_BUFA_binary_H1$habitatRaster)
landscapeContinuous <- unwrap(movementData_BUFA_continuous_H1$habitatRaster)

library(tidyterra)
library(patchwork)
library(ggtext)

binaryPlot <- ggplot() +
  geom_spatraster(data = landscapeBinary) +
  geom_sf(data = movementData_sf, colour = "#ffffff",
          fill = paletteVec["2"], pch = 21) +
  coord_sf(expand = 0, datum = sf::st_crs(32647)) +
  scale_x_continuous(breaks = seq(800000, 900000, 200)) +
  scale_y_continuous(breaks = seq(1600000, 1700000, 200)) +
  scale_fill_gradient(high = paletteVec["BADGER"],
                      na.value = "#ffffff") +
  labs(x = "", y = "",
       fill = "<b>Hypothesised Habitat</b><br>
         <i><span style='font-size:6pt'>(Binary 0/1 of the hypothesised habitat)</span></i>",
       title = "Binary Habitat Classification") +
  theme_bw() +
  theme(
    text = element_text(colour = "#191919"),
    line = element_line(colour = "#808080"),
    plot.title = element_text(size = 12, face = 4),
    axis.text = element_text(size = 5, colour = "#808080"),
    axis.title = element_markdown(size = 7, face = 2),
    axis.title.x = element_markdown(angle = 0, hjust = 0, vjust = 0,
                                    margin = margin(t = 10, r = 0, b = 0, l = 0)),
    axis.title.y = element_markdown(angle = 0, hjust = 1, vjust = 1,
                                    margin = margin(t = 0, r = 10, b = 0, l = 0)),
    axis.ticks = element_line(size = 0.5),
    axis.ticks.length = unit(1, "mm"),
    axis.line = element_line(linewidth = 0.5),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    strip.background = element_blank(),
    strip.text = element_markdown(size = 8.5, face = 4,
                                  hjust = 0, vjust = 1),
    legend.position = "bottom",
    legend.box.margin = margin(0,0,0,0),
    legend.margin = margin(0,0,0,0),
    legend.title = element_markdown(size = 8, hjust = 0.5)) +
  guides(fill = guide_colourbar(
    direction = "horizontal",
    title.position = "top",
    label.hjust = 0.5,
    label.vjust = 1,
    nbin = 100,
    draw.llim = FALSE,
    draw.ulim = FALSE,
    ticks.linewidth = unit(0.5, "mm"),
    ticks.colour = "#191919",
    barwidth = unit(70, "mm"),
    barheight = unit(3, "mm")))

continuousPlot <- ggplot() +
  geom_spatraster(data = landscapeContinuous) +
  geom_sf(data = movementData_sf, colour = "#ffffff",
          fill = paletteVec["2"], pch = 21) +
  coord_sf(expand = 0, datum = sf::st_crs(32647)) +
  scale_x_continuous(breaks = seq(800000, 900000, 200)) +
  scale_y_continuous(breaks = seq(1600000, 1700000, 200)) +
  scale_fill_gradient(high = paletteVec["BADGER"],
                      low = "#ffffff") +
  labs(x = "", y = "",
       fill = "<b>Inverted Distance (m)</b><br>
         <i><span style='font-size:6pt'>(Highest values are within hypothesised habitat type)</span></i>",
       title = "Continuous Habitat Classification") +
  theme_bw() +
  theme(
    text = element_text(colour = "#191919"),
    line = element_line(colour = "#808080"),
    plot.title = element_text(size = 12, face = 4),
    axis.text = element_text(size = 5, colour = "#808080"),
    axis.title = element_markdown(size = 7, face = 2),
    axis.title.x = element_markdown(angle = 0, hjust = 0, vjust = 0,
                                    margin = margin(t = 10, r = 0, b = 0, l = 0)),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks = element_line(size = 0.5),
    axis.ticks.length = unit(1, "mm"),
    axis.line = element_line(linewidth = 0.5),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    strip.background = element_blank(),
    strip.text = element_markdown(size = 8.5, face = 4,
                                  hjust = 0, vjust = 1),
    legend.position = "bottom",
    legend.box.margin = margin(0,0,0,0),
    legend.margin = margin(0,0,0,0),
    legend.title = element_markdown(size = 8, hjust = 0.5)) +
  guides(fill = guide_colourbar(
    direction = "horizontal",
    title.position = "top",
    label.hjust = 0.5,
    label.vjust = 1,
    nbin = 100,
    draw.llim = FALSE,
    draw.ulim = FALSE,
    ticks.linewidth = unit(0.5, "mm"),
    ticks.colour = "#191919",
    barwidth = unit(70, "mm"),
    barheight = unit(3, "mm")))

binaryPlot + continuousPlot +
  plot_annotation(title = "BUCA: <i>Bungarus candidus</i>",
                  theme = theme(plot.title = element_markdown(face = 2)))
