#' Plot the landscapes used for simulation and the classified used for analysis
#'
#' @name plot_analysis_landscapes
#' @description A
#' @param movementDataAll All combined movement data list.
#' @return a
#'
#' @export
plot_analysis_landscapes <- function(movementDataAll){
  
  # targets::tar_load("movementDataAll")
  # targets::tar_load("movementData_BUFA_H1_binary")
  # targets::tar_load("movementData_BUFA_H1_continuous")
  # targets::tar_source()
  
  paletteList <- get_palette()
  paletteVec <- paletteList$corePalette
  
  for(spHy in unique(str_extract(names(movementDataAll), "...._H."))){
    # spHy <- unique(str_extract(names(movementDataAll), "...._H."))[1]
    movementData_sf <- movementDataAll[str_detect(names(movementDataAll),
                                                  paste0("movementData_", spHy))][[1]]$movementData_sf
    landscapeBinary <- rast(here("data",
                                 paste0("raster", spHy, 
                                        "_binary.tif")))
    landscapeContinuous <- rast(here("data",
                                     paste0("raster", spHy, 
                                            "_continuous.tif")))
    # plot(landscapeContinuous)
    speciesColour <- paletteList$speciesPalette[str_extract(spHy, "^....")]
    
    if(str_detect(spHy, "OPHA")){
      xyBreaks <- 2000
      comboTitle <- glue::glue("<span style='color:{speciesColour}'>OPHA</span>: <i>Ophiophagus hannah</i> (King Cobra)")
    } else if(str_detect(spHy, "PYBI")){
      xyBreaks <- 2000
      comboTitle <- glue::glue("<span style='color:{speciesColour}'>PYBI</span>: <i>Python bivittatus</i> (Burmese Python)")
      
    } else if(str_detect(spHy, "BUCA")){
      xyBreaks <- 1000
      comboTitle <- glue::glue("<span style='color:{speciesColour}'>BUCA</span>: <i>Bungarus candidus</i> (Malayan Krait)")
      
    } else if(str_detect(spHy, "BUFA")){
      xyBreaks <- 500
      comboTitle <- glue::glue("<span style='color:{speciesColour}'>BUFA</span>: <i>Bungarus fasciatus</i> (Banded Krait)")
      
    }
    
    binaryPlot <- ggplot() +
      geom_spatraster(data = landscapeBinary) +
      geom_sf(data = movementData_sf, colour = "#ffffff",
              fill = speciesColour, pch = 21, alpha = 0.55, size = 0.75) +
      coord_sf(expand = 0, datum = sf::st_crs(32647)) +
      scale_x_continuous(breaks = seq(800000, 900000, xyBreaks)) +
      scale_y_continuous(breaks = seq(1500000, 1700000, xyBreaks)) +
      scale_fill_gradient(high = paletteVec["BADGER"],
                          low = "#ffffff") +
      labs(x = "Easting", y = "Northing",
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
        axis.text.y = element_text(angle = 45, hjust = 1, vjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks = element_line(linewidth = 0.5),
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
              fill = speciesColour, pch = 21, alpha = 0.55, size = 0.75) +
      coord_sf(expand = 0, datum = sf::st_crs(32647)) +
      scale_x_continuous(breaks = seq(800000, 900000, xyBreaks)) +
      scale_y_continuous(breaks = seq(1500000, 1700000, xyBreaks)) +
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
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
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
    
    combinedPlot <- binaryPlot + continuousPlot +
      plot_annotation(title = comboTitle,
                      theme = theme(plot.title = element_markdown(face = 2)))
    
    ggsave(plot = combinedPlot, filename = here("figures", paste0("landscape_plot_", spHy, ".png")),
           dpi = 300, height = 180, width = 230, units = "mm")
    ggsave(plot = combinedPlot, filename = here("figures", paste0("landscape_plot_", spHy, ".pdf")),
           height = 180, width = 230, units = "mm")
    
  }
}