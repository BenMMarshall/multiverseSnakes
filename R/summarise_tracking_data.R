#' Generate summary tables and plots
#'
#' @name summarise_tracking_data
#' @description A
#' @param movementDataAll All combined movement data list.
#' @return a
#'
#' @export
summarise_tracking_data <- function(movementDataAll){
  # targets::tar_load("movementDataAll")
  
  allMovementData <- do.call(rbind, list(movementDataAll$movementData_OPHA_H1_binary$movementData_sf,
                                         movementDataAll$movementData_PYBI_H1_binary$movementData_sf,
                                         movementDataAll$movementData_BUCA_H1_binary$movementData_sf,
                                         movementDataAll$movementData_BUFA_H1_binary$movementData_sf))
  
  if(class(allMovementData$datetime)[1] == "POSIXct"){
    allMovementData$t <- allMovementData$datetime
  } else {
    allMovementData$t <- as.POSIXct(allMovementData$datetime)
  }
  
  allMovementData_step <- allMovementData %>% 
    group_by(id) %>% 
    arrange(t) %>%  
    mutate(stepLength = sqrt((x - lag(x))^2 +
                               (y - lag(y))^2),
           timeLag = as.numeric(difftime(t, lag(t),
                                         units = "hours"))) %>% 
    st_drop_geometry()
  
  summaryTable <- allMovementData_step %>% 
    group_by(species, id) %>% 
    summarise(datapoints = n(),
              duration = as.numeric(difftime(max(t),
                                             min(t),
                                             units = "days")),
              timelag = paste0(round(digits = 2, mean(timeLag, na.rm = TRUE)), " ±",
                               round(digits = 2,
                                     sd(timeLag, na.rm = TRUE)/
                                       sqrt(length(stepLength)))),
              moves = sum(stepLength > 0, na.rm = TRUE),
              stepLength = paste0(round(digits = 2, mean(stepLength, na.rm = TRUE)), 
                                  " ±", round(digits = 2, sd(stepLength, na.rm = TRUE)/
                                                sqrt(length(stepLength))))
    )
  
  write.csv(summaryTable, file = here("tables", "summary_individuals.csv"),
            row.names = FALSE)
  
  dataDuraMovesSummaries <- summaryTable %>% 
    group_by(species) %>% 
    summarise(datapointsMean = mean(datapoints),
              datapointsSE = sd(datapoints)/sqrt(length(datapoints)),
              movesMean = mean(moves),
              movesSE = sd(moves)/sqrt(length(moves)),
              durationMean = mean(duration),
              durationSE = sd(duration)/sqrt(length(duration))
    )
  
  write.csv(dataDuraMovesSummaries, file = here("tables", "summary_species_dataDuraMoves.csv"),
            row.names = FALSE)
  
  timeStepSummaries <- allMovementData_step %>% 
    group_by(species) %>% 
    summarise(timelagMean = mean(timeLag, na.rm = TRUE),
              timelagSE = sd(timeLag, na.rm = TRUE)/sqrt(length(timeLag)),
              timelagMax = max(timeLag, na.rm = TRUE),
              timelagMin = min(timeLag, na.rm = TRUE),
              steplengthMean = mean(stepLength, na.rm = TRUE),
              steplengthSE = sd(stepLength, na.rm = TRUE)/sqrt(length(stepLength))
    )
  
  write.csv(timeStepSummaries, file = here("tables", "summary_species_timeStep.csv"),
            row.names = FALSE)
  
  paletteList <- get_palette()
  
  names(paletteList$speciesPalette) <- c(
    "Ophiophagus hannah",
    "Python bivittatus",
    "Bungarus candidus",
    "Bungarus fasciatus")
  
  allMovementData_plotData <- allMovementData_step %>% 
    ungroup() %>% 
    arrange(id) %>% 
    mutate(id = factor(id, levels = sort(unique(id), decreasing = TRUE))) %>% 
    left_join(paletteList$speciesColourDF) %>% 
    mutate(
      speciesCol = glue::glue("<span style='color:{colour}'>{species}</span>"),
      speciesCol = factor(speciesCol, levels = c(
        "<span style='color:#bba07e'>Ophiophagus hannah</span>",
        "<span style='color:#6c2b05'>Python bivittatus</span>",
        "<span style='color:#322b21'>Bungarus candidus</span>",
        "<span style='color:#b28904'>Bungarus fasciatus</span>")
      ))
  
  idLabels <- allMovementData_plotData %>%
    group_by(id, species, speciesCol) %>%
    summarise(t = min(t)) %>% 
    ungroup() %>% 
    mutate(lab = paste0("<b>", id, "</b> \u2B9E "))
  
  facetLabels <- allMovementData_plotData %>%
    group_by(species, speciesCol) %>%
    summarise()
  
  timeLinePlot <- allMovementData_plotData %>% 
    ggplot() +
    geom_point(aes(x = t, y = id,
                   colour = species),
               size = 1, shape = "|") +
    geom_hline(aes(yintercept = 0.25, colour = species),
               linetype = 2) +
    geom_richtext(data = idLabels,
                  aes(x = t, y = id, label = lab,
                      colour = species),
                  size = 2.5, hjust = 1, vjust = 0.5,
                  fill = NA, label.color = NA) +
    geom_richtext(data = facetLabels,
                  aes(x = as.POSIXct("2012-01-01", format = "%Y-%m-%d"),
                      y = 0, label = speciesCol),
                  fontface = 4,
                  size = 5, hjust = 0, vjust = 0,
                  fill = NA, label.color = NA, position = position_nudge(y = 0.05)) +
    scale_colour_manual(values = paletteList$speciesPalette) +
    facet_grid(rows = vars(speciesCol),
               scales = "free_y", space = "free_y", switch = "y") +
    scale_x_datetime(date_breaks = "years",
                     date_labels = "%Y",
                     limits = c(as.POSIXct("2012-01-01", format = "%Y-%m-%d"), NA),
                     expand = c(0, 0)) +
    labs(x = "Year", y = "Animal ID", colour = "Time lag larger\nthan planned") +
    theme_bw() +
    theme(
      line = element_line(colour = "grey15"),
      text = element_text(colour = "grey15"),
      plot.title = element_text(size = 10, face = 2),
      plot.subtitle = element_text(size = 6, face = 3),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.line.x = element_line(),
      axis.ticks.y = element_blank(),
      axis.title = element_text(face = 2),
      axis.title.y.left = element_blank(),
      axis.text.y.left = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(face = 2, hjust = 0, vjust = 1),
      strip.text.y.left = element_blank(),
      # strip.text.y.left = element_markdown(angle = 0, hjust = 0, vjust = 1,
      #                                      margin = margin(0, 5, 20, 0),
      #                                      face = 4),
      strip.text.x.top = element_blank(),
      strip.placement = "outside",
      legend.position = "none"
    ) +
    guides(colour = guide_legend(override.aes = list(size = 5)))
  
  ggsave(plot = timeLinePlot, file = here("figures", "timeLinePlot.png"),
         dpi = 300, width = 220, height = 160, units = "mm")
  ggsave(plot = timeLinePlot, file = here("figures", "timeLinePlot.pdf"),
         width = 220, height = 160, units = "mm")
  
  timeLabels <- timeStepSummaries %>% 
    left_join(paletteList$speciesColourDF) %>% 
    mutate(
      speciesCol = glue::glue("<span style='color:{colour}'>{species}</span>"),
      speciesCol = factor(speciesCol, levels = c(
        "<span style='color:#bba07e'>Ophiophagus hannah</span>",
        "<span style='color:#6c2b05'>Python bivittatus</span>",
        "<span style='color:#322b21'>Bungarus candidus</span>",
        "<span style='color:#b28904'>Bungarus fasciatus</span>")
      )) %>% 
    mutate(labelText = paste0(
      "Mean lag = ", 
      round(digits = 2, timelagMean),
      " ± ", 
      round(digits = 2, timelagSE),
      " hours<br>",
      "Range = ", 
      round(digits = 2, timelagMin),
      " - ", 
      round(digits = 2, timelagMax), " hours"
    ))
  
  timeLagPlot <- allMovementData_plotData %>% 
    mutate(speciesCol = factor(speciesCol, levels = c(
      "<span style='color:#b28904'>Bungarus fasciatus</span>",
      "<span style='color:#322b21'>Bungarus candidus</span>",
      "<span style='color:#6c2b05'>Python bivittatus</span>",
      "<span style='color:#bba07e'>Ophiophagus hannah</span>"
    )
    )) %>% 
    filter(!is.na(timeLag)) %>% 
    ggplot() +
    geom_density_ridges(aes(x = timeLag, y = speciesCol, fill = species),
                        alpha = 0.75, colour = NA) +
    geom_hline(aes(yintercept = speciesCol, colour = species),
               linetype = 2) +
    geom_point(data = timeLabels,
               aes(x = timelagMean, y = speciesCol, colour = species),
               alpha = 0.95, position = position_nudge(y = -0.05)) +
    geom_richtext(data = timeLabels,
                  aes(x = 167, y = speciesCol, label = labelText, colour = species),
                  vjust = 1, hjust = 1, fill = NA, label.color = NA, size = 3,
                  fontface = 3) +
    labs(x = "Time lag between tracks (hours)", y = "Density") +
    scale_fill_manual(values = paletteList$speciesPalette) +
    scale_colour_manual(values = paletteList$speciesPalette) +
    scale_x_sqrt(breaks = c(2, seq(0,12,6), seq(24, 168, 24),
                            seq(240, 800, 240)),
                 limits = c(0, 168)) +
    theme_bw() +
    theme(
      line = element_line(colour = "grey15"),
      text = element_text(colour = "grey15"),
      plot.title = element_text(size = 10, face = 2),
      plot.subtitle = element_text(size = 6, face = 3),
      # panel.grid = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.line.x = element_line(),
      axis.ticks.y = element_blank(),
      axis.title = element_blank(),
      axis.title.y.left = element_blank(),
      axis.text.y.left = element_markdown(face = 4, vjust = 0),
      strip.background = element_blank(),
      strip.text = element_text(face = 2, hjust = 0, vjust = 1),
      strip.text.y.left = element_markdown(angle = 0, hjust = 0, vjust = 1,
                                           margin = margin(0, 5, 20, 0),
                                           face = 4),
      strip.text.x.top = element_blank(),
      strip.placement = "outside",
      legend.position = "none"
    ) +
    guides(colour = guide_legend(override.aes = list(size = 5)))
  
  ggsave(plot = timeLagPlot, file = here("figures", "timeLagPlot.png"),
         dpi = 300, width = 180, height = 150, units = "mm")
  ggsave(plot = timeLagPlot, file = here("figures", "timeLagPlot.pdf"),
         width = 180, height = 150, units = "mm")
  
  return(list("tables" = list("summaryTable" = summaryTable,
                              "dataDuraMovesSummaries" = dataDuraMovesSummaries,
                              "timeStepSummarie" = timeStepSummaries),
              "plot" = list("timeLinePlot" = timeLinePlot,
                            "timeLagPlot" = timeLagPlot)))
  
}
