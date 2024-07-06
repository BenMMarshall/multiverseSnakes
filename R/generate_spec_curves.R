#' Generate specification curves
#'
#' @name generate_spec_curves
#' @description A
#' @param outputResults 
#' @param method c("ssf", "area", "pois", "twoStep")
#' @return a
#'
#' @export
generate_spec_curves <- function(outputResults, method){
  
  # targets::tar_load("ssfResults")
  # targets::tar_load("poisResults")
  # targets::tar_load("areaBasedResults")
  # targets::tar_load("twoStepResults")
  # targets::tar_source()
  # 
  # library(here)
  # library(dplyr)
  # library(ggplot2)
  # library(ggtext)
  
  paletteList <- get_palette()
  
  # method <- "ssf"
  # method <- "pois"
  # method <- "area"
  # method <- "twoStep"
  # outputResults <- ssfResults
  # outputResults <- poisResults
  # outputResults <- areaBasedResults
  # outputResults <- twoStepResults
  
  if(method == "ssf"){
    outputResults <- outputResults %>% 
      mutate("estimate" = modelAvg) %>% 
      dplyr::select(-modelAvg, -modelAvgSE, -modelAvgLower, -modelAvgUpper,
             -averagingMethod, -singleHabIssues)
    
  } else if(method == "area"){
    outputResults <- outputResults %>% 
      mutate("estimate" = companaHabDiff) %>% 
      dplyr::select(-companaHabDiff, -companaLambda, -companaP, -type) %>% 
      dplyr::filter(classLandscape == "binary")
    
  } else if(method == "pois"){
    # outputResults <- poisResults
    outputResults <- outputResults %>% 
      mutate("estimate" = estimateDiff) %>% 
      dplyr::select(-mean, -sd, -q025, -q50, -q975, -mode, -kld, -estimateDiff,
             -mmarginal, -emarginal, -term)
    
  } else if(method == "twoStep"){
    # check not using :log_sl_ estimates too
    outputResults <- outputResults %>% 
      dplyr::mutate("estimate" = twoStepBeta) %>% 
      dplyr::select(-twoStepBeta, -twoStepSE)
  }
  
  if(method == "area"){
    levelOrdering <- unique(c(
      sort(unique(outputResults$availablePoints)),
      unique(outputResults$type),
      unique(outputResults$areaMethod),
      sort(unique(outputResults$contour)),
      sort(unique(outputResults$samplingPattern)),
      sort(unique(outputResults$test))))
  } else {
    levelOrdering <- unique(c(
      sort(unique(outputResults$availablePerStep)),
      sort(unique(outputResults$modelFormula)),
      sort(unique(outputResults$stepDist)),
      sort(unique(outputResults$turnDist))))
  }
  
  if(!method == "area"){
    outputResults$stepDist <- ifelse(outputResults$stepDist == "gamma",
                                     "Gamma",
                                     "Exponential")
    outputResults$turnDist <- ifelse(outputResults$turnDist == "unif",
                                     "Uniform",
                                     "Von Mises")
    outputResults$modelFormula <- ifelse(outputResults$modelForm == "mf.is",
                                         "Integrated",
                                         "Standard")
  } else if(method == "area"){
    outputResults$samplingPattern <- ifelse(outputResults$samplingPattern == "rd",
                                            "Random",
                                            "Stratified")
    outputResults$test <- ifelse(outputResults$test == "randomisation",
                                 "Randomisation",
                                 "Parametric")
  }
  
  outputPlotData <- outputResults %>% 
    dplyr::select(-analysis) %>% 
    dplyr::mutate(across(!contains("estimate"), as.character)) %>% 
    tidyr::pivot_longer(cols = !contains(c("species", "classLandscape", "hypothesis", "estimate")),
                        names_to = "variable") %>% 
    dplyr::mutate(
      variable = case_when(
        variable == "modelFormula" ~ "Model Formula (SSF or iSSF)",
        variable == "availablePerStep" ~ "Available Points per Step",
        variable == "stepDist" ~ "Distribution of Step Lengths",
        variable == "turnDist" ~ "Distribution of Turn Angles",
        variable == "availablePoints" ~ "Available Points Multiplier",
        variable == "averagingMethod" ~ "Model Averaging Method",
        variable == "samplingPattern" ~ "Sampling Pattern",
        variable == "test" ~ "Compana Test Method",
        variable == "contour" ~ "Available Area Contour (%)",
        variable == "areaMethod" ~ "Available Area Method"
      )) %>%
    dplyr::group_by(variable, value, classLandscape) %>%
    dplyr::mutate(d_medEst = estimate - median(outputResults$estimate, na.rm = TRUE)) %>%
    dplyr::ungroup() %>% 
    dplyr::ungroup() %>%
    dplyr::mutate(variable = factor(variable, levels = c(
      "Model Formula (SSF or iSSF)",
      "Available Points per Step",
      "Distribution of Step Lengths",
      "Distribution of Turn Angles",
      "Model Averaging Method",
      "Available Points Multiplier",
      "Sampling Pattern",
      "Compana Test Method",
      "Available Area Method",
      "Available Area Contour (%)")
    )) %>% 
    left_join(paletteList$speciesColourDF) %>% 
    mutate(
      speciesCol = glue::glue("<span style='color:{colour}'>{species}</span>"),
      speciesCol = factor(speciesCol, levels = c(
        "<span style='color:#bba07e'>Ophiophagus hannah</span>",
        "<span style='color:#6c2b05'>Python bivittatus</span>",
        "<span style='color:#322b21'>Bungarus candidus</span>",
        "<span style='color:#b28904'>Bungarus fasciatus</span>")
      ))
  
  outputPlotData$classLandscape <- ifelse(str_detect(outputPlotData$classLandscape, "binary"),
                                          "Binary Habitat Classification",
                                          "Continuous Habitat Classification")
  outputResults$classLandscape <- ifelse(str_detect(outputResults$classLandscape, "binary"),
                                         "Binary Habitat Classification",
                                         "Continuous Habitat Classification")
  
  medData <- outputPlotData %>%
    dplyr::group_by(species, variable, value, classLandscape) %>%
    dplyr::summarise(modelMedEst = median(estimate, na.rm = TRUE))
  
  # if(method %in% c("twoStep", "area")){
  #   xlimits <- as.vector(quantile(outputPlotData$estimate, probs = c(.001, .999)))
  #   upperOutliers <- outputResults %>% 
  #     group_by(classLandscape) %>% 
  #     filter(estimate < xlimits[1]) %>% 
  #     count()
  #   lowerOutliers <- outputResults %>% 
  #     group_by(classLandscape) %>% 
  #     filter(estimate > xlimits[2]) %>% 
  #     count()
  # } else {
  xlimits <- c(NA, NA)
  # }
  
  overallSpecData <- outputResults %>%
    dplyr::arrange(estimate) %>%
    group_by(species, hypothesis, classLandscape) %>% 
    dplyr::mutate(index = row_number(),
                  d_medEst = estimate - median(outputResults$estimate, na.rm = TRUE)) %>% 
    left_join(paletteList$speciesColourDF) %>% 
    mutate(
      speciesCol = glue::glue("<span style='color:{colour}'>{species}</span>"),
      speciesCol = factor(speciesCol, levels = c(
        "<span style='color:#bba07e'>Ophiophagus hannah</span>",
        "<span style='color:#6c2b05'>Python bivittatus</span>",
        "<span style='color:#322b21'>Bungarus candidus</span>",
        "<span style='color:#b28904'>Bungarus fasciatus</span>")
      )) %>% 
    mutate(hypoSupport = case_when(
      estimate > 0 & species == "Ophiophagus hannah" ~ "Support OPHA",
      estimate > 0 & species == "Python bivittatus" ~ "Support PYBI",
      estimate > 0 & species == "Bungarus candidus" ~ "Support BUCA",
      estimate > 0 & species == "Bungarus fasciatus" ~ "Support BUFA",
      TRUE ~ "No support"
    ))
  
  hypoSpeciesPalette <- paletteList$speciesPalette
  names(hypoSpeciesPalette) <- paste0("Support ", names(hypoSpeciesPalette))
  hypoSpeciesPalette <- c(hypoSpeciesPalette, "No support" = "#999999")
  
  overallMed <- overallSpecData %>%
    group_by(species, speciesCol, hypothesis, classLandscape) %>% 
    mutate(n = n()) %>%
    group_by(classLandscape, speciesCol, species, hypothesis) %>%
    summarise(medEst = median(estimate, na.rm = TRUE),
              n = n[1]) %>% 
    # left_join(paletteList$speciesColourDF) %>% 
    # mutate(lab = glue::glue("<span style='color:{colour}'>{round(medEst, digits = 2)}</span>")) %>% 
    mutate(lab = round(medEst, digits = 2)) %>%
    mutate(hypoSupport = case_when(
      medEst > 0 & species == "Ophiophagus hannah" ~ "Support OPHA",
      medEst > 0 & species == "Python bivittatus" ~ "Support PYBI",
      medEst > 0 & species == "Bungarus candidus" ~ "Support BUCA",
      medEst > 0 & species == "Bungarus fasciatus" ~ "Support BUFA",
      TRUE ~ "No support"
    ))
  
  (overallSpecCurve <- overallSpecData %>%
      ggplot() +
      geom_vline(xintercept = 0, linewidth = 0.25, alpha = 0.9, colour = "#403F41",
                 linetype = 1) +
      geom_hline(aes(yintercept = 0.0, colour = hypoSupport),
                 linetype = 2) +
      geom_point(aes(x = estimate, y = index, colour = hypoSupport), alpha = 0.75,
                 pch = 19, size = 1.2)+
      geom_segment(data = overallMed,
                   aes(x = medEst, xend = medEst, y = n,
                       yend = -Inf, colour = hypoSupport),
                   alpha = 0.75, linewidth = 0.95, linetype = 1) +
      geom_richtext(data = overallMed, aes(x = medEst, y = Inf,
                                           label = lab, fill = hypoSupport),
                    hjust = 0.95, vjust = 1, fontface = 4, size = 3,
                    label.colour = NA,
                    text.colour = "#ffffff") +
      scale_colour_manual(values = hypoSpeciesPalette) +
      scale_fill_manual(values = hypoSpeciesPalette) +
      facet_grid(rows = vars(speciesCol, hypothesis),
                 cols = vars(classLandscape), space = "free", switch = "y") +
      labs(y = "", x = "Estimate") +
      scale_x_continuous(limits = xlimits) +
      theme_bw() +
      theme(
        line = element_line(colour = "#403F41"),
        text = element_text(colour = "#403F41"),
        strip.background = element_blank(),
        strip.text = element_text(face = 2, hjust = 1, vjust = 1),
        strip.text.y.left = element_markdown(angle = 0, hjust = 1, vjust = 1,
                                             margin = margin(0, -30, 20, -10),
                                             face = 4),
        strip.text.x.top = element_blank(),
        strip.placement = "outside",
        axis.text.y.left = element_blank(),
        axis.ticks.y.left = element_blank(),
        axis.title.x = element_blank(),
        axis.line.x = element_line(),
        strip.clip = "off",
        legend.position = "none",
        panel.border = element_blank(),
        panel.spacing = unit(18, "pt"),
        panel.grid = element_blank())
  )
  
  (splitSpecCurve <- outputPlotData %>%
      ggplot() +
      geom_vline(xintercept = 0, linewidth = 0.5, alpha = 0.9, colour = "#403F41",
                 linetype = 1) +
      geom_point(aes(x = estimate, y = value, colour = species, shape  = hypothesis),
                 position = position_jitter(width = 0, height = 0.2), alpha = 0.65,
                 size = 1) +
      geom_point(data = medData, aes(x = modelMedEst, y = value), colour = "#FFFFFF", 
                 alpha = 1, size = 1.5, position = position_nudge(y = -0.45), shape = 17) +
      geom_point(data = medData, aes(x = modelMedEst, y = value, colour = species),
                 alpha = 1, size = 1, position = position_nudge(y = -0.45), shape = 17) +
      geom_hline(yintercept = seq(0.5,10.5,1), linewidth = 0.5, alpha = 0.25, colour = "#403F41",
                 linetype = 2) +
      facet_grid(rows = vars(variable),
                 cols = vars(classLandscape), scales = "free_y", space = "free", switch = "y") +
      # facet_grid(species + variable ~ classLandscape, scales = "free_y", space = "free", switch = "y") +
      labs(y = "", x = "Estimate") +
      scale_x_continuous(limits = xlimits) +
      scale_colour_manual(values = paletteList$speciesFullPalette) +
      scale_shape_manual(values = c(16, 17)) +
      theme_bw() +
      theme(
        line = element_line(colour = "#403F41"),
        text = element_text(colour = "#403F41"),
        # strip.text.y.left = element_text(angle = 0, margin = margin(-8.5,12,0,0)),
        # axis.text.y.left = element_text(margin = margin(0,-165,0,80)), # 2nd value needed to alligns with facet, 4th gives space left
        axis.ticks.y.left = element_blank(),
        axis.line.x = element_line(),
        strip.background = element_blank(),
        strip.text = element_text(face = 2, hjust = 0, vjust = 1),
        strip.text.y.left = element_markdown(angle = 0, hjust = 0, vjust = 1,
                                             margin = margin(0, 5, 20, 0),
                                             face = 4),
        strip.text.x.top = element_blank(),
        strip.placement = "outside",
        legend.position = "none",
        panel.border = element_blank(),
        panel.spacing = unit(18, "pt"),
        panel.grid = element_blank())
  )
  
  (specComplete <- wrap_plots(overallSpecCurve, splitSpecCurve) +
      plot_layout(heights = c(1.5, 1), guides = "collect"))
  
  ggsave(filename = here("figures",
                         paste0("specCurve_", method, ".png")),
         plot = specComplete,
         width = 360, height = 280, units = "mm", dpi = 300)
  
  return(specComplete)
  
}