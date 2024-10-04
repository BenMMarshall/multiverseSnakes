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
  # targets::tar_load("rsfResults")
  # targets::tar_load("wrsfResults")
  # targets::tar_source()
  # 
  # library(here)
  # library(dplyr)
  # library(tidyr)
  # library(ggplot2)
  # library(ggtext)
  # library(ggnewscale)
  # library(stringr)
  # library(patchwork)
  
  paletteList <- get_palette()
  
  # method <- "ssf"
  # method <- "pois"
  # method <- "area"
  # method <- "twoStep"
  # method <- "rsf"
  # method <- "wrsf"
  # outputResults <- ssfResults
  # outputResults <- poisResults
  # outputResults <- areaBasedResults
  # outputResults <- twoStepResults
  # outputResults <- rsfResults
  # outputResults <- wrsfResults
  
  if(method == "ssf"){
    outputResults <- outputResults %>% 
      mutate("estimate" = modelAvg,
             "se" = modelAvgSE) %>% 
      mutate(hypoSupportSig = case_when(
        modelAvgLower > 0 & modelAvgUpper > 0 & species == "Ophiophagus hannah" ~ "Significant Support OPHA",
        modelAvgLower > 0 & modelAvgUpper > 0 & species == "Python bivittatus" ~ "Significant Support PYBI",
        modelAvgLower > 0 & modelAvgUpper > 0 & species == "Bungarus candidus" ~ "Significant Support BUCA",
        modelAvgLower > 0 & modelAvgUpper > 0 & species == "Bungarus fasciatus" ~ "Significant Support BUFA",
        TRUE ~ "No Support"
      ),
      lower = modelAvgLower,
      upper = modelAvgUpper) %>%  
      dplyr::select(-modelAvg, -modelAvgSE, -modelAvgLower, -modelAvgUpper,
                    -averagingMethod, -singleHabIssues)
    
  } else if(method == "area"){
    outputResults <- outputResults %>% 
      mutate("estimate" = companaHabDiff,
             "se" = companaP) %>% 
      mutate(hypoSupportSig = case_when(
        se < 0.05 & species == "Ophiophagus hannah" ~ "Significant Support OPHA",
        se < 0.05 & species == "Python bivittatus" ~ "Significant Support PYBI",
        se < 0.05 & species == "Bungarus candidus" ~ "Significant Support BUCA",
        se < 0.05 & species == "Bungarus fasciatus" ~ "Significant Support BUFA",
        TRUE ~ "No Support"
      ), lower = estimate - 2*se,
      upper = estimate + 2*se) %>%  
      dplyr::select(-companaHabDiff, -companaLambda, -companaP, -type)
    
  } else if(method == "pois"){
    # outputResults <- poisResults
    outputResults <- outputResults %>% 
      mutate("estimate" = estimateDiff,
             "se" = sd) %>% 
      mutate(hypoSupportSig = case_when(
        (estimate - 2*se) > 0 & species == "Ophiophagus hannah" ~ "Significant Support OPHA",
        (estimate - 2*se) > 0 & species == "Python bivittatus" ~ "Significant Support PYBI",
        (estimate - 2*se) > 0 & species == "Bungarus candidus" ~ "Significant Support BUCA",
        (estimate - 2*se) > 0 & species == "Bungarus fasciatus" ~ "Significant Support BUFA",
        TRUE ~ "No Support"
      ), lower = estimate - 2*se,
      upper = estimate + 2*se) %>%  
      dplyr::select(-mean, -sd, -q025, -q50, -q975, -mode, -kld, -estimateDiff,
                    -mmarginal, -emarginal, -term)
    
  } else if(method == "twoStep"){
    # check not using :log_sl_ estimates too
    outputResults <- outputResults %>% 
      dplyr::mutate("estimate" = twoStepBeta,
                    "se" = twoStepSE) %>% 
      mutate(hypoSupportSig = case_when(
        (estimate - 1.96*se) > 0 & species == "Ophiophagus hannah" ~ "Significant Support OPHA",
        (estimate - 1.96*se) > 0 & species == "Python bivittatus" ~ "Significant Support PYBI",
        (estimate - 1.96*se) > 0 & species == "Bungarus candidus" ~ "Significant Support BUCA",
        (estimate - 1.96*se) > 0 & species == "Bungarus fasciatus" ~ "Significant Support BUFA",
        TRUE ~ "No Support"
      ), lower = estimate - 1.96*se,
      upper = estimate + 1.96*se) %>%   
      dplyr::select(-twoStepBeta, -twoStepSE)
  } else if(method == "rsf"){
    # check not using :log_sl_ estimates too
    outputResults <- outputResults %>% 
      dplyr::mutate("estimate" = Estimate,
                    "se" = SE) %>% 
      mutate(hypoSupportSig = case_when(
        (estimate - 1.96*se) > 0 & species == "Ophiophagus hannah" ~ "Significant Support OPHA",
        (estimate - 1.96*se) > 0 & species == "Python bivittatus" ~ "Significant Support PYBI",
        (estimate - 1.96*se) > 0 & species == "Bungarus candidus" ~ "Significant Support BUCA",
        (estimate - 1.96*se) > 0 & species == "Bungarus fasciatus" ~ "Significant Support BUFA",
        TRUE ~ "No Support"
      ), lower = estimate - 1.96*se,
      upper = estimate + 1.96*se) %>%   
      dplyr::select(-SE, -zValue, -PrZ, -type)
  } else if(method == "wrsf"){
    wrsfSummaryList <- lapply(outputResults, summary)
    wrsfEstimatesList <- lapply(names(wrsfSummaryList), function(x){
      estDF <- data.frame(
        "estimate" = wrsfSummaryList[[x]][["CI"]][1,2],
        "lower" = wrsfSummaryList[[x]][["CI"]][1,1],
        "upper" = wrsfSummaryList[[x]][["CI"]][1,3],
        "name" = x
      )
    })
    wrsfEstimates <- do.call(rbind, wrsfEstimatesList)
    wrsfEstimates$analysis <- "wrsf"
    wrsfEstimates$classLandscape <- ifelse(str_detect(wrsfEstimates$name, "binary"),
                                           "Binary Habitat Classification",
                                           "Continuous Habitat Classification")
    wrsfEstimates$species <- str_extract(wrsfEstimates$name, "OPHA|PYBI|BUCA|BUFA")
    wrsfEstimates$hypothesis <- str_extract(wrsfEstimates$name, "H1|H2")
    outputResults <- wrsfEstimates %>% 
      mutate(species = case_when(
        species == "OPHA" ~ "Ophiophagus hannah",
        species == "PYBI" ~ "Python bivittatus",
        species == "BUCA" ~ "Bungarus candidus",
        species == "BUFA" ~ "Bungarus fasciatus"
      )) %>% 
      dplyr::select(-name)
  }
  
  if(!method == "wrsf"){
    if(method %in% c("area", "rsf")){
      outputResults$samplingPattern <- ifelse(outputResults$samplingPattern == "rd",
                                              "Random",
                                              "Stratified")
      if(method == "area"){
        outputResults$test <- ifelse(outputResults$test == "randomisation",
                                     "Randomisation",
                                     "Parametric")
      }
    } else {
      outputResults$stepDist <- ifelse(outputResults$stepDist == "gamma",
                                       "Gamma",
                                       "Exponential")
      outputResults$turnDist <- ifelse(outputResults$turnDist == "unif",
                                       "Uniform",
                                       "Von Mises")
      outputResults$modelFormula <- ifelse(outputResults$modelForm == "mf.is",
                                           "Integrated",
                                           "Standard")
    }
  }
  
  if(method %in% c("area", "rsf")){
    levelOrdering <- unique(c(
      sort(unique(outputResults$availablePoints)),
      unique(outputResults$type),
      unique(outputResults$method),
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
  
  hypoSpeciesPalette <- paletteList$speciesPalette
  names(hypoSpeciesPalette) <- paste0("Support ", names(hypoSpeciesPalette))
  hypoSpeciesPalette <- c(hypoSpeciesPalette, "No Support" = "#999999")
  hypoSpeciesPalette_sig <- paletteList$speciesPalette
  names(hypoSpeciesPalette_sig) <- paste0("Significant Support ", names(hypoSpeciesPalette_sig))
  hypoSpeciesPalette_sig <- c(hypoSpeciesPalette_sig, "No Support" = "#999999")
  
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
  # xlimits <- c(NA, NA)
  xlimits <- range(outputResults$estimate, na.rm = TRUE)
  # }
  
  if(!method == "wrsf"){
    
    outputPlotData <- outputResults %>% 
      dplyr::select(-analysis) %>% 
      dplyr::mutate(across(!matches("^estimate$|^se$"), as.character)) %>% 
      tidyr::pivot_longer(cols = !contains(c("species", "classLandscape", "hypothesis", "estimate", "se",
                                             "hypoSupportSig", "lower", "upper")),
                          names_to = "variable") %>% 
      dplyr::mutate(
        variable = case_when(
          variable == "availablePerStep" ~ "Available Points per Step",
          variable == "stepDist" ~ "Distribution of Step Lengths",
          variable == "turnDist" ~ "Distribution of Turn Angles",
          variable == "modelFormula" ~ "Model Formula (SSF or iSSF)",
          variable == "availablePoints" ~ "Available Points Multiplier",
          variable == "averagingMethod" ~ "Model Averaging Method",
          variable == "samplingPattern" ~ "Sampling Pattern",
          variable == "test" ~ "Compana Test Method",
          variable == "contour" ~ "Available Area Contour (%)",
          variable == "method" ~ "Available Area Method",
          variable == "areaMethod" ~ "Available Area Method"
        )) %>%
      dplyr::group_by(variable, value, classLandscape) %>%
      dplyr::mutate(d_medEst = estimate - median(outputResults$estimate, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(variable = factor(variable, levels = c(
        "Available Points per Step",
        "Distribution of Step Lengths",
        "Distribution of Turn Angles",
        "Model Formula (SSF or iSSF)",
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
        )) %>% 
      mutate(value = factor(value, levels = levelOrdering))
    
    outputPlotData$classLandscape <- ifelse(str_detect(outputPlotData$classLandscape, "binary"),
                                            "Binary Habitat Classification",
                                            "Continuous Habitat Classification")
    
    medData <- outputPlotData %>%
      dplyr::group_by(species, hypothesis, variable, value, classLandscape) %>%
      dplyr::summarise(modelMedEst = median(estimate, na.rm = TRUE)) %>% 
      mutate(group = paste0(species, hypothesis, classLandscape))
    
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
        TRUE ~ "No Support"
      )) %>% 
      mutate(classLandscape = ifelse(str_detect(classLandscape, "binary"),
                                     "Binary Habitat Classification",
                                     "Continuous Habitat Classification")) 
    
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
        TRUE ~ "No Support"
      )) %>% 
      mutate(hypoSupportSig = case_when(
        hypoSupport == "Support OPHA" ~ "Significant Support OPHA",
        hypoSupport == "Support PYBI" ~ "Significant Support PYBI",
        hypoSupport == "Support BUCA" ~ "Significant Support BUCA",
        hypoSupport == "Support BUFA" ~ "Significant Support BUFA",
        TRUE ~ "No Support"
      ))
    
    
    countDataLabel <- overallSpecData %>% 
      group_by(species, hypoSupportSig, hypothesis, classLandscape) %>% 
      count() %>% 
      ungroup() %>% 
      mutate(uniqueKey = paste0(species, hypoSupportSig, hypothesis, classLandscape))
    
    if(method == "area"){
      countDataLabel <- countDataLabel %>% 
        filter(classLandscape == "Binary Habitat Classification")
      overallMed <- overallMed %>% 
        filter(classLandscape == "Binary Habitat Classification")
      overallSpecData <- overallSpecData %>% 
        filter(classLandscape == "Binary Habitat Classification")
      outputPlotData <- outputPlotData %>% 
        filter(classLandscape == "Binary Habitat Classification")
      overallMed <- overallMed %>% 
        filter(classLandscape == "Binary Habitat Classification")
      medData <- medData %>% 
        filter(classLandscape == "Binary Habitat Classification")
    }
    
    missingCounts <- rbind(expand_grid(species = "Ophiophagus hannah",
                                       hypoSupportSig = c("Significant Support", "No Support"),
                                       hypothesis = "H2",
                                       classLandscape = unique(countDataLabel$classLandscape)),
                           expand_grid(species = unique(countDataLabel$species),
                                       hypoSupportSig = c("Significant Support", "No Support"),
                                       hypothesis = "H1",
                                       classLandscape = unique(countDataLabel$classLandscape))) %>% 
      rowwise() %>% 
      mutate(hypoSupportSig = ifelse(hypoSupportSig == "Significant Support",
                                     paste(hypoSupportSig,
                                           str_to_upper(paste0(substr(unlist(str_split(species, "\\s")), 1, 2),
                                                               collapse = ""))),
                                     hypoSupportSig)) %>% 
      mutate(n = NA,
             uniqueKey = paste0(species, hypoSupportSig, hypothesis, classLandscape))
    
    countDataLabelAll <- countDataLabel %>% 
      full_join(missingCounts, by = "uniqueKey") %>% 
      dplyr::select(species = species.y, hypoSupportSig = hypoSupportSig.y, hypothesis = hypothesis.y,
             classLandscape = classLandscape.y, n = n.x) %>% 
      mutate(n = ifelse(is.na(n), 0, n)) %>% 
      group_by(species, hypothesis, classLandscape) %>% 
      mutate(nTotal = sum(n),
             percent = n/nTotal*100,
             lab = paste(n, "/", nTotal)) %>% 
      left_join(paletteList$speciesColourDF) %>% 
      mutate(
        speciesCol = glue::glue("<span style='color:{colour}'>{species}</span>"),
        speciesCol = factor(speciesCol, levels = c(
          "<span style='color:#bba07e'>Ophiophagus hannah</span>",
          "<span style='color:#6c2b05'>Python bivittatus</span>",
          "<span style='color:#322b21'>Bungarus candidus</span>",
          "<span style='color:#b28904'>Bungarus fasciatus</span>")
        ))
    
    simpleBars <- countDataLabelAll %>% 
      ggplot() +
      geom_col(aes(y = classLandscape, x = percent, fill = hypoSupportSig)) +
      geom_text(data = countDataLabelAll %>% 
                  filter(!hypoSupportSig == "No Support"),
                aes(y = classLandscape, x = 50, label = lab), colour = "#ffffff",
                fontface = 2) +
      scale_fill_manual(values = hypoSpeciesPalette_sig) +
      scale_x_continuous(limits = c(0, 100)) +
      facet_grid(rows = vars(hypothesis, species)) +
      theme_void() +
      theme(axis.text.y = element_text())
    
    ggsave(filename = here("figures",
                           paste0("simpleBars_", method, ".pdf")),
           plot = simpleBars,
           width = 250, height = 150, units = "mm")
    
    (overallSpecCurve <- overallSpecData %>%
        ggplot() +
        geom_vline(xintercept = 0, linewidth = 0.65, alpha = 0.9, colour = "#403F41",
                   linetype = 2) +
        geom_hline(data = countDataLabelAll %>% 
                     filter(!hypoSupportSig == "No Support"),
                   aes(yintercept = 0.0, colour = hypoSupportSig),
                   linetype = 2) +
        geom_point(aes(x = estimate, y = index, colour = hypoSupportSig), alpha = 0.75,
                   pch = 19, size = 1.2)+
        geom_richtext(data = countDataLabelAll %>% 
                        filter(!hypoSupportSig == "No Support"),
                      aes(x = Inf, y = -Inf,
                          label = lab, fill = hypoSupportSig),
                      hjust = 0.95, vjust = 0, fontface = 4, size = 3,
                      label.colour = NA,
                      text.colour = "#ffffff") +
        scale_colour_manual(values = hypoSpeciesPalette_sig) +
        scale_fill_manual(values = hypoSpeciesPalette_sig) +
        new_scale_colour() +
        new_scale_fill() +
        geom_segment(data = overallMed,
                     aes(x = medEst, xend = medEst, y = n,
                         yend = -Inf, colour = hypoSupport),
                     alpha = 0.75, linewidth = 0.85, linetype = 1) +
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
          strip.text.x.top = element_text(face = 4, hjust = 1, vjust = 1),
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
        geom_vline(xintercept = 0, linewidth = 0.65, alpha = 0.9, colour = "#403F41",
                   linetype = 2) +
        geom_point(aes(x = estimate, y = value, colour = species, shape  = hypothesis),
                   position = position_jitter(width = 0, height = 0.2), alpha = 0.65,
                   size = 1) +
        geom_path(data = medData, aes(x = modelMedEst, y = value,
                                      group = group, colour = species),
                  alpha = 0.5, linewidth = 0.5) +
        geom_point(data = medData, aes(x = modelMedEst, y = value), colour = "#FFFFFF", 
                   alpha = 1, size = 1.5, position = position_nudge(y = 0), shape = 23) +
        geom_point(data = medData, aes(x = modelMedEst, y = value, colour = species),
                   alpha = 1, size = 1, position = position_nudge(y = 0), shape = 23) +
        geom_hline(yintercept = seq(0.5,10.5,1), linewidth = 0.5, alpha = 0.25, colour = "#403F41",
                   linetype = 2) +
        {if(!method %in% c("area"))facet_grid(rows = vars(variable),
                   cols = vars(classLandscape), scales = "free_y", space = "free", switch = "y")}+
        {if(method %in% c("area"))facet_grid(rows = vars(variable),
                                             scales = "free_y", space = "free", switch = "y")}+
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
    ggsave(filename = here("figures",
                           paste0("specCurve_", method, ".pdf")),
           plot = specComplete,
           width = 360, height = 280, units = "mm")
    
    # Single species curves ---------------------------------------------------
    
    for(sp in unique(overallSpecData$species)){
      
      # sp <- unique(overallSpecData$species)[1]
      xlimits <- overallSpecData %>%
        filter(species == sp) %>% 
        pull(estimate) %>% 
        range(na.rm = TRUE)
      
      if(xlimits[1] > 0){
        xlimits[1] <- 0
      }
      
      countDataLabel <- overallSpecData %>% 
        filter(species == sp) %>% 
        group_by(hypoSupportSig, hypothesis, classLandscape) %>% 
        count() %>% 
        ungroup() 
      
      rowsNeeded <- c(paste0("Significant Support ",
                             str_to_upper(paste0(substr(unlist(str_split(sp, "\\s")), 1, 2),
                                                 collapse = ""))),
                      "No Support")
      if(length(unique(countDataLabel$hypoSupportSig)) == 1){
        
        countDataLabel <- rbind(countDataLabel,
                                data.frame(
                                  hypoSupportSig = rowsNeeded[!rowsNeeded %in% unique(countDataLabel$hypoSupportSig)],
                                  hypothesis = countDataLabel$hypothesis[1],
                                  classLandscape = countDataLabel$classLandscape[1],
                                  n = 0
                                ))
        
      }
      
      # simpleBars <- countDataLabel %>% 
      #   complete(hypoSupportSig, hypothesis, classLandscape, fill = list(n = 0)) %>% 
      #   ggplot() +
      #   geom_col(aes(y = hypothesis, x = n, fill = hypoSupportSig)) +
      #   scale_fill_manual(values = hypoSpeciesPalette_sig) +
      #   facet_grid(rows = vars(classLandscape)) +
      #   theme_void() +
      #   theme(axis.text.y = element_text())
      # 
      # ggsave(filename = here("figures",
      #                        paste0("simpleBars_", sp, "_", method, ".pdf")),
      #        plot = simpleBars,
      #        width = 240, height = 120, units = "mm")
      
      countDataLabel <- countDataLabel %>% 
        complete(hypoSupportSig, hypothesis, classLandscape, fill = list(n = 0)) %>% 
        group_by(classLandscape, hypothesis) %>% 
        mutate(total = sum(n)) %>% 
        filter(!hypoSupportSig == "No Support") %>% 
        mutate(label = paste0(n, " | ", total))
      
      labCol <- hypoSpeciesPalette_sig[str_detect(names(hypoSpeciesPalette_sig),
                                                  str_to_upper(paste0(substr(unlist(str_split(sp, "\\s")), 1, 2),
                                                                      collapse = "")))]
      
      shapesVal <- c(17, 16)
      if(method == "area"){
        names(shapesVal) <- c("TRUE", "FALSE")
      } else {
        names(shapesVal) <- c(names(labCol), "FALSE")
      }
      
      (overallSpecCurve_species <- overallSpecData %>%
          filter(species == sp) %>% 
          ggplot() +
          geom_vline(xintercept = 0, linewidth = 0.65, alpha = 0.9, colour = "#403F41",
                     linetype = 2) +
          geom_hline(aes(yintercept = 0.0, colour = hypoSupport), linewidth = 0.65,
                     alpha = 0.9, colour = "#403F41",
                     linetype = 2) +
          {if(method %in% c("ssf", "pois", "rsf", "twoStep"))geom_errorbarh(aes(
            xmin = lower, xmax = upper,
            y = index, colour = hypoSupportSig), alpha = 0.5,
            linewidth = 0.95, height = 0)}+
          {if(method %in% c("ssf", "pois", "rsf", "twoStep"))geom_point(aes(
            x = estimate, y = index,
            colour = hypoSupportSig), alpha = 0.75,
            pch = 19, size = 1.2)}+
          # {if(method %in% c("pois", "rsf", "twoStep"))geom_errorbarh(aes(
          #   xmin = estimate-se, xmax = estimate+se,
          #   y = index, colour = hypoSupportSig), alpha = 0.5,
          #   linewidth = 0.95, height = 0)}+
          # {if(method %in% c("pois", "rsf", "twoStep"))geom_point(aes(
          #   x = estimate, y = index,
          #   colour = hypoSupportSig), alpha = 0.75,
          #   pch = 19, size = 1.2)}+
          {if(method %in% c("area"))geom_point(aes(
            x = estimate, y = index, colour = hypoSupportSig), alpha = 0.75,
            size = 1.2)}+
          geom_segment(data = overallMed %>%
                         filter(species == sp),
                       aes(x = medEst, xend = medEst, y = n,
                           yend = -Inf, colour = hypoSupportSig),
                       alpha = 0.75, linewidth = 0.65, linetype = 1) +
          geom_richtext(data = overallMed %>%
                          filter(species == sp), aes(x = medEst, y = Inf,
                                                     label = lab, fill = hypoSupportSig),
                        hjust = 0.95, vjust = 1, fontface = 4, size = 3,
                        label.colour = NA,
                        text.colour = "#ffffff") +
          geom_richtext(data = countDataLabel,
                        aes(x = Inf, y = 0,
                            label = label), fill = labCol,
                        hjust = 0.95, vjust = 0, fontface = 2, size = 3,
                        label.colour = NA,
                        text.colour = "#ffffff") +
          scale_colour_manual(values = hypoSpeciesPalette_sig) +
          scale_fill_manual(values = hypoSpeciesPalette_sig) +
          scale_shape_manual(values = shapesVal) +
          facet_grid(rows = vars(speciesCol, hypothesis),
                     cols = vars(classLandscape), space = "free", switch = "y") +
          labs(y = "", x = "Estimate") +
          # scale_x_continuous(limits = xlimits) +
          coord_cartesian(xlim = xlimits) +
          theme_bw() +
          theme(
            line = element_line(colour = "#403F41"),
            text = element_text(colour = "#403F41"),
            strip.background = element_blank(),
            strip.text = element_text(face = 2, hjust = 1, vjust = 1),
            strip.text.y.left = element_markdown(angle = 0, hjust = 1, vjust = 1,
                                                 margin = margin(0, -30, 20, -10),
                                                 face = 4),
            strip.text.x.top = element_text(face = 4, hjust = 1, vjust = 1),
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
      
      (splitSpecCurve_species <- outputPlotData %>%
          filter(species == sp) %>%
          ggplot() +
          geom_vline(xintercept = 0, linewidth = 0.65, alpha = 0.9, colour = "#403F41",
                     linetype = 2) +
          geom_point(aes(x = estimate, y = value, colour = species, shape  = hypothesis),
                     position = position_jitter(width = 0, height = 0.2), alpha = 0.65,
                     size = 1) +
          geom_path(data = medData %>%
                      filter(species == sp), aes(x = modelMedEst, y = value,
                                                 group = group, colour = species),
                    alpha = 0.5, linewidth = 0.5) +
          geom_point(data = medData %>%
                       filter(species == sp), aes(x = modelMedEst, y = value), colour = "#FFFFFF", 
                     alpha = 1, size = 1.5, position = position_nudge(y = 0), shape = 23) +
          geom_point(data = medData %>%
                       filter(species == sp), aes(x = modelMedEst, y = value, colour = species),
                     alpha = 1, size = 1, position = position_nudge(y = 0), shape = 23) +
          geom_hline(yintercept = seq(0.5,10.5,1), linewidth = 0.5, alpha = 0.25, colour = "#403F41",
                     linetype = 2) +
          {if(!method %in% c("area"))facet_grid(rows = vars(variable),
                                                cols = vars(classLandscape), scales = "free_y", space = "free", switch = "y")}+
          {if(method %in% c("area"))facet_grid(rows = vars(variable),
                                               scales = "free_y", space = "free", switch = "y")}+
          labs(y = "", x = "Estimate") +
          # scale_x_continuous(limits = xlimits) +
          coord_cartesian(xlim = xlimits) +
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
      
      (ciSpecCurve_species <- outputPlotData %>%
          filter(species == sp) %>%
          mutate(ciWidth = as.numeric(upper) - as.numeric(lower)) %>% 
          ggplot() +
          geom_vline(xintercept = 0, linewidth = 0.65, alpha = 0.9, colour = "#403F41",
                     linetype = 2) +
          geom_point(aes(x = ciWidth, y = value, colour = species, shape  = hypothesis),
                     position = position_jitter(width = 0, height = 0.2), alpha = 0.65,
                     size = 1) +
          geom_hline(yintercept = seq(0.5,10.5,1), linewidth = 0.5, alpha = 0.25, colour = "#403F41",
                     linetype = 2) +
          {if(!method %in% c("area"))facet_grid(rows = vars(variable),
                                                cols = vars(classLandscape), scales = "free_y", space = "free", switch = "y")}+
          {if(method %in% c("area"))facet_grid(rows = vars(variable),
                                               scales = "free_y", space = "free", switch = "y")}+
          labs(y = "", x = "Confidence Interval Width") +
          # scale_x_continuous(limits = xlimits) +
          # coord_cartesian(xlim = xlimits) +
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
      
      (specComplete_species <- wrap_plots(overallSpecCurve_species,
                                          splitSpecCurve_species,
                                          ciSpecCurve_species) +
          plot_layout(heights = c(1, 1, 1), guides = "collect"))
      
      ggsave(filename = here("figures",
                             paste0("specCurve_", sp, "_", method, ".png")),
             plot = specComplete_species,
             width = 360, height = 280, units = "mm", dpi = 300)
      ggsave(filename = here("figures",
                             paste0("specCurve_", sp, "_", method, ".pdf")),
             plot = specComplete_species,
             width = 360, height = 280, units = "mm")
      
    } # sp end
    
  } else {
    # WRSF --------------------------------------------------------------------
    
    xlimits <- c(NA, 3.5)
    
    outputPlotData <- outputResults %>% 
      dplyr::select(-analysis) %>% 
      dplyr::mutate(across(!matches("estimate|lower|upper"), as.character)) %>% 
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
        TRUE ~ "No Support"
      ))
    
    upperOutliers <- data.frame(
      speciesCol = factor("<span style='color:#bba07e'>Ophiophagus hannah</span>",
                          levels = c(
                            "<span style='color:#bba07e'>Ophiophagus hannah</span>",
                            "<span style='color:#6c2b05'>Python bivittatus</span>",
                            "<span style='color:#322b21'>Bungarus candidus</span>",
                            "<span style='color:#b28904'>Bungarus fasciatus</span>")
      ),
      hypothesis = "H1",
      classLandscape = "Continuous Habitat Classification",
      lab = paste0("<b>", round(outputPlotData$estimate[3], digits = 1), "</b>, 95% <i>",
                   round(outputPlotData$lower[3], digits = 1), "-",
                   round(outputPlotData$upper[3], digits = 1),
                   "<br>outlier not shown</i> \u2B9E")
    )
    
    (specComplete_wrsf <- outputPlotData %>%
        ggplot() +
        geom_vline(xintercept = 0, linewidth = 0.25, alpha = 0.9, colour = "#403F41",
                   linetype = 1) +
        # geom_hline(aes(yintercept = 0.0, colour = hypoSupport),
        #            linetype = 2) +
        geom_point(aes(x = estimate, y = hypothesis, colour = hypoSupport), alpha = 1,
                   pch = 19, size = 3)+
        geom_errorbarh(aes(xmin = lower, xmax = upper, y = hypothesis, colour = hypoSupport),
                       height = 0.1, linewidth = 1.1) +
        geom_text(aes(x = estimate, y = hypothesis, label = round(estimate, digits = 2),
                      colour = hypoSupport), hjust = 0.5, vjust = 0, size = 3, fontface = 2,
                  position = position_nudge(y = 0.15)) +
        geom_text(aes(x = lower, y = hypothesis, label = round(lower, digits = 2),
                      colour = hypoSupport), hjust = 1, vjust = 1, size = 2, fontface = 3,
                  position = position_nudge(y = -0.1)) +
        geom_text(aes(x = upper, y = hypothesis, label = round(upper, digits = 2),
                      colour = hypoSupport), hjust = 0, vjust = 1, size = 2, fontface = 3,
                  position = position_nudge(y = -0.1)) +
        geom_richtext(data = upperOutliers,
                      aes(x = xlimits[2], y = hypothesis, label = lab),
                      colour = hypoSpeciesPalette["Support OPHA"],
                      vjust = 0.5, hjust = 1, lineheight = 0.95, fontface = 3,
                      size = 2.5, label.colour = NA)+
        scale_colour_manual(values = hypoSpeciesPalette) +
        scale_fill_manual(values = hypoSpeciesPalette) +
        facet_grid(rows = vars(speciesCol, hypothesis),
                   cols = vars(classLandscape), space = "free_y", switch = "y",
                   scale = "free") +
        labs(y = "", x = "Estimate") +
        coord_cartesian(clip = "off", xlim = xlimits) +
        # scale_x_continuous(limits = xlimits) +
        theme_bw() +
        theme(
          line = element_line(colour = "#403F41"),
          text = element_text(colour = "#403F41"),
          strip.background = element_blank(),
          strip.text = element_text(face = 2, hjust = 1, vjust = 1),
          strip.text.y.left = element_markdown(angle = 0, hjust = 1, vjust = 1,
                                               margin = margin(0, -30, 20, -10),
                                               face = 4),
          strip.text.x.top = element_text(face = 4, hjust = 1, vjust = 1),
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
    
    ggsave(filename = here("figures",
                           paste0("specCurve_", method, ".png")),
           plot = specComplete_wrsf,
           width = 210, height = 120, units = "mm", dpi = 300)
    
    ggsave(filename = here("figures",
                           paste0("specCurve_", method, ".pdf")),
           plot = specComplete_wrsf,
           width = 210, height = 120, units = "mm")
    
    return(specComplete_wrsf)
  }
  
  return(specComplete)
  
}