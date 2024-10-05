#' Generate summary beta plot
#'
#' @name plot_meta_betas
#' @description A
#' @param movementDataAll All combined movement data list.
#' @return a
#'
#' @export
plot_meta_betas <- function(modelExtracts){
  
  # targets::tar_source()
  # targets::tar_load("modelExtracts")
  
  paletteList <- get_palette()
  modelPalette <- unname(paletteList$corePalette[1:4])
  names(modelPalette) <- c(
    "<b style='color:#AD6DED'>Two-step</b>",
    "<b style='color:#7D26D4'>Poisson</b>",
    "<b style='color:#4F0E99'>Step Selection</b>",
    "<b style='color:#E87D13'>Area Based</b>")
  
  metaBetas <- modelExtracts$betasOutputs
  
  # library(dplyr)
  # library(ggplot2)
  # library(ggtext)
  # library(stringr)
  
  betasOutputs <- metaBetas %>% 
    mutate(method = str_extract(model, "ssf|rsf|pois|twoStep|areaBased"),
           species = str_extract(model, "OPHA|PYBI|BUCA|BUFA"),
           hypothesis = str_extract(model, "H1|H2"),
           classLandscape = str_extract(model, "binary|continuous")) %>% 
    mutate(species = case_when(
      species == "OPHA" ~ "Ophiophagus hannah",
      species == "PYBI" ~ "Python bivittatus",
      species == "BUCA" ~ "Bungarus candidus",
      species == "BUFA" ~ "Bungarus fasciatus"
    )) %>% 
    filter(!.variable == "b_Intercept") %>% 
    mutate(
    hypothesis = factor(hypothesis, levels = c(
      "H1",
      "H2"
    )),
    method = factor(method, levels = c(
      "areaBased",
      "rsf",
      "twoStep",
      "ssf",
      "pois"
    )),
    classLandscape = factor(
      case_when(
        classLandscape == "binary" ~ "Binary Habitat Classification",
        classLandscape == "continuous" ~ "Continuous Habitat Classification"), 
      levels = c(
        "Binary Habitat Classification",
        "Continuous Habitat Classification"
      ))
    ) %>% 
    mutate(
      .variable = factor(case_when(
        .variable == "b_trackDuraScaled" ~ "\u03B2 Tracking Duration",
        .variable == "b_trackFreqScaled" ~ "\u03B2 Tracking Frequency",
        .variable == "b_sampleSizeScaled" ~ "\u03B2 Sample Size",
        .variable == "b_modelFormulamf.ss" ~ "\u03B2 Model Formula: Not Integrated",
        .variable == "b_stepDistgamma" ~ "\u03B2 Step Distribution: Gamma",
        .variable == "b_turnDistvonmises" ~ "\u03B2 Turn Distribution: Von Mises",
        .variable == "b_availablePerStepScaled" ~ "\u03B2 Available Points Per Step",
        .variable == "b_averagingMethodNaiveaverage" ~ "\u03B2 Averaging Method: Naive",
        .variable == "b_methodMCP" ~ "\u03B2 Available Area: MCP",
        .variable == "b_methodKDEhref" ~ "\u03B2 Available Area: KDE href",
        .variable == "b_areaMethodMCP" ~ "\u03B2 Available Area: MCP",
        .variable == "b_areaMethodKDEhref" ~ "\u03B2 Available Area: KDE href",
        .variable == "b_samplingPatternst" ~ "\u03B2 Sampling Pattern: Stratified",
        .variable == "b_typeIII" ~ "\u03B2 Desigen Type: III",
        .variable == "b_contourScaled" ~ "\u03B2 Available Area Contour",
        .variable == "b_availablePointsScaled" ~ "\u03B2 Available Points Multipiler",
        .variable == "b_testrandomisation" ~ "\u03B2 Compana Test: Randomisation"
      ),
      levels = rev(c(
        "\u03B2 Sample Size",
        "\u03B2 Tracking Duration",
        "\u03B2 Tracking Frequency",
        
        "\u03B2 Available Points Per Step",
        "\u03B2 Step Distribution: Gamma",
        "\u03B2 Turn Distribution: Von Mises",
        "\u03B2 Model Formula: Not Integrated",
        
        "\u03B2 Averaging Method: Naive",
        
        "\u03B2 Available Area: MCP",
        "\u03B2 Available Area: KDE href",
        "\u03B2 Available Area Contour",
        "\u03B2 Desigen Type: III",
        "\u03B2 Available Points Multipiler",
        "\u03B2 Sampling Pattern: Stratified",
        "\u03B2 Compana Test: Randomisation"
      ))
      )) %>% 
    mutate(method = case_when(
      method == "areaBased" ~ "Compana",
      method == "rsf" ~ "Resource Selection",
      method == "ssf" ~ "Step Selection",
      method == "twoStep" ~ "Two-step",
      method == "pois" ~ "Poisson")
    ) %>% 
    left_join(paletteList$speciesColourDF) %>% 
    mutate(
      speciesCol = glue::glue("<span style='color:{colour}'>{species}</span>"),
      speciesCol = factor(speciesCol, levels = c(
        "<span style='color:#bba07e'>Ophiophagus hannah</span>",
        "<span style='color:#6c2b05'>Python bivittatus</span>",
        "<span style='color:#322b21'>Bungarus candidus</span>",
        "<span style='color:#b28904'>Bungarus fasciatus</span>")
      ))
  
  speciesColDF <- paletteList$speciesColourDF %>% 
    mutate(
      speciesCol = glue::glue("<span style='color:{colour}'>{species}</span>"),
      speciesCol = factor(speciesCol, levels = c(
        "<span style='color:#bba07e'>Ophiophagus hannah</span>",
        "<span style='color:#6c2b05'>Python bivittatus</span>",
        "<span style='color:#322b21'>Bungarus candidus</span>",
        "<span style='color:#b28904'>Bungarus fasciatus</span>")
      ))
  speciesColVec <- speciesColDF$colour
  names(speciesColVec) <- speciesColDF$speciesCol
  
  plotList <- list()
  i <- 1
  for(meth in unique(betasOutputs$method)){
    for(land in unique(betasOutputs$classLandscape)){
      i <- i+1
      # meth <- "Compana"
      # land <- "Binary Habitat Classification"
      if(meth == "Compana" & land == "Continuous Habitat Classification"){
        plotList[[i]] <- NULL
        {next}
      }
      
      betaPlotSingle <- betasOutputs %>% 
        filter(classLandscape == land,
               method == meth) %>% 
        ggplot() +
        geom_vline(xintercept = 0, linewidth = 0.5, alpha = 0.9,
                   colour = paletteList$corePalette["coreGrey"],
                   linetype = 1) +
        geom_hline(yintercept = seq(0.5,20.5,1), linewidth = 0.25, alpha = 0.5,
                   colour = paletteList$corePalette["coreGrey"],
                   linetype = 2) +
        geom_pointrange(aes(y = .variable, x = .value, xmin = .lower, xmax = .upper, 
                            colour = speciesCol, shape = hypothesis),
                        fatten = 2,
                        position = position_dodge2(width = 0.75, reverse = TRUE)) +
        # geom_errorbar(aes(y = .variable, xmin = .lower, xmax = .upper, 
        #                   colour = species), width = 0) +
        # geom_point(aes(y = .variable, x = .value,
        #                   colour = species, shape = hypothesis),
        #            size = 0.75) +
        scale_colour_manual(values = speciesColVec) +
        facet_grid(rows = vars(method), cols = vars(classLandscape),
                   space = "free", scales = "free", drop = TRUE,
                   axes = "all", switch = "y") +
        labs(x = "Beta coefficient estimate", y = "Variable",
             colour = "Species", shape = "Hypothesis") +
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
          strip.text.x = element_markdown(angle = 0, hjust = 0, vjust = 1,
                                          margin = margin(0, 5, 20, 0),
                                          face = 4),
          axis.title.y = element_blank(),
          strip.placement = "outside",
          axis.title.x = element_blank(),
          # legend.position = "none",
          panel.border = element_blank(),
          panel.spacing = unit(18, "pt"),
          panel.grid = element_blank())
      
      
      plotList[[i]] <- betaPlotSingle
      
    }
  }
  
  plotList[sapply(plotList, is.null)] <- NULL
  
  combinedPlot <- wrap_plots(
    plotList[[8]],
    plotList[[9]] +
      theme(strip.text.y.left = element_blank(),
            axis.text.y.left = element_blank(),
            axis.title.y.left = element_blank()),
    plotList[[1]] +
      theme(strip.text.x.top = element_blank()),
    plotList[[2]] +
      theme(strip.text.y.left = element_blank(),
            axis.text.y.left = element_blank(),
            axis.title.y.left = element_blank(),
            strip.text.x.top = element_blank()),
    plotList[[4]] +
      theme(strip.text.x.top = element_blank()),
    plotList[[5]] +
      theme(strip.text.y.left = element_blank(),
            axis.text.y.left = element_blank(),
            axis.title.y.left = element_blank(),
            strip.text.x.top = element_blank()),
    plotList[[6]] +
      theme(strip.text.x.top = element_blank()),
    plotList[[7]] +
      theme(strip.text.y.left = element_blank(),
            axis.text.y.left = element_blank(),
            axis.title.y.left = element_blank(),
            strip.text.x.top = element_blank()),
    plotList[[3]] +
      theme(strip.text.x.top = element_blank()),
    guide_area()
  ) +
    plot_layout(guides = "collect",
                ncol = 2) &
    theme(legend.direction = "vertical", 
          legend.position = "bottom",
          legend.box = "horizontal",
          legend.title = element_text(face = 2),
          legend.text = element_markdown(face = 3))
  
  ggsave(filename = here("figures",
                         paste0("specCurve_", method, ".png")),
         plot = combinedPlot,
         width = 360, height = 280, units = "mm", dpi = 300)
  ggsave(filename = here("figures",
                         paste0("specCurve_", method, ".pdf")),
         plot = combinedPlot,
         width = 360, height = 280, units = "mm")
  
  
}