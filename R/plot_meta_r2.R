#' Generate summary r2 plot
#'
#' @name plot_meta_r2
#' @description A
#' @param modelExtracts All combined modelExtracts
#' @return a
#'
#' @export
plot_meta_r2 <- function(modelExtracts){
  
  # library(ggplot2)
  # library(ggtext)
  # library(stringr)
  
  # targets::tar_source()
  # targets::tar_load("modelExtracts")
  
  paletteList <- get_palette()
  modelPalette <- unname(paletteList$corePalette[1:4])
  names(modelPalette) <- c(
    "<b style='color:#AD6DED'>Two-step</b>",
    "<b style='color:#7D26D4'>Poisson</b>",
    "<b style='color:#4F0E99'>Step Selection</b>",
    "<b style='color:#E87D13'>Area Based</b>")
  
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
  
  r2results <- modelExtracts$r2Outputs
  
  r2results <- r2results %>% 
    mutate(method = str_extract(model, "ssf|rsf|pois|twoStep|areaBased"),
           species = str_extract(model, "OPHA|PYBI|BUCA|BUFA"),
           hypothesis = str_extract(model, "H1|H2"),
           classLandscape = str_extract(model, "binary|continuous")) %>% 
    mutate(species = case_when(
      species == "OPHA" ~ "Ophiophagus hannah",
      species == "PYBI" ~ "Python bivittatus",
      species == "BUCA" ~ "Bungarus candidus",
      species == "BUFA" ~ "Bungarus fasciatus"
    ))
  
  r2conditional <- r2results %>% 
    group_by(method, classLandscape) %>% 
    filter(variable == "r2 conditional")
  
  r2conditional <- r2conditional %>% 
    mutate(
      hypothesis = factor(hypothesis, levels = c(
        "H1",
        "H2"
      )),
      classLandscape = factor(
        case_when(
          classLandscape == "binary" ~ "Binary Habitat Classification",
          classLandscape == "continuous" ~ "Continuous Habitat Classification"), 
        levels = c(
          "Binary Habitat Classification",
          "Continuous Habitat Classification"
        ))
    )  %>% 
    left_join(paletteList$speciesColourDF) %>% 
    mutate(
      speciesCol = glue::glue("<span style='color:{colour}'>{species}</span>"),
      speciesCol = factor(speciesCol, levels = c(
        "<span style='color:#bba07e'>Ophiophagus hannah</span>",
        "<span style='color:#6c2b05'>Python bivittatus</span>",
        "<span style='color:#322b21'>Bungarus candidus</span>",
        "<span style='color:#b28904'>Bungarus fasciatus</span>")
      )) %>% 
    mutate(method = case_when(
      method == "areaBased" ~ "Compana",
      method == "rsf" ~ "Resource Selection",
      method == "ssf" ~ "Step Selection",
      method == "twoStep" ~ "Two-step",
      method == "pois" ~ "Poisson"
    )) %>% 
    mutate(method = factor(method,
                           levels = c(
                             "Compana",
                             "Resource Selection",
                             "Two-step",
                             "Step Selection",
                             "Poisson"
                           )))
  
  r2Plot <- r2conditional %>% 
    ggplot() +
    geom_vline(xintercept = 0, linewidth = 0.5, alpha = 0.9,
               colour = paletteList$corePalette["coreGrey"],
               linetype = 1) +
    geom_hline(yintercept = seq(0.5,20.5,1), linewidth = 0.25, alpha = 0.5,
               colour = paletteList$corePalette["coreGrey"],
               linetype = 2) +
    geom_pointrange(aes(y = method, x = value, xmin = lower, xmax = upper, 
                        colour = speciesCol, shape = hypothesis),
                    fatten = 2,
                    position = position_dodge2(width = 0.75, reverse = TRUE)) +
    facet_grid(cols = vars(classLandscape),
               space = "free", scales = "free", drop = TRUE,
               switch = "y") +
    scale_colour_manual(values = speciesColVec) +
    labs(x = expression(R^2), y = "Variable",
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
      axis.title.x = element_text(face = 2, hjust = 0.5, vjust = 1),
      # legend.position = "none",
      panel.border = element_blank(),
      panel.spacing = unit(18, "pt"),
      panel.grid = element_blank(),
      legend.background = element_blank(),
      legend.direction = "vertical", 
      legend.position = "inside",
      legend.position.inside = c(1,0.21),
      legend.justification.inside = c(1,1),
      legend.box = "horizontal",
      legend.title = element_text(face = 2),
      legend.text = element_markdown(face = 3))
  
  ggsave(filename = here::here("figures", "r2Plot.png"),
         plot = r2Plot,
         width = 220, height = 190, units = "mm", dpi = 300)
  ggsave(filename = here::here("figures", "r2Plot.pdf"),
         plot = r2Plot,
         width = 220, height = 190, units = "mm")
  
  return(r2Plot)
  
}
