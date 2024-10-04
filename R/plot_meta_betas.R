targets::tar_source()
targets::tar_load("modelExtracts")

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
  mutate(species = factor(species, levels = c(
    "Ophiophagus hannah",
    "Python bivittatus",
    "Bungarus candidus",
    "Bungarus fasciatus"
  )),
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
  classLandscape = factor(classLandscape, levels = c(
    "binary",
    "continuous"
  ))
  )

betasOutputs %>% 
  ggplot() +
  geom_pointrange(aes(y = .variable, x = .value, xmin = .lower, xmax = .upper, 
                    colour = species, shape = hypothesis),
                  fatten = 2,
                  position = position_dodge2(width = 0.75, reverse = TRUE)) +
  # geom_errorbar(aes(y = .variable, xmin = .lower, xmax = .upper, 
  #                   colour = species), width = 0) +
  # geom_point(aes(y = .variable, x = .value,
  #                   colour = species, shape = hypothesis),
  #            size = 0.75) +
  facet_grid(rows = vars(method), cols = vars(classLandscape),
             scales = "free")

             