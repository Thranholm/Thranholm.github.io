## Render ep-valg

rm(list = ls())
library(pacman)
p_load(tidyverse, quarto)

source("epvalg/Oversigt over xml links.R")

loop_aar <- xml_link_oversigt %>% 
  filter(type == "fintælling" & valg == "EP") %>% 
  filter(!is.na(xml_link)) %>% 
  pull(aar)

walk(loop_aar, ~quarto_render("EP_rapport.qmd",
                              execute_params = list(aar = .x),
                              output_file = paste("EP_rapport", paste0(.x, ".html"), sep = "_")))




