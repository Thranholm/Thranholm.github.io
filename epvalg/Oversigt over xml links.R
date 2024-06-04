## Oversigt over valg

xml_link_oversigt <- tibble(aar = seq(2009, year(Sys.time()), by = 5),
       valg = "EP") %>% 
  bind_rows(tibble(aar = c(2011, 2015, 2019, 2022),
            valg = "FT")) %>% 
  cross_join(tibble(type = c("valgaften", "fintælling"))) %>% 
  mutate(xml_link = case_when(## EP-valg
                              aar == 2009 & valg == "EP" ~ "https://www.dst.dk/valg/Valg1191212",
                              aar == 2014 & valg == "EP" ~ "https://www.dst.dk/valg/Valg1475795",
                              aar == 2019 & valg == "EP" ~ "https://www.dst.dk/valg/Valg1684426",
                              aar == 2024 & valg == "EP" ~ NA_character_,
                              
                              ## FT-valg
                              aar == 2011 & valg == "FT" ~ "https://www.dst.dk/valg/Valg1204271",
                              aar == 2015 & valg == "FT" ~ "https://www.dst.dk/valg/Valg1487635",
                              aar == 2019 & valg == "FT" ~ "https://www.dst.dk/valg/Valg1684447",
                              aar == 2022 & valg == "FT" ~ "https://www.dst.dk/valg/Valg1968094")) %>% 
  mutate(xml_link = if_else(!is.na(xml_link), 
                            if_else(type == "valgaften",
                                    paste(xml_link, "xml", "valgdag.xml", sep = "/"),
                                    paste(xml_link, "xml", "fintal.xml", sep = "/")), 
                            NA_character_),
         mandater = case_when(valg == "EP" & aar == 2009 ~ 13,
                              valg == "EP" & aar == 2014 ~ 13,
                              valg == "EP" & aar == 2019 ~ 14,
                              valg == "EP" & aar == 2024 ~ 15),
         partiliste = case_when(valg == "EP" & aar == 2009 ~ NA,
                                valg == "EP" & aar == 2014 ~ NA,
                                valg == "EP" & aar == 2019 ~ str_c("O", "Ø", "Å"),
                                valg == "EP" & aar == 2024 ~ "Ø"))






