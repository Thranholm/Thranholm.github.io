## Europaparlamentsvalg


mandattal <- xml_link_oversigt %>% 
  filter(valg == "EP" & aar == params$aar) %>% 
  distinct(mandater) %>% 
  pull()


stemmer_land <- py$land %>% 
  clean_names() %>% 
  mutate(stemmer_antal = as.numeric(stemmer_antal)) %>% 
  as_tibble()

dhont <- tibble(dhont = 1:mandattal)

kvotienter_forbund <- stemmer_land %>%
  select(type, id_parti, bogstav, navn, stemmer_antal) %>% 
  left_join(valgforbund, by = "bogstav") %>% 
  summarise(stemmer_forbund = sum(stemmer_antal), .by = c("forbund_bogstav")) %>% 
  mutate(mandater = mandattal) %>% 
  cross_join(dhont) %>% 
  mutate(kvotient_forbund = stemmer_forbund/dhont,
         rank = rank(-kvotient_forbund),
         mandat_forbund = rank <= mandater)


ant_mandater_forbund <- kvotienter_forbund %>% 
  summarise(ant_mandater_forbund = sum(mandat_forbund), .by = c("forbund_bogstav", "stemmer_forbund"))

dhont_forbund <- tibble(dhont = 1:max(ant_mandater_forbund$ant_mandater_forbund))

kvotienter_partier <- stemmer_land %>% 
  select(type, bogstav, id_parti, navn, stemmer_antal) %>% 
  left_join(valgforbund, by = "bogstav") %>% 
  left_join(ant_mandater_forbund, by = "forbund_bogstav") %>% 
  cross_join(dhont_forbund) %>% 
  mutate(kvotient_parti = stemmer_antal/dhont) %>% 
  mutate(rank_parti = rank(-kvotient_parti), .by = forbund_bogstav) %>% 
  mutate(mandat_parti = rank_parti <= ant_mandater_forbund,
         kvotient_forbund = stemmer_forbund/rank_parti,
         overall_rank = dense_rank(-kvotient_forbund))


ant_mandater_partier <- kvotienter_partier %>% 
  summarise(mandater = sum(mandat_parti), .by = c("bogstav", "id_parti", "navn", "stemmer_antal")) %>% 
  left_join(valgforbund, by = "bogstav") %>% 
  mutate(pct_stemmer = round_half_up(stemmer_antal/sum(stemmer_antal)*100, 2)) %>% 
  select(bogstav, forbund_bogstav, id_parti, navn, stemmer_antal, pct_stemmer, mandater)

rankings <- kvotienter_partier %>% 
  select(navn, forbund = forbund_bogstav, kvotient_parti, rank_parti, kvotient_forbund, overall_rank, mandat_parti)

