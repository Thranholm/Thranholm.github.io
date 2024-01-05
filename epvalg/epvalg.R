## Europaparlamentsvalg

mandattal <- 14

aar <- params$aar

type <- params$type

ep_xml <- xml_link_oversigt %>% 
  filter(aar == .env$aar & valg == "EP") %>% 
  filter(type == .env$type) %>% 
  pull(xml_link)

# ep_xml <- "https://www.dst.dk/valg/Valg1684426/xml/fintal.xml"

## Henter libraries
library(pacman)
p_load(tidyverse, xml2, readr, janitor)

ind <- read_xml(ep_xml)

stemmer_land <- hent_parti_stemmer(ind, "Land") %>% 
  mutate(stemmer = as.numeric(stemmerantal))

stemmer_land

dhont <- tibble(dhont = 1:mandattal)

kvotienter_forbund <- stemmer_land %>% 
  select(land, bogstav, navn, stemmer) %>% 
  left_join(valgforbund, by = "bogstav") %>% 
  summarise(stemmer_forbund = sum(stemmer), .by = c("land", "forbund_bogstav")) %>% 
  mutate(mandater = mandattal) %>% 
  cross_join(dhont) %>% 
  mutate(kvotient_forbund = stemmer_forbund/dhont,
         rank = rank(-kvotient_forbund),
         mandat_forbund = rank <= mandater)


ant_mandater_forbund <- kvotienter_forbund %>% 
  filter(mandat_forbund) %>% 
  summarise(ant_mandater_forbund = sum(mandat_forbund), .by = c("forbund_bogstav", "stemmer_forbund"))

dhont_forbund <- tibble(dhont = 1:max(ant_mandater_forbund$ant_mandater_forbund))

kvotienter_partier <- stemmer_land %>% 
  select(land, bogstav, navn, stemmer) %>% 
  left_join(valgforbund, by = "bogstav") %>% 
  left_join(ant_mandater_forbund, by = "forbund_bogstav") %>% 
  cross_join(dhont_forbund) %>% 
  mutate(kvotient_parti = stemmer/dhont) %>% 
  mutate(rank_parti = rank(-kvotient_parti), .by = forbund_bogstav) %>% 
  mutate(mandat_parti = rank_parti <= ant_mandater_forbund,
         kvotient_forbund = stemmer_forbund/rank_parti,
         overall_rank = dense_rank(-kvotient_forbund))


ant_mandater_partier <- kvotienter_partier %>% 
  summarise(mandater = sum(mandat_parti), .by = c("bogstav", "navn", "stemmer")) %>% 
  left_join(valgforbund, by = "bogstav") %>% 
  mutate(pct_stemmer = round_half_up(stemmer/sum(stemmer)*100, 2)) %>% 
  select(bogstav, forbund_bogstav, navn, stemmer, pct_stemmer, mandater)

rankings <- kvotienter_partier %>% 
  select(navn, forbund = forbund_bogstav, kvotient_parti, rank_parti, kvotient_forbund, overall_rank, mandat_parti)

kable(ant_mandater_partier, format = "simple")

kable(rankings %>% select(-mandat_parti), format = "simple",
      caption = "Fuld ranking")

kable(rankings %>% filter(mandat_parti) %>% select(-mandat_parti) %>% arrange(overall_rank),
      format = "simple", caption = "Rangorden for valgte mandater")

