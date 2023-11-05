
## Lav funktion

t <- xml_find_all(ind, "Land") %>% 
  xml_attrs() %>% 
  tibble() %>% 
  unnest_wider(col = everything())

x <- read_xml(t$filnavn) %>% 
  as_list()

x2 <- lapply(x$Data$Personer, attributes)

json <- toJSON(x2)

t3 <- as_tibble_col(fromJSON(json)) %>% 
  unnest_wider(col = value) %>% 
  unnest_longer(col = names)

x3 <- read_xml(t$filnavn) %>% 
  xml_find_all("///Person") %>% 
  xml_attrs() %>% 
  as_tibble_col() %>% 
  unnest_wider(col = value)

pers_stem <- t3 %>% 
  select(Bogstav, parti = navn, tot_pers_stem = PersonligeStemmer) %>% 
  bind_cols(x3) %>% 
  rename_with(str_to_lower) %>% 
  mutate(personligestemmer = as.numeric(personligestemmer))

pers_mandater <- pers_stem %>% 
  left_join(ant_mandater_partier %>% 
              select(bogstav, mandater),
            by = "bogstav")

pers_ranking <- pers_mandater %>% 
  mutate(pers_rank = rank(desc(personligestemmer)), .by = bogstav) %>% 
  mutate(mandat = pers_rank <= mandater) %>% 
  mutate(mandat_rang = rank(pers_rank), .by = c("bogstav", "mandat")) %>% 
  mutate(valgt_rang = if_else(mandat == TRUE, mandat_rang, NA), 
         stedfortæder_rang = if_else(mandat == TRUE, NA, mandat_rang))





  