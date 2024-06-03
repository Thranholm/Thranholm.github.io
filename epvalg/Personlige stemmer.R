


pers_stem <- py$land_person %>% 
  clean_names() %>% 
  as_tibble() %>% 
  mutate(across(c("personlige_stemmer_parti", "personlige_stemmer"), as.numeric))


pers_mandater <- pers_stem %>% 
  left_join(ant_mandater_partier %>% 
              select(id_parti, mandater),
            by = "id_parti") %>% 
  left_join(partiliste %>% 
              select(id_parti, partiliste),
            by = "id_parti") %>% 
  left_join(stemmer_land %>% 
              select(id_parti, stemmer_ialt = stemmer_antal),
            by = "id_parti")

pers_ranking
valg_sideordnet <- pers_mandater %>% 
  filter(partiliste == FALSE) %>% 
  mutate(pers_rank = rank(desc(personlige_stemmer)), .by = id_parti) %>% 
  mutate(mandat = pers_rank <= mandater) %>% 
  mutate(mandat_rang = rank(pers_rank), .by = c("id_parti", "mandat")) %>% 
  mutate(valgt_rang = if_else(mandat == TRUE, mandat_rang, NA), 
         stedfortæder_rang = if_else(mandat == TRUE, NA, mandat_rang))



loop_pers_mandater_partiliste <- pers_mandater %>% 
  filter(partiliste) %>% 
  mutate(listeorden = row_number(), .by = "id_parti") %>% 
  mutate(listestemmer = stemmer_ialt - personlige_stemmer_parti,
         fordelingstal = floor(stemmer_ialt/(mandater+1))+1,
         over_fordelingstal = personlige_stemmer >= fordelingstal,
         diff_fordelingstal = if_else(over_fordelingstal, 0, fordelingstal - personlige_stemmer),
         tildelte_partistemmer = NA_real_,
         rest_listestemmer = listestemmer - sum(tildelte_partistemmer, na.rm = TRUE))

pers_mandater_parti_liste <- loop_pers_mandater_partiliste

i <- 1

while (sum(pers_mandater_parti_liste$rest_listestemmer > 0, na.rm = TRUE)) {
  
  pers_mandater_parti_liste <- pers_mandater_parti_liste %>% 
    mutate(tildelte_partistemmer = case_when(listeorden == i & diff_fordelingstal <= rest_listestemmer ~ diff_fordelingstal,
                                             listeorden == i & diff_fordelingstal > rest_listestemmer  ~ rest_listestemmer,
                                             TRUE ~ tildelte_partistemmer),
           rest_listestemmer = listestemmer - sum(tildelte_partistemmer, na.rm = TRUE),
           .by = "id_parti")
  
  i <- i + 1
  
}

valgte_partiliste <- pers_mandater_parti_liste %>% 
  mutate(tildelte_partistemmer = if_else(is.na(tildelte_partistemmer), 0, tildelte_partistemmer),
         mandat = personlige_stemmer+tildelte_partistemmer >= fordelingstal) %>% 
  mutate(rang = rank(-(personlige_stemmer+tildelte_partistemmer), ties.method = "first"), .by = "id_parti") %>% 
  mutate(mandat = if_else(mandat == FALSE & rang <= mandater, TRUE, mandat)) %>% 
  mutate(mandat_rang = if_else(mandat, rang, NA), 
         stedfortræder_rang = )
         
  

  