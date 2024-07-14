## Danner by-data for tyskland


## Opdater tysk by-data

byer <- read_rds(here("fodbold_kalender", "data", "tyske_byer.rds"))

## Manglende byer
by_i_data <- kampprogram_geo %>% 
  select(spilleby) %>% 
  distinct() %>% 
  arrange(spilleby) %>% 
  mutate(findes = spilleby %in% byer$name) %>% 
  print(n = 50)

manglende_byer <- by_i_data %>% 
  filter(!findes) %>% 
  pull(spilleby)

## Henter byer

ny_geo_headers <- c("X-RapidAPI-Host" = "geography2.p.rapidapi.com",
                    "X-RapidAPI-Key" = secret_decrypt(Sys.getenv("PASS"), "FODBOLD_KEY"))

byer_list <- list(NULL)
for (i in seq_along(manglende_byer)){
  
  find_by <- request(paste0("https://geography2.p.rapidapi.com/cities?page=1&pagesize=10&name=", 
                            manglende_byer[i])) %>% 
    req_headers(!!!ny_geo_headers) %>% 
    req_perform()
  

  byer_df <- find_by %>% 
    resp_body_string() %>% 
    fromJSON() %>% 
    pluck("cities") %>% 
    filter(population == max(population))
    
  byer_list[[i]] <- byer_df
  
  Sys.sleep(2)
}

byer_update <- byer_list %>% 
  bind_rows() %>% 
  unnest(location) %>% 
  bind_rows(byer) %>% 
  distinct()

## Gemmer nyt datasæt

saveRDS(byer_update, here("fodbold_kalender", "data", "tyske_byer.rds"))


