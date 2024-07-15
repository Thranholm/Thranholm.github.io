

liga_url <-  "https://api-football-v1.p.rapidapi.com/v3/leagues"

 ligaer_get <- request(paste(base_url, "leagues", sep = "/")) %>% 
  req_headers(!!!header) %>% 
  req_perform()

 
 ligaer <- ligaer_get %>% 
   resp_body_string() %>% 
   fromJSON() %>%
   pluck("response") %>% 
   as_tibble() %>% 
   arrange(country$country)
 
## Superliga: 119s
## Jupiler: 144
## Æresdivisionen: 88
 
 ligaer[ligaer$league$id == 80,]$seasons
 
