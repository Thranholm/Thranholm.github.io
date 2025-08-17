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
  filter(!is.na(spilleby)) %>% 
  pull(spilleby)

## Henter byer

ny_geo_headers <- c("X-RapidAPI-Host" = "geography2.p.rapidapi.com",
                    "X-RapidAPI-Key" = secret_decrypt(Sys.getenv("PASS"), "FODBOLD_KEY"))


find_by <- map(manglende_byer, ~request("https://geography2.p.rapidapi.com/cities?page=1&pagesize=10") %>%
      req_url_query("name" = .x) %>%
      req_headers_redacted(!!!ny_geo_headers) %>%
      req_throttle(capacity = 1, fill_time_s = 1))

byer_get <- find_by %>%
  req_perform_sequential()

byer_df <- resps_data(byer_get, \(byer_get) resp_body_string(byer_get) %>% 
                        fromJSON() %>% 
                        pluck("cities") %>% 
                        as_tibble())

byer_df <- filter(byer_df, population == max(population), .by = name)

byer_update <- byer_df %>% 
  unnest(location) %>% 
  bind_rows(byer) %>% 
  distinct()

## Gemmer nyt datasæt

saveRDS(byer_update, here("fodbold_kalender", "data", "tyske_byer.rds"))


