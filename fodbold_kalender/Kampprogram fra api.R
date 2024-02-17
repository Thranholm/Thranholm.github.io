## Henter kampprogram for Tyskland

## Finder Tyskland 
## League info: 78, 79 og 80

liga_get <- tibble(liga_id = c(78, 79, 80),
                   get = list(NULL),
                   response = list(NULL))


for (i in 1:nrow(liga_get)) {
  
  liga_get$get[[i]] <- request(paste(base_url,
                                     paste0("fixtures",
                                            "?league=",
                                            liga_get$liga_id[i],
                                            "&season=2023"),
                                     sep = "/")) %>% 
    req_headers(!!!header) %>% 
    req_perform()
  
  liga_get$response[[i]] <- liga_get$get[[i]] %>% 
    resp_body_string() %>% 
    fromJSON() %>% 
    pluck("response") %>% 
    as_tibble()
  
}

kampprogram_fulddata <- bind_rows(liga_get$response)


kampprogram <- kampprogram_fulddata %>% 
  rowwise() %>% 
  mutate(runde = str_extract(league$round, 
                             "(?<=- ).+"), 
         stadie = str_extract(league$round,
                              ".+(?= -)"),
         aar = paste(league$season, league$season+1, sep = "/"),
         liga_id = league$id,
         liga = league$name,
         land = league$country,
         kampdato = date(fixture$date),
         kamptidspunkt = if_else(fixture$status$short == "TBD", 
                                 "TBA",
                                 format(
                                   with_tz(
                                     as_datetime(fixture$date, tz = fixture$timezone),
                                     tzone = "Europe/Berlin"), 
                                   "%H:%M")),
         ugedag = wday(kampdato, label = TRUE, abbr = FALSE, locale = "da_DK.UTF-8", week_start = 1),
         kampuge = if_else(ugedag == "Mon", isoweek(kampdato)-1, isoweek(kampdato)),
         weekend = if_else(ugedag %in% c("Fri", "Sat", "Sun", "Mon"), TRUE, FALSE),
         kamp = paste(teams$home$name, teams$away$name, sep = " - "),
         hjemmehold = teams$home$name,
         udehold = teams$away$name,
         spilleby = fixture$venue$city,
         stadion = fixture$venue$name,
         kampstatus = fixture$status$long,
         kamp_id = fixture$id,
         stadion_id = fixture$venue$id,
         timestamp = fixture$date,
         .keep = "none") %>% 
  distinct() %>% 
  ungroup() %>% 
  mutate(fiktiv_kamprunde = dense_rank(pick(kampuge, weekend)), .after = "weekend") %>% 
  mutate(kamptidspunkt = if_else(liga == "3. Liga" & runde != "38" & min(timestamp) == max(timestamp), 
                                 "TBA", 
                                 kamptidspunkt),
         .by = c("runde", "liga_id")) %>% 
  arrange(runde, match(liga, "Bundesliga"))



# stadions_get <- GET(url = paste(base_url,
#                                 paste0("venues",
#                                        "?country=germany"),
#                                 sep = "/"),
#                     config = add_headers(.headers = header))
# 
# stadions_list <- fromJSON(rawToChar(stadions_get$content))
# stadions <- as_tibble(stadions_list$response)



