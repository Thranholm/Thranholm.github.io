
## Stedlig mandatfordeling

library(pacman)

p_load(dplyr, tibble, tidyr, stringr, lubridate, httr2, 
       jsonlite, readr, sf, httr, xml2, purrr)

api_opstillingskreds <- GET("https://api.dataforsyningen.dk/opstillingskredse")

opstillingskredse <- fromJSON(rawToChar(api_opstillingskreds$content)) %>% 
  as_tibble() %>% 
  unnest(c(kommuner, storkreds), names_sep = "_")

opstilling_til_storkreds <- opstillingskredse %>% 
  distinct(storkreds_nummer, storkreds_navn, kommuner_kode, kommuner_navn) %>% 
  mutate(kode = str_sub(kommuner_kode, -3))


## Henter geo-data
api_storkreds <- GET("https://api.dataforsyningen.dk/storkredse?format=geojson")

storkreds_geo_data <- st_read(api_storkreds$content %>% rawToChar(), quiet = TRUE)

st_is_valid(storkreds_geo_data, reason = T)

areal <- storkreds_geo_data %>% 
  st_make_valid() %>% 
  mutate(areal_km2 = (st_area(.)/1000^2),
         areal_valg = areal_km2*20) %>% 
  select(navn, valglandsdelsbogstav, starts_with("areal")) %>% 
  as_tibble()

areal
sum(areal$areal_km2)

## Henter befolkningstallet
statbank_url <- "https://api.statbank.dk/v1"

tabel_navn <- "FOLK1A"

tid <- "2020K1"

bef_data <- request(paste(statbank_url, "data", tabel_navn, "CSV", 
              paste0("?",
                     paste(
                     "valuePresentation=code",
                     paste0("tid=", tid),
                     "område=*",
                     sep = "&")), 
              sep = "/")) %>% 
  req_perform() %>% 
  resp_body_string() %>% 
  read_csv2()

folketal <- bef_data %>%
  rename_with(str_to_lower) %>% 
  left_join(opstilling_til_storkreds, by = c("område" = "kode")) %>% 
  filter(!is.na(storkreds_navn)) %>% 
  summarise(folketal = sum(indhold), .by = c("storkreds_nummer", "storkreds_navn"))


### Henter vælgertal

## Import af data
fv_valg_xml <- "https://www.dst.dk/valg/Valg1968094/xml/fintal.xml"

ind2 <- read_xml(fv_valg_xml)

stemmeberettigede <- xml_find_all(ind2, "//Storkreds") %>% 
  xml_text() %>% 
  as_tibble_col(column_name = "storkreds") %>% 
  cbind(xml_find_all(ind2, "//Storkreds") %>% 
          xml_attrs() %>% 
          tibble() %>% 
          unnest_wider(col = everything())) %>% 
  mutate(stemmeberettigede = map(filnavn, ~.x %>% 
                                   read_xml() %>% 
                                   xml_find_all("//Stemmeberettigede") %>% 
                                   xml_text()) %>% 
           unlist() %>% 
           as.numeric()) %>% 
  select(storkreds, storkreds_id, landsdel_id, stemmeberettigede) %>% 
  mutate(storkreds_navn = str_remove(storkreds, "Storkreds") %>% 
           str_trim() %>% 
           str_remove("s$")) %>% 
  as_tibble()



### Samler data og regner

data_sted_fordeling <- folketal %>% 
  left_join(areal, by = c("storkreds_navn" = "navn")) %>% 
  left_join(stemmeberettigede, by = "storkreds_navn") %>% 
  mutate(faktor_sum = folketal+as.numeric(areal_valg)+stemmeberettigede)


library(electoral)


mandater_landsdel <- data_sted_fordeling %>%
  summarise(faktor_sum_landsdel = sum(faktor_sum), .by = "landsdel_id") %>% 
  mutate(mandater_landsdel_brøk = faktor_sum_landsdel/(sum(faktor_sum_landsdel)/175),
         nedrund = floor(mandater_landsdel_brøk),
         brøk = mandater_landsdel_brøk-nedrund,
         rank_brøk = rank(-brøk),
         rest = 175-sum(nedrund),
         rest_mandat = rank_brøk <= rest,
         mandater_landsdel = nedrund+rest_mandat)

## Fordeler på kredse
kredsmandater <- data_sted_fordeling %>% 
  mutate(faktor_sum_landsdel = if_else(row_number() == 1, sum(faktor_sum), NA), .by = "landsdel_id") %>% 
  distinct(landsdel_id, storkreds_navn, faktor_sum, faktor_sum_landsdel) %>% 
  mutate(kreds_mandat_land = faktor_sum_landsdel/(sum(faktor_sum)/135),
         nedrund = floor(kreds_mandat_land),
         brøk = kreds_mandat_land - nedrund,
         rank_brøk = dense_rank(desc(brøk)),
         rest = 135 - sum(nedrund, na.rm = TRUE),
         rest_mandat = rank_brøk <= rest,
         kreds_mandat_landsdel = nedrund + rest_mandat) %>% 
  fill(faktor_sum_landsdel, kreds_mandat_landsdel) %>% 
  select(landsdel_id, storkreds_navn, faktor_sum, faktor_sum_landsdel, kreds_mandat_landsdel) %>% 
  mutate(kreds_mandat_stor = faktor_sum/(faktor_sum_landsdel/kreds_mandat_landsdel),
         nedrund = floor(kreds_mandat_stor),
         brøk = kreds_mandat_stor - nedrund,
         rank_brøk = dense_rank(desc(brøk)),
         rest = kreds_mandat_landsdel - sum(nedrund),
         rest_mandat = rank_brøk <= rest,
         kreds_mandat_storkreds = nedrund + rest_mandat,
         .by = "landsdel_id") %>% 
  select(landsdel_id, storkreds_navn, faktor_sum, faktor_sum_landsdel, kreds_mandat_landsdel, kreds_mandat_storkreds)
  

if(kredsmandater[kredsmandater$storkreds_navn=="Bornholm", "kreds_mandat_storkreds"] < 2) {
  
  cat("Der er tildelt mindre end 2 kredsmandater til Bornholm. Derfor laves en ny beregning, hvor Bornholm tildeles 2 kredsmandater forlods.\n")
  
  bornholm <- data_sted_fordeling %>% 
    filter(storkreds_navn == "Bornholm") %>% 
    select(landsdel_id, storkreds_navn, faktor_sum) %>% 
    mutate(kreds_mandat_storkreds = 2)
  
  kredsmandater <- data_sted_fordeling %>% 
    filter(storkreds_navn != "Bornholm") %>% 
    mutate(faktor_sum_landsdel = if_else(row_number() == 1, sum(faktor_sum), NA), .by = "landsdel_id") %>% 
    distinct(landsdel_id, storkreds_navn, faktor_sum, faktor_sum_landsdel) %>% 
    mutate(kreds_mandat_land = faktor_sum_landsdel/(sum(faktor_sum)/133),
           nedrund = floor(kreds_mandat_land),
           brøk = kreds_mandat_land - nedrund,
           rank_brøk = dense_rank(desc(brøk)),
           rest = 133 - sum(nedrund, na.rm = TRUE),
           rest_mandat = rank_brøk <= rest,
           kreds_mandat_landsdel = nedrund + rest_mandat) %>% 
    fill(faktor_sum_landsdel, kreds_mandat_landsdel) %>% 
    select(landsdel_id, storkreds_navn, faktor_sum, faktor_sum_landsdel, kreds_mandat_landsdel) %>% 
    mutate(kreds_mandat_stor = faktor_sum/(faktor_sum_landsdel/kreds_mandat_landsdel),
           nedrund = floor(kreds_mandat_stor),
           brøk = kreds_mandat_stor - nedrund,
           rank_brøk = dense_rank(desc(brøk)),
           rest = kreds_mandat_landsdel - sum(nedrund),
           rest_mandat = rank_brøk <= rest,
           kreds_mandat_storkreds = nedrund + rest_mandat,
           .by = "landsdel_id") %>% 
    select(landsdel_id, storkreds_navn, faktor_sum, faktor_sum_landsdel, kreds_mandat_landsdel, kreds_mandat_storkreds) %>% 
    add_row(bornholm) %>% 
    arrange(landsdel_id)
    
}

## Kobler landsdelsmandater fra 175 på og udregner tillægsmandater pr. landsdel.
stedlig_mandatfordeling <- kredsmandater %>% 
  left_join(mandater_landsdel %>% select(landsdel_id, mandater_landsdel), by = "landsdel_id") %>% 
  mutate(tillægsmandater_landsdel = mandater_landsdel-sum(kreds_mandat_storkreds), .by = "landsdel_id") %>% 
  select(-mandater_landsdel)

 

tilføjer ny kode her


