
## Stedlig mandatfordeling

library(pacman)
p_load(dplyr, tibble, tidyr, stringr, lubridate, httr2,
       jsonlite, readr, sf, httr, xml2, purrr)
# library(electoral)

# tid <- "2020K1"
# tabel_navn <- "FOLK1A"
# tot_mandater <- 175
# tot_kredsmandater <- 135
tot_tillaegsmandater <- tot_mandater - tot_kredsmandater


source("epvalg/Oversigt over xml links.R")

## Finder seneste valg
ft_valg_xml <- xml_link_oversigt %>% 
  filter(valg=="FT" & type=="fintælling") %>% 
  filter(aar == max(aar)) %>% 
  pull(xml_link)


## Link mellem opstilling og storkreds
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
# st_write(storkreds_geo_data, "data/storkreds_geo.geojson")

# storkreds_geo_data <- st_read("data/storkreds_geo.geojson", quiet = TRUE)

areal <- storkreds_geo_data %>% 
  st_make_valid() %>% 
  mutate(areal_km2 = (st_area(.)/1000^2),
         areal_valg = areal_km2*20) %>% 
  select(navn, valglandsdelsbogstav, starts_with("areal")) %>% 
  as_tibble()

sum(areal$areal_km2)

## Henter befolkningstallet
statbank_url <- "https://api.statbank.dk/v1"

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

# bef_data <- read_csv2("data/folk1a.csv")

folketal <- bef_data %>%
  rename_with(str_to_lower) %>% 
  left_join(opstilling_til_storkreds, by = c("område" = "kode")) %>% 
  filter(!is.na(storkreds_navn)) %>% 
  summarise(folketal = sum(indhold), .by = c("storkreds_nummer", "storkreds_navn"))


### Henter vaelgertal

## Import af data

ind2 <- read_xml(ft_valg_xml)

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


mandater_landsdel <- data_sted_fordeling %>%
  summarise(faktor_sum_landsdel = sum(faktor_sum), .by = "landsdel_id") %>% 
  mutate(mandater_landsdel_broek = faktor_sum_landsdel/(sum(faktor_sum_landsdel)/tot_mandater),
         nedrund = floor(mandater_landsdel_broek),
         broek = mandater_landsdel_broek-nedrund,
         rank_broek = rank(-broek),
         rest = tot_mandater-sum(nedrund),
         rest_mandat = rank_broek <= rest,
         mandater_landsdel = nedrund+rest_mandat)

## Fordeler paa kredse
kredsmandater <- data_sted_fordeling %>% 
  mutate(faktor_sum_landsdel = if_else(row_number() == 1, sum(faktor_sum), NA), .by = "landsdel_id") %>% 
  distinct(landsdel_id, storkreds_navn, faktor_sum, faktor_sum_landsdel) %>% 
  mutate(kreds_mandat_land = faktor_sum_landsdel/(sum(faktor_sum)/tot_kredsmandater),
         nedrund = floor(kreds_mandat_land),
         broek = kreds_mandat_land - nedrund,
         rank_broek = dense_rank(desc(broek)),
         rest = tot_kredsmandater - sum(nedrund, na.rm = TRUE),
         rest_mandat = rank_broek <= rest,
         kreds_mandat_landsdel = nedrund + rest_mandat) %>% 
  fill(faktor_sum_landsdel, kreds_mandat_landsdel) %>% 
  select(landsdel_id, storkreds_navn, faktor_sum, faktor_sum_landsdel, kreds_mandat_landsdel) %>% 
  mutate(kreds_mandat_stor = faktor_sum/(faktor_sum_landsdel/kreds_mandat_landsdel),
         nedrund = floor(kreds_mandat_stor),
         broek = kreds_mandat_stor - nedrund,
         rank_broek = dense_rank(desc(broek)),
         rest = kreds_mandat_landsdel - sum(nedrund),
         rest_mandat = rank_broek <= rest,
         kreds_mandat_storkreds = nedrund + rest_mandat,
         .by = "landsdel_id") 

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
    mutate(kreds_mandat_land = faktor_sum_landsdel/(sum(faktor_sum)/(tot_kredsmandater-2)),
           nedrund = floor(kreds_mandat_land),
           broek = kreds_mandat_land - nedrund,
           rank_broek = dense_rank(desc(broek)),
           rest = tot_kredsmandater-2 - sum(nedrund, na.rm = TRUE),
           rest_mandat = rank_broek <= rest,
           kreds_mandat_landsdel = nedrund + rest_mandat) %>% 
    fill(faktor_sum_landsdel, kreds_mandat_landsdel) %>% 
    select(landsdel_id, storkreds_navn, faktor_sum, faktor_sum_landsdel, kreds_mandat_landsdel) %>% 
    mutate(kreds_mandat_stor = faktor_sum/(faktor_sum_landsdel/kreds_mandat_landsdel),
           nedrund = floor(kreds_mandat_stor),
           broek = kreds_mandat_stor - nedrund,
           rank_broek = dense_rank(desc(broek)),
           rest = kreds_mandat_landsdel - sum(nedrund),
           rest_mandat = rank_broek <= rest,
           kreds_mandat_storkreds = nedrund + rest_mandat,
           .by = "landsdel_id") %>% 
    # select(landsdel_id, storkreds_navn, faktor_sum, faktor_sum_landsdel, kreds_mandat_landsdel, kreds_mandat_storkreds) %>% 
    add_row(bornholm) %>% 
    arrange(landsdel_id)
    
}

## Kobler landsdelsmandater fra 175 paa og udregner tillaegsmandater pr. landsdel.
stedlig_mandatfordeling <- kredsmandater %>% 
  left_join(mandater_landsdel %>% select(landsdel_id, mandater_landsdel), by = "landsdel_id") %>% 
  mutate(tillaegsmandater_landsdel = mandater_landsdel-sum(kreds_mandat_storkreds), .by = "landsdel_id") %>% 
  mutate(landsdel_navn = case_when(landsdel_id == "7" ~ "Hovedstaden",
                                   landsdel_id == "8" ~ "Sjælland-Syddanmark",
                                   landsdel_id == "9" ~ "Midtjylland-Nordjylland")) %>% 
  select(landsdel_navn, storkreds_navn, faktor_sum, faktor_sum_landsdel, kreds_mandat_landsdel, kreds_mandat_storkreds, tillaegsmandater_landsdel)


stedlig_mandatfordeling_json <- stedlig_mandatfordeling %>% 
  toJSON()


# myfunk <- function(){
#   stedlig_mandatfordeling %>% 
#     select(storkreds_navn, kreds_mandat_storkreds)
# }
