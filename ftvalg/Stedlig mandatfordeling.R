
## Stedlig mandatfordeling

library(pacman)
p_load(dplyr, tibble, tidyr, stringr, lubridate, httr2,
       jsonlite, readr, sf, httr, xml2, purrr)
# library(electoral)

# tid <- "2020K1"
# tabel_navn <- "FOLK1A"
# tot_mandater <- 175
# tot_kredsmandater <- 135
# tot_tillaegsmandater <- tot_mandater - tot_kredsmandater


source("epvalg/Oversigt over xml links.R")

## Finder seneste valg
ft_valg_xml <- xml_link_oversigt %>% 
  filter(valg=="FT" & type=="fintælling") %>% 
  filter(aar == max(aar)) %>% 
  pull(xml_link)


## Link mellem storkreds og kommune
tid_api <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
query <- sprintf(" query {
  DAGI_Opstillingskreds(virkningstid: \"%s\", registreringstid: \"%s\") {
    pageInfo {
      endCursor
      hasNextPage
    }
    nodes {
      kredskommunenavn
      kredskommunenummer
      navn
      opstillingskredsnummer
      storkredsLokalid
      valgkredsnummer
      registreringFra
      registreringsaktoer
      registreringTil
      virkningFra
      virkningsaktoer
      virkningTil
    }
  }
}", tid_api, tid_api)


api_opstillingskreds <- request("https://graphql.datafordeler.dk/DAGI/v2") %>% 
  req_url_query(apikey = secret_decrypt(Sys.getenv("PASS_DAGI"), "DAGI_KEY")) %>% 
  req_body_json(list(query = query)) %>% 
  req_perform()
  

opstillingskredse <- api_opstillingskreds %>% 
  resp_body_json() %>% 
  pluck("data", "DAGI_Opstillingskreds", "nodes") %>% 
  bind_rows()


kommune_til_storkreds <- opstillingskredse %>% 
  distinct(storkredsLokalid, kredskommunenummer, kredskommunenavn) %>% 
  mutate(kode = str_sub(kredskommunenummer, -3))


query_storkreds <- sprintf("query {
  DAGI_Storkreds(virkningstid: \"%s\", registreringstid: \"%s\") {
    pageInfo {
      endCursor
      hasNextPage
    }
    nodes {
      navn
      id_lokalId
      storkredsnummer
      valglandsdelLokalid
      geometri {crs wkt}
    }
  }
}", tid_api, tid_api)

api_storkreds <- request("https://graphql.datafordeler.dk/DAGI/v2") %>% 
  req_url_query(apikey = secret_decrypt(Sys.getenv("PASS_DAGI"), "DAGI_KEY")) %>% 
  req_body_json(list(query = query_storkreds)) %>% 
  req_perform()

storkreds <- api_storkreds %>% 
  resp_body_json() %>% 
  pluck("data", "DAGI_Storkreds", "nodes") %>% 
  bind_rows() %>% 
  mutate(geo_name = names(geometri)) %>% 
  pivot_wider(names_from = geo_name, values_from = geometri) %>% 
  unnest(c(crs, wkt))

kommune_til_storkreds <- kommune_til_storkreds %>% 
  left_join(storkreds %>% select(storkredsnavn = navn, storkredsnummer, id_lokalId),
  by = join_by(storkredsLokalid == id_lokalId))

## Regner areal
areal <- storkreds %>% 
  st_as_sf(wkt = "wkt", crs = unique(storkreds$crs)) %>% 
  st_make_valid() %>% 
  mutate(areal_km2 = (st_area(.)/1000^2),
         areal_valg = areal_km2*20) %>% 
  select(navn, valglandsdelLokalid, starts_with("areal")) %>% 
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
  I() %>% 
  read_csv2()

# bef_data <- read_csv2("data/folk1a.csv")

folketal <- bef_data %>%
  rename_with(str_to_lower) %>% 
  left_join(kommune_til_storkreds, by = c("område" = "kode")) %>% 
  filter(!is.na(storkredsnavn)) %>% 
  summarise(folketal = sum(indhold), .by = c("storkredsnummer", "storkredsnavn"))


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
  left_join(areal, by = c("storkredsnavn" = "navn")) %>% 
  left_join(stemmeberettigede, by = c("storkredsnavn" = "storkreds_navn")) %>% 
  mutate(faktor_sum = folketal+as.numeric(areal_valg)+stemmeberettigede)


mandater_landsdel <- data_sted_fordeling %>%
  summarise(faktor_sum_landsdel = sum(faktor_sum), .by = "valglandsdelLokalid") %>% 
  mutate(mandater_landsdel_broek = faktor_sum_landsdel/(sum(faktor_sum_landsdel)/tot_mandater),
         nedrund = floor(mandater_landsdel_broek),
         broek = mandater_landsdel_broek-nedrund,
         rank_broek = rank(-broek),
         rest = tot_mandater-sum(nedrund),
         rest_mandat = rank_broek <= rest,
         mandater_landsdel = nedrund+rest_mandat)

## Fordeler paa kredse
kredsmandater <- data_sted_fordeling %>% 
  mutate(faktor_sum_landsdel = if_else(row_number() == 1, sum(faktor_sum), NA), .by = "valglandsdelLokalid") %>% 
  distinct(valglandsdelLokalid, storkredsnavn, faktor_sum, faktor_sum_landsdel) %>% 
  mutate(kreds_mandat_land = faktor_sum_landsdel/(sum(faktor_sum)/tot_kredsmandater),
         nedrund = floor(kreds_mandat_land),
         broek = kreds_mandat_land - nedrund,
         rank_broek = dense_rank(desc(broek)),
         rest = tot_kredsmandater - sum(nedrund, na.rm = TRUE),
         rest_mandat = rank_broek <= rest,
         kreds_mandat_landsdel = nedrund + rest_mandat) %>% 
  fill(faktor_sum_landsdel, kreds_mandat_landsdel) %>% 
  select(valglandsdelLokalid, storkredsnavn, faktor_sum, faktor_sum_landsdel, kreds_mandat_landsdel) %>% 
  mutate(kreds_mandat_stor = faktor_sum/(faktor_sum_landsdel/kreds_mandat_landsdel),
         nedrund = floor(kreds_mandat_stor),
         broek = kreds_mandat_stor - nedrund,
         rank_broek = dense_rank(desc(broek)),
         rest = kreds_mandat_landsdel - sum(nedrund),
         rest_mandat = rank_broek <= rest,
         kreds_mandat_storkreds = nedrund + rest_mandat,
         .by = "valglandsdelLokalid") 

if(kredsmandater[kredsmandater$storkredsnavn=="Bornholm", "kreds_mandat_storkreds"] < 2) {
  
  cat("Der er tildelt mindre end 2 kredsmandater til Bornholm. Derfor laves en ny beregning, hvor Bornholm tildeles 2 kredsmandater forlods.\n")
  
  bornholm <- data_sted_fordeling %>% 
    filter(storkredsnavn == "Bornholm") %>% 
    select(valglandsdelLokalid, storkredsnavn, faktor_sum) %>% 
    mutate(kreds_mandat_storkreds = 2)
  
  kredsmandater <- data_sted_fordeling %>% 
    filter(storkredsnavn != "Bornholm") %>% 
    mutate(faktor_sum_landsdel = if_else(row_number() == 1, sum(faktor_sum), NA), .by = "valglandsdelLokalid") %>% 
    distinct(valglandsdelLokalid, storkredsnavn, faktor_sum, faktor_sum_landsdel) %>% 
    mutate(kreds_mandat_land = faktor_sum_landsdel/(sum(faktor_sum)/(tot_kredsmandater-2)),
           nedrund = floor(kreds_mandat_land),
           broek = kreds_mandat_land - nedrund,
           rank_broek = dense_rank(desc(broek)),
           rest = tot_kredsmandater-2 - sum(nedrund, na.rm = TRUE),
           rest_mandat = rank_broek <= rest,
           kreds_mandat_landsdel = nedrund + rest_mandat) %>% 
    fill(faktor_sum_landsdel, kreds_mandat_landsdel) %>% 
    select(valglandsdelLokalid, storkredsnavn, faktor_sum, faktor_sum_landsdel, kreds_mandat_landsdel) %>% 
    mutate(kreds_mandat_stor = faktor_sum/(faktor_sum_landsdel/kreds_mandat_landsdel),
           nedrund = floor(kreds_mandat_stor),
           broek = kreds_mandat_stor - nedrund,
           rank_broek = dense_rank(desc(broek)),
           rest = kreds_mandat_landsdel - sum(nedrund),
           rest_mandat = rank_broek <= rest,
           kreds_mandat_storkreds = nedrund + rest_mandat,
           .by = "valglandsdelLokalid") %>% 
    # select(landsdel_id, storkreds_navn, faktor_sum, faktor_sum_landsdel, kreds_mandat_landsdel, kreds_mandat_storkreds) %>% 
    add_row(bornholm) %>% 
    arrange(valglandsdelLokalid)
    
}

## Kobler landsdelsmandater fra 175 paa og udregner tillaegsmandater pr. landsdel.
stedlig_mandatfordeling <- kredsmandater %>% 
  left_join(mandater_landsdel %>% select(valglandsdelLokalid, mandater_landsdel), by = "valglandsdelLokalid") %>% 
  mutate(tillaegsmandater_landsdel = mandater_landsdel-sum(kreds_mandat_storkreds), .by = "valglandsdelLokalid") %>% 
  mutate(landsdelnavn = case_when(valglandsdelLokalid == "218522" ~ "Hovedstaden",
                                  valglandsdelLokalid == "218524" ~ "Sjælland-Syddanmark",
                                  valglandsdelLokalid == "218526" ~ "Midtjylland-Nordjylland")) %>% 
  select(landsdelnavn, storkredsnavn, faktor_sum, faktor_sum_landsdel, kreds_mandat_landsdel, kreds_mandat_storkreds, tillaegsmandater_landsdel)


stedlig_mandatfordeling_json <- stedlig_mandatfordeling %>% 
  toJSON()


# myfunk <- function(){
#   stedlig_mandatfordeling %>% 
#     select(storkreds_navn, kreds_mandat_storkreds)
# }
