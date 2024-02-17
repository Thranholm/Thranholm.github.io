## Henter geo-data fra geoDB api

# byer <- kampprogram %>% 
#   distinct(spilleby) %>% 
#   mutate(spilleby = word(spilleby)) %>% 
#   mutate(spilleby = case_match(spilleby,
#                                "München" ~ "Munich",
#                                .default = spilleby))
#   
# 
# 
# byer_temp_get <- GET(url = paste(ny_geo_url,
#                                  paste0("cities",
#                                         "?name=",
#                                         byer[1,]),
#                                  sep = "/"),
#                      config = add_headers(.headers = ny_geo_headers))
# byer_temp_get_list <- fromJSON(rawToChar(byer_temp_get$content))
# 
# df <- as_tibble(byer_temp_get_list$cities)
# 
# tyske_byer_geo <- df[0,]
# 
# for (i in 1:nrow(byer)){
#   
#   byer_get_test <- GET(url = paste(ny_geo_url,
#                                    paste0("cities",
#                                           "?name=",
#                                           byer[i,]),
#                                    sep = "/"),
#                        config = add_headers(.headers = ny_geo_headers))
#   
#   byer_get_test_list <- fromJSON(rawToChar(byer_get_test$content))
#   
#   byer_df <- as_tibble(byer_get_test_list$cities) %>% 
#     filter(countryId == 82)
#   
#   tyske_byer_geo <- union(tyske_byer_geo, byer_df)
#   
#   Sys.sleep(3)
# 
# }  
# 
# 
# tyske_byer_geo <- tyske_byer_geo %>% 
#   mutate(name = case_match(name,
#                            "Munich" ~ "München",
#                            "Halle (Saale)" ~ "Saale",
#                            "Freiburg" ~ "Freiburg im Breisgau",
#                            .default = name)) %>% 
#   unnest(cols = "location")
# 
# saveRDS(tyske_byer_geo, file = "tyske_byer.Rds")

tyske_byer_geo <- readRDS("tyske_byer.Rds")

library(sf)  

tyskland_geo_json <- st_read("https://raw.githubusercontent.com/isellsoap/deutschlandGeoJSON/main/1_deutschland/2_hoch.geo.json", quiet = TRUE)

# tyskland_geo_json <- st_read("/Users/ethranholm/Downloads/germany.geojson", quiet = TRUE)


