## Indlæsning af pakker og sætter parametre for api

rm(list = ls())

library(pacman)
p_load(tidyverse, lubridate, httr, jsonlite, 
       devtools, janitor, stringr, httr2)

# key <- secret_make_key()

base_url <- "https://api-football-v1.p.rapidapi.com/v3"

header <- c("X-RapidAPI-Host" = "api-football-v1.p.rapidapi.com",
            "X-RapidAPI-Key" = secret_decrypt(Sys.getenv("PASS"), "FODBOLD_KEY"))

ny_geo_url <- "https://geography2.p.rapidapi.com"

ny_geo_headers <- c("X-RapidAPI-Host" = "world-geography2.p.rapidapi.com",
                    "X-RapidAPI-Key" = secret_decrypt(Sys.getenv("PASS"), "FODBOLD_KEY"))
