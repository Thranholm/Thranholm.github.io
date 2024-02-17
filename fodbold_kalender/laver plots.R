
## Laver plots

## Samler by-data med kampe

p_load(ggiraph, RColorBrewer)

tyske_byer_geo_filter <- tyske_byer_geo %>% 
  semi_join(kampprogram, by = c("name" = "spilleby")) %>% 
  filter(population == max(population), .by = "name")
  

kampprogram_geo <- kampprogram %>% 
  left_join(tyske_byer_geo_filter, by = join_by(spilleby == name))

kampprogram_nested <- kampprogram_geo %>% 
  mutate(liga = factor(liga, levels = c("Bundesliga", "2. Bundesliga", "3. Liga"))) %>% 
  nest(.by = c("fiktiv_kamprunde"), .key = "kampprogram") %>% 
  mutate(dato = map_dbl(kampprogram, ~min(.x$kampdato)) %>% 
           as.Date()) %>% 
  # slice_head(n = 1) %>% 
  mutate(plot = map(kampprogram, ~ggplot() + 
                      geom_sf(data = tyskland_geo_json) +
                      geom_point_interactive(data = .x, aes(x = longitude, y = latitude, colour = liga,
                                                            tooltip=sprintf("%s<br/>%s&nbsp;%s",kamp,kampdato, kamptidspunkt))) +
                      scale_color_manual(values = c("Bundesliga" = "black",
                                                    "2. Bundesliga" = "red",
                                                    "3. Liga" = "yellow")) +
                      ggtitle(paste("Kampprogram for kamprunde med start:", min(.x$ugedag), 
                                    format(min(.x$kampdato), "%d/%m/%Y"))) +
                      theme(legend.position = "right"))) %>% 
  mutate(idag = today(),
         date_diff = dato-idag,
         før_idag = idag > dato) %>% 
  arrange(før_idag, date_diff, fiktiv_kamprunde)






