## Funktion, der regner valgresultater


regn_valgforbund_kvotienter <- function(tabel_data, mandater){
  
  dhont <- tibble(dhont = 1:mandater)
  
  tabel_data %>% 
    summarise(stemmer = sum(Stemmer), .by = Valgforbund) %>% 
    cross_join(dhont) %>% 
    mutate(kvotienter = stemmer / dhont,
           mandat = rank(-kvotienter) <= mandater)
  
}

regn_valgforbund_mandater <- function(valgforbund_kvotienter){
  
  valgforbund_kvotienter %>% 
    summarise(Antal_mandater = sum(mandat),
              stemmer = max(stemmer), .by = Valgforbund) %>% 
    mutate(Stemmer_pct = stemmer/sum(stemmer)) %>% 
    select(Valgforbund, Stemmer_pct, Antal_mandater)
  
}


regn_valgforbund <- function(tabel_data, mandater){
  
  regn_valgforbund_kvotienter(tabel_data, mandater) %>% 
    regn_valgforbund_mandater()
  
}

regn_parti_kvotienter <- function(tabel_data, mandater){
  
  valgforbund_mandater <- regn_valgforbund_kvotienter(tabel_data, mandater) %>% 
    regn_valgforbund_mandater()
  
  dhont_forbund <- tibble(dhont = 1:max(valgforbund_mandater$Antal_mandater))
  
  tabel_data %>% 
    left_join(valgforbund_mandater, by = join_by(Valgforbund)) %>% 
    cross_join(dhont_forbund) %>% 
    mutate(kvotienter = Stemmer / dhont) %>% 
    mutate(mandat = rank(-kvotienter) <= Antal_mandater, .by = Valgforbund)
  
}

regn_parti_mandater <- function(parti_kvotienter){
  
  parti_kvotienter %>% 
    summarise(Antal_mandater = sum(mandat),
              Stemmer = max(Stemmer), .by = Parti) %>% 
    mutate(Stemmer_pct = Stemmer/sum(Stemmer)) %>% 
    select(Parti, Stemmer_pct, Antal_mandater)
    
  
}

regn_parti <- function(tabel_data, mandater){
  
  regn_parti_kvotienter(tabel_data, mandater) %>% 
    regn_parti_mandater()
  
}

uden_valgforbund_kvotienter <- function(tabel_data, mandater){
  
  dhont <- tibble(dhont = 1:mandater)
  
  tabel_data %>% 
    cross_join(dhont) %>% 
    mutate(kvotienter = Stemmer / dhont,
           mandat = rank(-kvotienter) <= mandater)
  
}

uden_valgforbund <- function(tabel_data, mandater){
  
  uden_valgforbund_kvotienter(tabel_data, mandater) %>% 
    regn_parti_mandater()
  
}

kvotient_tab_format <- function(tabel_data, 
                                sorting = c("Valgforbund", "Parti"),
                                type = "Parti"){
  
  tabel_data %>% 
    arrange(pick(all_of(sorting))) %>%
    select(dhont, all_of(type), kvotienter) %>% 
    pivot_wider(names_from = all_of(type), values_from = kvotienter)
  
}

mandat_pos_format <- function(tabel_data, 
                              sorting = c("Valgforbund", "Parti"),
                              type = "Parti"){
  
  tabel_data %>% 
    arrange(pick(all_of(sorting))) %>% 
    select(dhont, all_of(type), mandat) %>% 
    pivot_wider(names_from = all_of(type), values_from = mandat) %>% 
    select(-dhont) %>% 
    as.matrix() %>% 
    which(arr.ind = TRUE) %>% 
    as_tibble() %>% 
    mutate(row = row-1)
  
}

custom_sample <- function(x, size, fill = "X"){
  
  if(size <= length(x)){
    sample(x, size)
  } else {
    c(sample(x, length(x)), rep(fill, size - length(x)))
  }
  
}

partiforbund_tael <- function(tabel_data){
  
  tabel_data %>% 
    summarise(ant_parti = n_distinct(Parti), .by = Valgforbund) %>% 
    arrange(Valgforbund)

}

valgforbund_partier <- function(tabel_data){
  
  tabel_data %>% 
    summarise(Partier = paste(Parti, collapse = ", "), .by = Valgforbund)
  
}

valgforbund_partier_txt <- function(tabel_data){
  
  q <- tabel_data %>% 
    summarise(Partier = paste(Parti, collapse = ", "), .by = Valgforbund) %>% 
    arrange(Valgforbund)
 
  paste(q$Valgforbund, q$Partier, sep = ": ") %>% 
    paste(collapse = "<br>")
  
}


