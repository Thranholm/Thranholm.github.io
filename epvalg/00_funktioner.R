
## Funktion til at hente partistemmer
hent_parti_stemmer <- function(xml_data, xpath){
  # Henter navne
  xml_find_all(xml_data, xpath)  %>% 
    xml_text() %>% 
    as_tibble_col(column_name = unique(xml_name(xml_find_all(xml_data, xpath)))) %>% 
    # Henter data fra attributer
    cbind(xml_find_all(xml_data, xpath) %>% 
            xml_attrs() %>% 
            tibble() %>% 
            unnest_wider(col = everything())) %>% 
    ## Henter data fra XML
    mutate(test = lapply(filnavn, function(x) {
      read_xml(x) %>% 
        xml_find_all("//Stemmer//Parti") %>% 
        xml_attrs() %>% 
        tibble() %>% 
        unnest_wider(col = everything())
    })) %>% 
    unnest(cols = test) %>% 
    rename_with(str_to_lower)
}




