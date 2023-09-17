

rm(list = ls())
# install.packages("pacman")
library(pacman)

p_load(tidyverse, readr, xml2, XML, janitor, flatxml, reshape2, devtools)

## Import af data
fv_22_xml <- "https://www.dst.dk/valg/Valg1968094/xml/fintal.xml"

ind2 <- read_xml(fv_22_xml)

source("00_funktioner.R", encoding = "UTF-8")

## stemmer på de forskellige områder

land <- hent_parti_stemmer(ind2, "Land")
landsdel <- hent_parti_stemmer(ind2, "//Landsdel")
storkreds <- hent_parti_stemmer(ind2, "//Storkreds")
opstillingskreds <- hent_parti_stemmer(ind2, "//Opstillingskreds")
afstemningsomraade <- hent_parti_stemmer(ind2, "//Afstemningsomraade")



## Forsøger at hente de personlige stemmer

personlige_stemmer <- unique(storkreds$filnavn)[1] %>% 
  read_xml() %>% 
  xml_find_all("//Personer/Parti/Person") %>% 
  xml_attrs() %>% 
  #xml_children() %>% 
  #as.list.data.frame() %>% 
  as_tibble_col() %>% 
  unnest_wider(value)


t <- tibble(storkreds %>% select(storkreds:filnavn) %>% distinct()) %>% 
  rowwise() %>% 
  mutate(personlige_stemmer = list(map(filnavn, function(x) read_xml(x) %>% 
                                         xml_find_all("//Personer/Parti/Person") %>% 
                                         xml_attrs() %>% 
                                         as_tibble_col() %>% 
                                         unnest_wider(value)))) %>% 
  unnest(personlige_stemmer)




t2 <- unique(afstemningsomraade$filnavn)[1] %>% 
  read_xml() %>% 
  xml_find_all("//Personer/Parti") %>% 
  as_list() %>% 
  as_tibble_col()
  



xml_structure(t2)
xmlToDataFrame(nodes = getNodeSet(t2, "//Personer/Parti"))


attributes(t2$value$Parti$Person)






t <- afstemningsomraade$filnavn[1] %>% 
  read_xml()


  attributes()
  xmlToDataFrame()
  
  
  xml_contents()
  
  xml_attrs()
  
  xml_text()



xml_find_all(t$test[[1]], "//Stemmer//Parti") %>% 
  xml_attrs() %>% 
  tibble() %>% 
  unnest_wider(col = everything())

  

t2 <- data.frame(Reduce(rbind, t))


ind <- read_xml(fv_22_xml) %>% 
  xmlTreeParse()

ind2 <- read_xml(fv_22_xml)

ind

read_xml(fv_22_xml) %>% 
  xml_structure()

xml_text(ind2)
t <- xml_find_all(ind2, "//Afstemningsomraade")

storkreds <- xml_find_all(ind2, "//Storkredse")
xml_text(xml_find_all(ind2, "//Storkredse"))
xml_attrs(xml_find_all(ind2, "//Storkredse"))


xml_top <- xmlRoot(ind)

print(xml_top)[1:2]

ind2 <- xmlSApply(xml_top, function(x) xmlSApply(x, xmlValue))

df <- data.frame(t(ind2), row.names = NULL) %>% 
  as_tibble()



