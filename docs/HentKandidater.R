library(tidyverse)
library(xml2)

# Strukturer med info fra 10 Storkredse
A <- readRDS("data/Storkredse.rds")


# Struktur med info om personlige stemmer
Kandidater <- NULL
VariabelNavne <- c("StorkredsID",
                   "StorkredsNavn",
                   "PartiBogstav",
                   "PartiNavn",
                   "Kandidat",
                   "StemmerPersonlige",
                   "StemmerTildelteParti",
                   "StemmerIAlt")

# Niveau 1: <Personer> i Storkreds i
for (i in 1:length(A$Storkreds)){
  # i <- 1
  StorkredsNavn <- A$Storkreds[i]
  StorkredsID <- A$storkreds_id[i]
  a1 <- as_list(read_xml(pull(A[i, "filnavn"])))$Data$Personer
  
  # Niveau 2: <Parti> j i <Personer> i <Storkeds> i
  for (j in 1:length (a1)){
    # j <- 1
    a2 <- a1[j]$Parti
    
    # Niveau 3A: <Parti> er mere end en tom liste, dvs <Parti> med en eller flere <Person>)
    if (length(a2) > 0){
      PartiBogstav <- attributes(a2)$Bogstav
      PartiNavn <- attributes(a2)$navn
      
      # Niveau 4: <Person> k i <Parti> j i <Personer> i <Storkreds> i
      for (k in 1:length (a2)){
        # k <- 1
        a3 <- a2[k]$Person
        
        Kandidat             <- attributes(a3)$Navn
        StemmerPersonlige    <- attributes(a3)$PersonligeStemmer
        StemmerTildelteParti <- attributes(a3)$TildeltePartiStemmer
        StemmerIAlt          <- attributes(a3)$StemmerIAlt
        
        person <- c(StorkredsID,
                    StorkredsNavn,
                    PartiBogstav,
                    PartiNavn,
                    Kandidat,
                    StemmerPersonlige,
                    StemmerTildelteParti,
                    StemmerIAlt)
        
        names(person) <- VariabelNavne
        
        Kandidater <- bind_rows(Kandidater, person)
      }
    }
    
    # Niveau 3b: <Parti> er en tom liste, dvs. udenfor parti 
    else {
      PartiBogstav <- NA
      PartiNavn <- "Uden for partier"
      
      Kandidat <- attributes(a2)$navn
      StemmerPersonlige    <- attributes(a2)$PersonligeStemmer
      StemmerTildelteParti <- attributes(a2)$TildeltePartiStemmer
      StemmerIAlt          <- attributes(a2)$StemmerIAlt
      
      person <- c(StorkredsID,
                  StorkredsNavn,
                  PartiBogstav,
                  PartiNavn,
                  Kandidat,
                  StemmerPersonlige,
                  StemmerTildelteParti,
                  StemmerIAlt)
      
      names(person) <- VariabelNavne
      
      Kandidater <- bind_rows(Kandidater, person)
    } 
  }
}

Kandidater <- mutate(Kandidater, across(starts_with("Stemmer"), ~as.integer(.x)))

saveRDS(Kandidater, "data/Kandidater.rds")
