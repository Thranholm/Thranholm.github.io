library(shiny)
library(DT)
library(httpuv)
library(shinylive)

ui <- fluidPage(numericInput(inputId = "tot_mandater", label = "Total antal mandater", value = 175),
                numericInput(inputId = "tot_kredsmandater", label = "Antal kredsmandater", value = 135),
                # tags$hr(),
                DT::dataTableOutput(outputId = "table"))

server <- function(input, output) {
  
  filtered_data <- reactive({
    tot_mandater <- input$tot_mandater
    tot_kredsmandater <- input$tot_kredsmandater
    source("ftvalg/Stedlig mandatfordeling.R", local = TRUE)
    
    DT::datatable(stedlig_mandatfordeling %>% 
                        select(storkreds_navn, kreds_mandat_storkreds) %>% 
                        bind_rows(stedlig_mandatfordeling %>% 
                                    select(landsdel_navn, tillaegsmandater_landsdel) %>% 
                                    distinct()) %>% 
                        select(landsdel_navn, storkreds_navn, kreds_mandat_storkreds, tillaegsmandater_landsdel) %>% 
                        arrange(match(landsdel_navn, "Hovedstaden"), 
                                match(storkreds_navn, c("Koebenhavn", "Koebenhavns Omegn", "Nordsjaelland", "Bornholm")),
                                match(landsdel_navn, "Sjaelland-Syddanmark"),
                                match(storkreds_navn, c("Sjaelland", "Fyn", "Sydjylland")),
                                match(landsdel_navn, "Midtjylland-Nordjylland")),
                  options = list(paging = FALSE,
                                 ordering = FALSE,
                                 #scrollY = FALSE,
                                 #scrollX = FALSE,
                                 dom = "t"),
                  rownames = FALSE,
                  colnames = c("Landsdel", "Storkreds", "Kredsmandater", "Tillaegsmandater"),
                  filter = "none")
  })
  
  output$table <- renderDataTable({
    filtered_data()
  })
}

# source("ftvalg/Stedlig mandatfordeling.R")

shinyApp(ui, server)


