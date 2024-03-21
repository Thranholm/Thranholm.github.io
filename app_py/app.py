from shiny import ui, render, App, run_app
import numpy as np
import pandas as pd
import rpy2
import rpy2.robjects as robjects
from os import getcwd

app_ui = ui.page_fluid(ui.input_numeric("tot_mandater", "Total antal mandater", 175),
                       ui.input_numeric("tot_kredsmandater", "Antal kredsmandater", 135),
                       ui.output_table("sted"))

r = robjects.r

def server(input, output, session):
  
  def koer_data():
    tot_mandater = input.tot_mandater
    tot_kredsmandater = input.tot_kredsmandater
    r('source("ftvalg/Stedlig mandatfordeling.R')
    @render.table
    def sted():
      r = robjects.r
      sted_py = pd.read_json(r.stedlig_mandatfordeling_json[0])
      return render.table(sted_py)
  

app = App(app_ui, server)

run_app(App(app_ui, server))

r = robjects.r
r('source("ftvalg/Stedlig mandatfordeling.R")')

sted_py = pd.read_json(r.stedlig_mandatfordeling_json[0])



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
