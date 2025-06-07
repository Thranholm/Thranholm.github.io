
library(shiny)
library(magrittr)
library(dplyr)
library(tidyr)
library(rhandsontable)
library(jsonlite)

library(rhandsontable)

# source("komvalg_shiny/valg_shiny_regn.R")
source("valg_shiny_regn.R")

ui <- fluidPage(
  titlePanel("Regn Kommunalvalg/Europaparlamentsvalg"),
  
  sidebarPanel(
    titlePanel("Input til beregninger"),
    numericInput("mandater", label = "Antal mandater (medlemmer i byråd/kommunalbestyrelse)", value = 27),
    numericInput("num_rows", label = "Antal partier", value = 5, min = 1),
    # conditionalPanel(
    #   condition = "input.num_rows == null || input.num_rows < 1",
    #   tags$div("Please enter a valid number of rows", 
    #            style = "color: red; font-size: 12px;")
    # ),
    rHandsontableOutput("hot_table")
  ),
  
  mainPanel(
    titlePanel("Resultater"),
    tabsetPanel(
      tabPanel("Partiresultat",
               br(),
               fluidRow(
                 column(9, rHandsontableOutput("parti_res")),
                 column(3, HTML("<b>Valgforbund:</b>"), br(), uiOutput("valgfor"))
                 ),
               br(),
               rHandsontableOutput("parti_res_kvotient")),
      
      tabPanel("Valgforbundsresultat",
               br(),
               fluidRow(
                 column(9, rHandsontableOutput("valgforbund_res")),
                 column(3, HTML("<b>Valgforbund:</b>"), br(), uiOutput("valgfor2"))
                 ),
               br(),
               rHandsontableOutput("valgforbund_res_kvotient")),
      
      tabPanel("Uden valgforbund",
               br(),
               rHandsontableOutput("uden_valgforbund"),
               br(),
               rHandsontableOutput("uden_valgforbund_kvotient")),
      
      tabPanel("Med vs. uden valgforbund",
               br(),
               rHandsontableOutput("versus"),
               br(),
               HTML("<b>Valgforbund:</b>"), br(), uiOutput("valgfor3"))
  )
  )   
)

server <- function(input, output, session) {
  values <- reactiveValues()
  
  # Initialize
  values$df <- data.frame(
    Parti = character(0),
    Valgforbund = character(0),
    Stemmer = character(0),
    stringsAsFactors = FALSE
  )
  
  partier <- c("A", "B", "C", "D", "F", "H", "I", "K", "M", "O", "Q", "V", "Æ", "Ø", "Å")
  
  # Automatically react to numeric input changes
  observe({
    target_rows <- input$num_rows
    
    if (is.null(target_rows) || is.na(target_rows) || target_rows < 1) {
      return()
    }
    
    current_rows <- nrow(values$df)
    
    if (target_rows != current_rows) {
      if (target_rows > current_rows) {
        # Add rows
        new_rows <- target_rows - current_rows
        new_data <- data.frame(
          Parti = sort(custom_sample(setdiff(partier, values$df$Parti), new_rows)),
          Valgforbund = sample(c("V1", "V2", "V3", "V4"), new_rows, replace = TRUE),
          Stemmer = sample(0:500000, new_rows),
          stringsAsFactors = FALSE
        ) 
        values$df <- rbind(values$df, new_data) %>% 
          arrange(Parti, .locale = "da")
        
      } else {
        # Remove rows (keep existing data for remaining rows)
        values$df <- values$df[1:target_rows, , drop = FALSE]
      }
    }
  })
  
  # Render the table
  output$hot_table <- renderRHandsontable({
    if (is.null(values$df) || nrow(values$df) == 0) {
      
      return(NULL)
      
    } else {
      rhandsontable(values$df, width = "100%", height = 400, rowHeaders = NULL) %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all") %>%
        hot_cols(columnSorting = TRUE)
    }

    }
  )
  
  # Capture user edits
  observe({
    if (!is.null(input$hot_table)) {
      values$df <- hot_to_r(input$hot_table)
    }
  })
  
  ## Funktion til highlight, mellemrum ved tusindtal, og "," som decimal
  myrenderer <- "function(instance, td, row, col, prop, value, cellProperties) {
       Handsontable.renderers.TextRenderer.apply(this, arguments);
       
       // Right align all numeric cells
       td.style.textAlign = 'right';
       
       if (td.innerHTML) {
         var text = td.innerHTML;
         
         // First replace decimal point with comma
         text = text.replace('.', ',');
         
         // Add space as thousands separator
         // This regex adds space every 3 digits from the right, before the decimal part
         text = text.replace(/\\B(?=(\\d{3})+(?!\\d))/g, ' ');
         
         td.innerHTML = text;
       }
       
       if (instance.params) {
         hcols = instance.params.col_highlight
         hcols = hcols instanceof Array ? hcols : [hcols]
         hrows = instance.params.row_highlight
         hrows = hrows instanceof Array ? hrows : [hrows]
         
         for (i = 0; i < hcols.length; i++) { 
           if (hcols[i] == col && hrows[i] == row) {
             td.style.background = '#00FF00';
           }
         }
       }
     }"
  
  # Laver partiresultaterne
  parti_resultater <- reactive({
    calc_results <- regn_parti(values$df, input$mandater)
  })
  
  output$parti_res <- renderRHandsontable({
    results <- parti_resultater()
    rhandsontable(results, readOnly = TRUE, rowHeaders = NULL,
                  colHeaders = c("Parti", "Stemmer (%)", "Antal mandater")) %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE,
                stretchH = "all") %>% 
      hot_col(2, format = "0.00 %")
  })
  
  
  # valgforbundsoversigt
  valgforbundspartier <- reactive({
    calc_results <- valgforbund_partier(values$df) %>% 
      arrange(Valgforbund)
  })
  
  valgfor_react <- reactive({
    calc_results <- valgforbund_partier_txt(values$df)
  })
  
  output$valgfor <- renderUI({
    text <- valgfor_react()
    HTML(text)
  })
  
  output$valgfor2 <- renderUI({
    text <- valgfor_react()
    HTML(text)
  })
  
  output$valgfor3 <- renderUI({
    text <- valgfor_react()
    HTML(text)
  })
  
  
  # kvotienter
  parti_kvotienter <- reactive({
    calc_results <- regn_parti_kvotienter(values$df, input$mandater) %>% 
      kvotient_tab_format(sorting = "Parti") %>%
      rename(Divisor = dhont)
  })
  
  parti_kvotienter_highlight <- reactive({
    calc_results <- regn_parti_kvotienter(values$df, input$mandater) %>% 
      mandat_pos_format(sorting = "Parti")
  })
  
  ant_parti_pr_forbund <- reactive({
    calc_results <- partiforbund_tael(values$df)
  })
  
  output$parti_res_kvotient <- renderRHandsontable({
    results <- parti_kvotienter()
    
    highlight <- parti_kvotienter_highlight()
    
    ant_partier_json <- ant_parti_pr_forbund() %>% 
      select(label = Valgforbund, colspan = ant_parti) %>% 
      as.list()
    
    rhandsontable(results,
                  col_highlight = highlight$col,
                  row_highlight = highlight$row,
                  rowHeaders = NULL,
                  digits = 2) %>% 
      hot_cols(renderer = myrenderer, manualColumnResize = TRUE) %>%
      hot_table(nestedHeaders = list(list(ant_partier_json), colnames(results))) %>% 
      hot_col("Divisor", halign = "htLeft") %>% 
      hot_rows(fixedRowsTop = 1)
    
    })
  
  ####### Laver valgforbund #########
  valgforbund_resultater <- reactive({
    calc_results <- regn_valgforbund(values$df, input$mandater)
  })
  
  output$valgforbund_res <- renderRHandsontable({
    results <- valgforbund_resultater()
    rhandsontable(results, readOnly = TRUE, rowHeaders = NULL,
                  colHeaders = c("Valgforbund", "Stemmer (%)", "Antal mandater")) %>% 
      hot_table(highlightCol = TRUE, highlightRow = TRUE,
                stretchH = "all") %>% 
      hot_col(2, format = "0.00 %")
  })
  
  
  # kvotienter
  valgforbund_kvotienter <- reactive({
    calc_results <- regn_valgforbund_kvotienter(values$df, input$mandater) %>% 
      kvotient_tab_format(sorting = "Valgforbund", type = "Valgforbund") %>% 
      rename(Divisor = dhont)
  })
  
  valgforbund_kvotienter_highlight <- reactive({
    calc_results <- regn_valgforbund_kvotienter(values$df, input$mandater) %>% 
      mandat_pos_format(sorting = "Valgforbund", type = "Valgforbund")
  })
  
  
  output$valgforbund_res_kvotient <- renderRHandsontable({
    results <- valgforbund_kvotienter()
    
    highlight <- valgforbund_kvotienter_highlight()
    
    rhandsontable(results, readOnly = TRUE,
                  col_highlight = highlight$col,
                  row_highlight = highlight$row,
                  rowHeaders = NULL,
                  digits = 2) %>% 
      hot_cols(renderer = myrenderer) %>%
      hot_col("Divisor", halign = "htLeft")
    
  })
  
  
  ##### Laver uden at der må indgås valgforbund ########
  uden_forbund <- reactive({
    calc_results <- uden_valgforbund(values$df, input$mandater)
  })
  
  output$uden_valgforbund <- renderRHandsontable({
    results <- uden_forbund()
    rhandsontable(results, readOnly = TRUE, rowHeaders = NULL,
                  colHeaders = c("Parti", "Stemmer (%)", "Antal mandater")) %>% 
      hot_table(highlightCol = TRUE, highlightRow = TRUE,
                stretchH = "all") %>% 
      hot_col(2, format = "0.00 %")
  })

  
  # kvotienter
  uden_valgforbund_kvotienter_res <- reactive({
    calc_results <- uden_valgforbund_kvotienter(values$df, input$mandater) %>% 
      kvotient_tab_format(sorting = "Parti", type = "Parti") %>%
      rename(Divisor = dhont)
  })
  
  uden_valgforbund_kvotienter_highlight <- reactive({
    calc_results <- uden_valgforbund_kvotienter(values$df, input$mandater) %>% 
      mandat_pos_format(sorting = "Parti", type = "Parti")
  })
  
  
  output$uden_valgforbund_kvotient <- renderRHandsontable({
    results <- uden_valgforbund_kvotienter_res()
    
    highlight <- uden_valgforbund_kvotienter_highlight()
    
    rhandsontable(results, readOnly = TRUE,
                  col_highlight = highlight$col,
                  row_highlight = highlight$row,
                  rowHeaders = NULL,
                  digits = 2) %>% 
      hot_cols(renderer = myrenderer) %>%
      hot_col("Divisor", halign = "htLeft")
    
  })
  
  ### Sammenligning, med og uden valgforbund
  
  versus_regn <- reactive({
    calc_results <- regn_parti(values$df, input$mandater) %>% 
      rename(med_forbund = Antal_mandater) %>% 
      left_join(uden_valgforbund(values$df, input$mandater) %>% 
                  rename(uden_forbund = Antal_mandater) %>% 
                  select(-Stemmer_pct), 
                by = "Parti") %>% 
      select(Parti, Stemmer_pct, med_forbund, uden_forbund) 
  })
  
  output$versus <- renderRHandsontable({
    results <- versus_regn()
    
    rhandsontable(results, rowHeaders = NULL,
                  colHeaders = c("Parti", 
                                 "Stemmer (%)",
                                 "Antal mandater <br> med valgforbund", 
                                 "Antal mandater <br> uden valgforbund")) %>% 
      hot_col(2, format = "0.00 %") %>% 
      hot_table(stretchH = "all")
    
  })
  
}

shinyApp(ui = ui, server = server)