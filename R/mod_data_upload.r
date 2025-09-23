# R/mod_data_upload.R
library(shiny)
library(dplyr)
library(lubridate)

validate_and_clean <- function(df) {
  required <- c("id","site","date","type","value","carbon.emission.in.kgco2e")
  missing <- setdiff(required, names(df))
  df$date <- dmy(df$date)
  list(ok=TRUE, data=df)
}

# UI
mod_data_upload_ui <- function(id) {
  ns <- NS(id)
  tagList(
    radioButtons(ns("source"), "Data source",
                 choices = c("Use sample data" = "sample", "Upload CSV" = "upload"),
                 selected = "sample"),
    conditionalPanel(condition = sprintf("input['%s'] == 'upload'", ns("source")),
                     fileInput(ns("file"), "Upload CSV", accept = c(".csv"))),
    actionButton(ns("load"), "Load Data", class = "btn-primary"),
    tags$hr(),
    uiOutput(ns("load_message"))
  )
}

# Server
mod_data_upload_server <- function(input, output, session,
                                   sample_path = "data/SAMPLE ASSIGNMENT DATA.csv") {
  ns <- session$ns
  data_r <- reactiveVal(NULL)
  # validate_and_clean <- function(df) {
  #   required <- c("id","site","date","type","value","carbon.emission.in.kgco2e")
  #   missing <- setdiff(required, names(df))
  #   df$date <- dmy(df$date)
  #   list(ok=TRUE, data=df)
  # }
  
  observeEvent(input$load, {
    if (input$source == "sample") {
      if (!file.exists(sample_path)) {
        output$load_message <- renderUI({
          tags$div(class="text-danger", paste("Sample file not found at", sample_path))
        })
        return()
      }
      df <- read.csv(sample_path)
    } else {
      req(input$file)
      df <- read.csv(input$file$datapath)
    }
    validated <- validate_and_clean(df)
    if (!validated$ok) {
      output$load_message <- renderUI({
        tags$div(class="text-danger", validated$message)
      })
      data_r(NULL)
    } else {
      output$load_message <- renderUI({
        tags$div(class="text-success", "Data loaded successfully.")
      })
      data_r(validated$data)
    }
  })
  
  reactive(data_r())
}
