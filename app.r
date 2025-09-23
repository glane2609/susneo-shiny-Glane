library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
source("R/mod_data_upload.R")
source("R/mod_dashboard.R")
source("R/BusinessLogic.R")

ui <- fluidPage(
  titlePanel("SUSNEO — Energy & Emissions Dashboard"),
  sidebarLayout(
    sidebarPanel(
      mod_data_upload_ui("u1")
    ),
    mainPanel(
      mod_dashboard_ui("d1")
    )
  )
)

server <- function(input, output, session) {
  data_r <- callModule(mod_data_upload_server, "u1",
                       sample_path = "data/SAMPLE ASSIGNMENT DATA.csv")

  biz <- reactiveVal(NULL)
  observeEvent(data_r(), {
    req(data_r())
    biz(BusinessLogic$new(data_r()))
  })

  callModule(mod_dashboard_server, "d1", data_reactive = data_r)
}

shinyApp(ui, server)
