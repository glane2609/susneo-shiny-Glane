# R/mod_dashboard.R
library(shiny)
library(dplyr)
library(ggplot2)
library(DT)

fmt_si <- function(x, digits = 2) {
  sapply(x, function(v) {
    if (is.na(v)) return(NA_character_)
    v_abs <- abs(v)
    if (v_abs >= 1e6) {
      paste0(round(v / 1e6, digits), " M")
    } else if (v_abs >= 1e3) {
      paste0(round(v / 1e3, max(1, digits - 1)), " K")
    } else {
      as.character(round(v, digits = 0))
    }
  }, USE.NAMES = FALSE)
}

# UI
mod_dashboard_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("filters_ui")),
    fluidRow(
      column(2, div(class="kpi", uiOutput(ns("kpi_total")))),
      column(2, div(class="kpi", uiOutput(ns("kpi_avg")))),
      column(2, div(class="kpi", uiOutput(ns("kpi_em")))),
      column(3, div(class="kpi", uiOutput(ns("kpi_peak")))),
      column(3, div(class="kpi", uiOutput(ns("kpi_facilities"))))
    ),
    hr(),
    tabsetPanel(
      tabPanel("Time Series", plotOutput(ns("plot_time"))),
      tabPanel("Comparison", plotOutput(ns("plot_comp"))),
      tabPanel("Emissions Over Time", plotOutput(ns("plot_em_time"))),
      tabPanel("Energy Type Share", plotOutput(ns("plot_type_share"))),
      tabPanel("Summary Table", DTOutput(ns("summary_table")))
    )
  )
}

# Server
mod_dashboard_server <- function(input, output, session, data_reactive) {
  ns <- session$ns

  output$filters_ui <- renderUI({
    df <- data_reactive()
    req(df)
    tagList(
      dateRangeInput(ns("date_range"), "Date range",
                     start = min(df$date, na.rm = TRUE),
                     end   = max(df$date, na.rm = TRUE)),
      selectInput(ns("sites"), "Facilities",
                  choices  = sort(unique(df$site)),
                  selected = unique(df$site),
                  multiple = TRUE)
    )
  })

  filtered <- reactive({
    df <- data_reactive()
    req(df)
    if (!is.null(input$date_range)) {
      df <- df %>% filter(date >= input$date_range[1],
                          date <= input$date_range[2])
    }
    if (!is.null(input$sites) && length(input$sites) > 0) {
      df <- df %>% filter(site %in% input$sites)
    }
    df %>% arrange(site, date)
  })

  # KPI helper to wrap string with unit and bold
  kpi_tag <- function(label, value_str) {
    tagList(h4(label), p(HTML(paste0("<strong>", value_str, "</strong>"))))
  }

  # KPIs (formatted with K/M)
  output$kpi_total <- renderUI({
    val <- sum(filtered()$value, na.rm = TRUE)
    kpi_tag("Total Consumption", paste0(fmt_si(val), " units"))
  })

  output$kpi_avg <- renderUI({
    agg <- filtered() %>%
      group_by(site, date) %>%
      summarise(daily = sum(value, na.rm = TRUE), .groups = "drop")
    avg_val <- mean(agg$daily, na.rm = TRUE)
    kpi_tag("Avg per day/site", paste0(fmt_si(avg_val), " units"))
  })

  output$kpi_em <- renderUI({
    val <- sum(filtered()[["carbon.emission.in.kgco2e"]], na.rm = TRUE)
    kpi_tag("Total Emissions (kgCO2e)", paste0(fmt_si(val), " kgCO2e"))
  })

  output$kpi_peak <- renderUI({
    agg <- filtered() %>%
      group_by(site, date) %>%
      summarise(daily = sum(value, na.rm = TRUE), .groups = "drop")
    peak <- if (nrow(agg) > 0) max(agg$daily, na.rm = TRUE) else NA_real_
    kpi_tag("Peak Daily Consumption", paste0(fmt_si(peak), " units"))
  })

  output$kpi_facilities <- renderUI({
    n_sites <- length(unique(filtered()$site))
    kpi_tag("Facilities Selected", as.character(n_sites))
  })

  # bold axis theme
  bold_axis_theme <- theme(
    axis.title = element_text(face = "bold", size = 14),
    axis.text  = element_text(face = "bold", size = 12),
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 11)
  )

  # Time series: aggregate then plot; use fmt_si for y axis labels
  output$plot_time <- renderPlot({
    df <- filtered()
    req(nrow(df) > 0)

    daily <- df %>%
      group_by(site, date) %>%
      summarise(total = sum(value, na.rm = TRUE), .groups = "drop") %>%
      arrange(site, date)

    ggplot(daily, aes(x = date, y = total, color = site, group = site)) +
      geom_line() + geom_point(size = 1) +
      labs(title = "Consumption over time", x = "Date", y = "Daily Consumption") +
      scale_y_continuous(labels = function(x) fmt_si(x, digits = 2)) +
      theme_minimal() + bold_axis_theme
  })

  # Comparison chart (site x type) with y labels formatted
  output$plot_comp <- renderPlot({
    df <- filtered()
    req(nrow(df) > 0)
    comp <- df %>%
      group_by(site, type) %>%
      summarise(total = sum(value, na.rm = TRUE), .groups = "drop") %>%
      arrange(site, type)

    ggplot(comp, aes(x = site, y = total, fill = type)) +
      geom_col(position = position_dodge(width = 0.9)) +
      geom_text(aes(label = fmt_si(total)),
                position = position_dodge(width = 0.9),
                vjust = -0.3, size = 3.5, fontface = "bold") +
      labs(title = "Comparison by site & type", x = "Site", y = "Total Consumption") +
      scale_y_continuous(labels = function(x) fmt_si(x, digits = 2)) +
      theme_minimal() + bold_axis_theme
  })


  # Emissions over time
  output$plot_em_time <- renderPlot({
    df <- filtered()
    req(nrow(df) > 0)
    em <- df %>%
      group_by(date) %>%
      summarise(emissions = sum(`carbon.emission.in.kgco2e`, na.rm = TRUE), .groups = "drop") %>%
      arrange(date)

    ggplot(em, aes(x = date, y = emissions)) +
      geom_line(color = "maroon") + geom_point(color = "maroon") +
      labs(title = "Emissions over time", x = "Date", y = "Emissions (kgCO2e)") +
      scale_y_continuous(labels = function(x) fmt_si(x, digits = 2)) +
      theme_minimal() + bold_axis_theme
  })

  output$plot_type_share <- renderPlot({
    df <- filtered()
    req(nrow(df) > 0)
    share <- df %>%
      group_by(type) %>%
      summarise(total = sum(value, na.rm = TRUE), .groups = "drop") %>%
      arrange(type)

    ggplot(share, aes(x = "", y = total, fill = type)) +
      geom_col(width = 1, color = "white") +
      coord_polar(theta = "y") +
      geom_text(aes(label = paste0(type, ": ", fmt_si(total))),
                position = position_stack(vjust = 0.5),
                size = 4, fontface = "bold", color = "white") +
      labs(title = "Energy Type Share", x = NULL, y = NULL) +
      theme_void() +
      theme(legend.position = "none")
  })

  output$summary_table <- renderDT({
    df <- filtered()
    req(nrow(df) > 0)

    summary <- df %>%
      group_by(site, type) %>%
      summarise(total     = sum(value, na.rm = TRUE),
                avg       = mean(value, na.rm = TRUE),
                emissions = sum(.data[["carbon.emission.in.kgco2e"]], na.rm = TRUE),
                .groups   = "drop") %>%
      arrange(site, type)

    summary_display <- summary %>%
      mutate(
        total_fmt = fmt_si(total, digits = 2),
        avg_fmt = fmt_si(avg, digits = 2),
        emissions_fmt = paste0(fmt_si(emissions, digits = 2), " kgCO2e")
      ) %>%
      select(site, type, total_fmt, avg_fmt, emissions_fmt)

    datatable(summary_display,
              colnames = c("Site", "Type", "Total (fmt)", "Avg (fmt)", "Emissions (fmt)"),
              options = list(pageLength = 10, autoWidth = TRUE),
              rownames = FALSE)
  })
}
