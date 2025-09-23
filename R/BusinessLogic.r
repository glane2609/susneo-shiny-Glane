# R/BusinessLogic.R
library(R6)
library(dplyr)

BusinessLogic <- R6Class(
  "BusinessLogic",
  public = list(
    data = NULL,

    initialize = function(data) {
      stopifnot(is.data.frame(data))
      self$data <- data
    },

    filter_data = function(date_range = NULL, sites = NULL) {
      df <- self$data
      if (!is.null(date_range)) {
        df <- df %>% filter(date >= date_range[1], date <= date_range[2])
      }
      if (!is.null(sites)) {
        df <- df %>% filter(site %in% sites)
      }
      df %>% arrange(date, site)
    },
    # KPIs ----
    total_consumption = function(df = NULL) {
      if (is.null(df)) df <- self$data
      sum(df$value, na.rm = TRUE)
    },

    average_consumption = function(df = NULL) {
      if (is.null(df)) df <- self$data
      agg <- df %>% group_by(site, date) %>%
        summarise(daily = sum(value, na.rm = TRUE), .groups = "drop")
      mean(agg$daily, na.rm = TRUE)
    },

    total_emissions = function(df = NULL) {
      if (is.null(df)) df <- self$data
      sum(df[["carbon.emission.in.kgco2e"]], na.rm = TRUE)
    },

    peak_daily_consumption = function(df = NULL) {
      if (is.null(df)) df <- self$data
      agg <- df %>% group_by(site, date) %>%
        summarise(daily = sum(value, na.rm = TRUE), .groups = "drop")
      if (nrow(agg) == 0) return(NA_real_)
      max(agg$daily, na.rm = TRUE)
    },

    facility_count = function(df = NULL) {
      if (is.null(df)) df <- self$data
      length(unique(df$site))
    },

    # Summaries ----
    summary_by_site_type = function(df = NULL) {
      if (is.null(df)) df <- self$data
      df %>%
        group_by(site, type) %>%
        summarise(total     = sum(value, na.rm = TRUE),
                  avg       = mean(value, na.rm = TRUE),
                  emissions = sum(.data[["carbon.emission.in.kgco2e"]], na.rm = TRUE),
                  .groups   = "drop")
    },

    emissions_over_time = function(df = NULL) {
      if (is.null(df)) df <- self$data
      df %>%
        group_by(date) %>%
        summarise(emissions = sum(.data[["carbon.emission.in.kgco2e"]], na.rm = TRUE), .groups = "drop")
    },

    energy_type_share = function(df = NULL) {
      if (is.null(df)) df <- self$data
      df %>%
        group_by(type) %>%
        summarise(total = sum(value, na.rm = TRUE), .groups = "drop")
    }
  )
)
