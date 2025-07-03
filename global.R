library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(fresh)
library(DT)
library(jsonlite)
library(shinyWidgets)
library(shinyBS)
library(googlesheets4)
library(dplyr)
library(purrr)
library(posterior)
library(ggplot2)
library(tidyr)

source("./functions.R")

gs4_deauth()
sheet_id <- "1v9F6y_iAnZFPfI4788rZT_KBS72p4Q5OOWy22r5-BCU"

read_with_fallback <- function(sheet_id, sheet_name, local_path) {
  tryCatch({
    message(paste("Attempting to read", sheet_name, "from Google Sheets..."))
    dat <- read_sheet(sheet_id, sheet = sheet_name)
    saveRDS(dat, local_path)  # Optionally update local cache
    dat
  }, error = function(e) {
    message(paste("Failed to read", sheet_name, "from Google Sheets. Using local file:", local_path))
    readRDS(local_path)
  })
}

# Read all 3 sheets (from Google or Local)
general_values   <- read_with_fallback(sheet_id, "General",   "data/general_values.rds")
diarrhea_values  <- read_with_fallback(sheet_id, "Diarrhea",  "data/diarrhea_values.rds")
pna_values       <- read_with_fallback(sheet_id, "Pneumonia", "data/pna_values.rds")

# Assign Data -------------------------------------------------------------
## General ----------------------------------------------------------------
general <- setNames(general_values$value, general_values$short_name)

## Diarrhea ---------------------------------------------------------------
diarrhea_values <- diarrhea_values %>% filter(!is.na(short_name))
short_diar <- setNames(diarrhea_values$short_value, diarrhea_values$short_name)
long_diar <- setNames(diarrhea_values$long_value, diarrhea_values$short_name)

## Pneumonia --------------------------------------------------------------
pna_values <- pna_values %>% filter(!is.na(short_name))
short_pna <- setNames(pna_values$short_value, pna_values$short_name)
long_pna <- setNames(pna_values$long_value, pna_values$short_name)


# IgG Model ---------------------------------------------------------------
#igg_model <- readRDS("data/fit_igg_model.rds")
posterior_draws <- readRDS("data/igg_model_draws.rds")

