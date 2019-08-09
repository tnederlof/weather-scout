# call general functions
source("general_functions.R")

# load libraries
library(leaflet)
library(sp)
library(ggmap)
library(geosphere)
library(shinycssloaders)
library(tidyverse)
library(openxlsx)
library(readxl)
library(stringr)
library(lubridate)
library(DBI)
library(scales)
library(RCurl)
library(RPostgres)
library(plotly)
library(promises)
library(future)
library(pool)
library(shiny)
library(shinydashboard)
library(config)
library(googleway)
library(miniUI)

# call database credentials
dw <- config::get("maindb")
# set google api key
register_google(key = dw$google_api)

plan(multiprocess)
options(spinner.color = "#1F4879")
options(scipen = 9999)
options(expressions = 10000)
