source("startup.R")

## Description & Data Details
# All of the weather station information is available at the `weather_station_address`
# address below. This is a fixed width file with the width of each column shown below.
# ------------------------------
#   Variable   Columns   Type
# ------------------------------
# ID           1-11    Character
# LATITUDE     13-20   Real
# LONGITUDE    22-30   Real
# ELEVATION    32-37   Real
# STATE        39-40   Character
# NAME         42-71   Character
# GSNFLAG      73-75   Character
# HCNFLAG      77-79   Character
# WMOID        81-85   Character
# METHOD*	     87-99   Character
# ------------------------------
#
# The zipcode file does not have the column widths in the readme.txt file but the id and zipcode
# widths are known (1-11 and 12-16), the rest is made up of the name column so 50 is used to ensure
# even the long names are captured.
#
# For more details including field descriptions, etc please see Section III. File Formats Part E
# https://www1.ncdc.noaa.gov/pub/data/normals/1981-2010/readme.txt


## Data Loading & Processing
# load in weather station data
weather_station_address <- "https://www1.ncdc.noaa.gov/pub/data/normals/1981-2010/station-inventories/allstations.txt"
raw_station_tbl <- readr::read_fwf(weather_station_address,
                                   readr::fwf_widths(c(12, 8, 10, 7, 3, 31, 4, 4, 6, 14)),
                                   guess_max = 100000,
                                   col_types = cols())
# add in column names
colnames(raw_station_tbl) <- c("id", "latitude", "longitude", "elevation", "state", "name", 
                               "gsnflag", "hcnflag", "wmoid", "method")
# change flags to proper booleans
raw_station_tbl <- raw_station_tbl %>%
  dplyr::select(-method) %>%
  dplyr::mutate(gsnflag = dplyr::case_when(is.na(gsnflag) ~ F,
                                           T ~ T),
                hcnflag = dplyr::case_when(is.na(hcnflag) ~ F,
                                           T ~ T))

# load in station zipcode data
weather_station_zipcode_address <- "https://www1.ncdc.noaa.gov/pub/data/normals/1981-2010/station-inventories/zipcodes-normals-stations.txt"
raw_zipcode_tbl <- readr::read_fwf(weather_station_zipcode_address,
                                   readr::fwf_widths(c(11, 6, 50)),
                                   col_types = cols())
colnames(raw_zipcode_tbl) <- c("id", "zipcode", "post_office")

# merge tables together
full_station_tbl <- dplyr::left_join(raw_station_tbl, raw_zipcode_tbl, by = "id")

con <- DBI::dbConnect(RPostgres::Postgres(),
                      dbname = dw$database,
                      host = dw$server,
                      port = dw$port,
                      user = dw$uid,
                      password = dw$pwd)

# find which station support different frequencies of data
hourly_tbl <- dplyr::tbl(con, "hourly_weather_data") %>%
  dplyr::select(id, metric, value) %>%
  dplyr::filter(metric == "temperature mean", !is.na(value)) %>% 
  dplyr::select(id) %>% 
  dplyr::distinct() %>%
  dplyr::collect()
hourly_tbl[["hourly"]] <- T

daily_tbl <- dplyr::tbl(con, "daily_weather_data") %>%
  dplyr::select(id, metric, value) %>%
  dplyr::filter(metric == "Long-term averages of daily average temperature", !is.na(value)) %>% 
  dplyr::select(id) %>%
  dplyr::distinct() %>%
  dplyr::collect()
daily_tbl[["daily"]] <- T

full_station_tbl <- full_station_tbl %>% dplyr::left_join(hourly_tbl) %>% dplyr::left_join(daily_tbl)
full_station_tbl$hourly[is.na(full_station_tbl$hourly)] <- F
full_station_tbl$daily[is.na(full_station_tbl$daily)] <- F
# remove if station doesnt have hourly or daily
full_station_tbl <- full_station_tbl %>% dplyr::filter((hourly | daily))

## Data Adding to PostgreSQL Database
add_to_db(full_station_tbl, table_name = "weather_station_map",
          index_fields = NULL,
          dw,
          overwrite = F)







