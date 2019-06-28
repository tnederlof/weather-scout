source("startup.R")

## Description & Data Details
# The hourly weather data is contained on a series of files, each one containing a different weather value.
# This is a fixed width file with the width of each column shown below.
# ------------------------------
# Variable  Columns  Type
# ----------------------------
# STNID       1- 11  Character
# MONTH      13- 14  Integer
# DAY        16- 17  Integer
# VALUE1     19- 23  Integer
# FLAG1      24- 24  Character
# - - - - - - - - - - - - - - 
# VALUE24   180-184  Integer
# FLAG24    185-185  Character
# ----------------------------
#
# For more details including field descriptions, etc please see Section III. File Formats Part D
# https://www1.ncdc.noaa.gov/pub/data/normals/1981-2010/readme.txt


## Define Helper Functions
get_and_parse_hourly_data <- function(filename, metric) {
  # load in hourly data (suppress parsing warnings)
  raw_hourly_tbl <- readr::read_fwf(paste0("data_parsing/hourly/", filename), readr::fwf_widths(c(12, 3, 3, rep(c(5, 2), 24))),
                                    col_types = cols())
  
  # add in column names
  hour_col_names <- paste(rep(c("hour_value", "hour_flag"), 24),
                          rep(seq(1, 24), each = 2), sep = "_")
  colnames(raw_hourly_tbl) <- c("id", "month", "day", hour_col_names)
  
  # transform data into long (tidy) format (values and flags seperate then joined together)
  hourly_value_tbl <- raw_hourly_tbl %>%
    tidyr::gather(starts_with("hour_value"), key = "hour", value = "value") %>%
    dplyr::select(-c(starts_with("hour_flag"))) %>%
    dplyr::mutate(hour = purrr::map_chr(stringr::str_split(hour, "_"), 3))
  hourly_flag_tbl <- raw_hourly_tbl %>%
    tidyr::gather(starts_with("hour_flag"), key = "hour", value = "flag") %>%
    dplyr::select(-c(starts_with("hour_value"))) %>%
    dplyr::mutate(hour = purrr::map_chr(stringr::str_split(hour, "_"), 3))
  hourly_tbl <- dplyr::left_join(hourly_value_tbl, hourly_flag_tbl, by = c("id", "month", "day", "hour"))
  
  # before setting types, check to ensure all numeric data is coercible (dont want to lose values)
  expected_months <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
  assertthat::assert_that(all(expected_months %in% unique(hourly_tbl$month)),
                          msg = 'An unexpected value was found in the `month` column of the hourly table.')
  expected_days <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13",
                     "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26",
                     "27", "28", "29", "30", "31")
  assertthat::assert_that(all(expected_days %in% unique(hourly_tbl$day)),
                          msg = 'An unexpected value was found in the `day` column of the hourly table.')
  expected_hours <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14",
                      "15", "16", "17", "18", "19", "20", "21", "22", "23", "24")
  assertthat::assert_that(all(expected_hours %in% unique(hourly_tbl$hour)),
                          msg = 'An unexpected value was found in the `hour` column of the hourly table.')
  
  # convert month, day and hour to integers, set missing values to NA
  # (any 4 number negative code not -7777) and near 0 values (code -7777) to 0
  hourly_tbl_clean <- hourly_tbl %>%
    dplyr::mutate_at(vars(month:hour), as.integer) %>%
    dplyr::mutate(value = dplyr::case_when(value %in% c(-9999L, -8888L, -6666L, -5555L) ~ as.integer(NA),
                                           value == -7777L ~ 0L,
                                           T ~ value))
  
  # add metric column & reorg the order
  hourly_tbl_clean[["metric"]] <- metric
  hourly_tbl_clean <- hourly_tbl_clean %>% dplyr::select(id, metric, everything())
  return(hourly_tbl_clean)
}

## Data Loading & Processing
# load the lookup table
hourly_file_name_lookup_tbl <- readr::read_csv("data_parsing/lookup_tables/hourly_filename_lookup.csv",
                                               col_types = cols())
# iterate through each hourly file, parse, clean the file and save the clean table
all_hourly_data_list <- purrr::map2(hourly_file_name_lookup_tbl$filename, hourly_file_name_lookup_tbl$metric,
                                    function(filename, metric) {
                                      print(metric)
                                      get_and_parse_hourly_data(filename, metric)
                                    })

# check each element of the list and then add each to the database
index_list <- vector(mode = "list", length = length(all_hourly_data_list))
index_list[[1]] <- c("id")
purrr::map2(all_hourly_data_list[13:26], index_list[13:26], function(data_tbl, index_params) {
  print(unique(data_tbl$metric))
  add_to_db(data_tbl, table_name = "hourly_weather_data",
            index_fields = index_params,
            credentials_list, overwrite = F)
})





