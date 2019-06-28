source("startup.R")

## Description & Data Details
# The daily weather data is contained on a series of files, each one containing a different weather value.
# This is a fixed width file with the width of each column shown below.
# ------------------------------
# Variable  Columns  Type
# ----------------------------
# STNID       1- 11  Character
# MONTH      13- 14  Integer
# VALUE1     19- 23  Integer
# FLAG1      24- 24  Character
# - - - - - - - - - - - - - - 
# VALUE31   229-233  Integer
# FLAG31    234-234  Character
# ----------------------------
#
# For more details including field descriptions, etc please see Section III. File Formats Part C
# https://www1.ncdc.noaa.gov/pub/data/normals/1981-2010/readme.txt

## Define Helper Functions
get_and_parse_daily_data <- function(filename, metric) {
  # load in daily data (suppress parsing warnings)
  raw_daily_tbl <- readr::read_fwf(paste0("data_parsing/daily/", filename), readr::fwf_widths(c(12, 6, rep(c(5, 2), 31))),
                                    col_types = cols())
  
  # add in column names
  daily_col_names <- paste(rep(c("day_value", "day_flag"), 31), rep(seq(1, 31), each = 2), sep = "_")
  colnames(raw_daily_tbl) <- c("id", "month", daily_col_names)
  
  # transform data into long (tidy) format (values and flags seperate then joined together)
  daily_value_tbl <- raw_daily_tbl %>%
    tidyr::gather(starts_with("day_value"), key = "day", value = "value") %>%
    dplyr::select(-c(starts_with("day_flag"))) %>%
    dplyr::mutate(day = purrr::map_chr(stringr::str_split(day, "_"), 3))
  daily_flag_tbl <- raw_daily_tbl %>%
    tidyr::gather(starts_with("day_flag"), key = "day", value = "flag") %>%
    dplyr::select(-c(starts_with("day_value"))) %>%
    dplyr::mutate(day = purrr::map_chr(stringr::str_split(day, "_"), 3))
  daily_tbl <- dplyr::left_join(daily_value_tbl, daily_flag_tbl, by = c("id", "month", "day"))
  
  # before setting types, check to ensure all numeric data is coercible (dont want to lose values)
  expected_months <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
  assertthat::assert_that(all(unique(daily_tbl$month) %in% expected_months),
                          msg = 'An unexpected value was found in the `month` column of the daily table.')
  expected_days <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13",
                     "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26",
                     "27", "28", "29", "30", "31", "1", "2", "3", "4", "5", "6", "7", "8", "9")
  assertthat::assert_that(all(unique(daily_tbl$day) %in% expected_days),
                          msg = 'An unexpected value was found in the `day` column of the daily table.')

  # convert month and day to integers, set missing values to NA
  # (any 4 number negative code not -7777) and near 0 values (code -7777) to 0
  daily_tbl_clean <- daily_tbl %>%
    dplyr::mutate_at(vars(month:day), as.integer) %>%
    dplyr::mutate(value = dplyr::case_when(value %in% c(-9999L, -6666L, -5555L) ~ as.integer(NA),
                                           value == -7777L ~ 0L,
                                           T ~ value)) %>%
    dplyr::filter(value != -8888L)
  
  # add metric column & reorg the order
  daily_tbl_clean[["metric"]] <- metric
  daily_tbl_clean <- daily_tbl_clean %>% dplyr::select(id, metric, everything())
  return(daily_tbl_clean)
}


## Data Loading & Processing
# load the lookup table
daily_file_name_lookup_tbl <- readr::read_csv("data_parsing/lookup_tables/daily_filename_lookup.csv",
                                               col_types = cols())
# iterate through each daily file, parse, clean the file and save the clean table
# add each to the database
index_list <- vector(mode = "list", length = nrow(daily_file_name_lookup_tbl))
index_list[[1]] <- c("id")
all_daily_data_list <- purrr::pmap(list(daily_file_name_lookup_tbl$filename[12:45], daily_file_name_lookup_tbl$metric[12:45], index_list[12:45]),
                                    function(filename, metric, index_params) {
                                      print(metric)
                                      temp_tbl <- get_and_parse_daily_data(filename, metric)
                                      temp_result <- add_to_db(temp_tbl, table_name = "daily_weather_data",
                                                index_fields = index_params,
                                                credentials_list, overwrite = F)
                                    })





