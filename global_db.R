

pool <- pool::dbPool(
  drv = RPostgres::Postgres(),
  dbname = dw$database,
  host = dw$server,
  port = dw$port,
  user = dw$uid,
  password = dw$pwd)

all_stations_tbl <- dplyr::tbl(pool, "weather_station_map") %>%
  dplyr::collect() %>%
  dplyr::mutate(freq = dplyr::case_when(hourly ~ "h",
                                        daily ~ "d",
                                        T ~ as.character(NA))) %>%
  dplyr::filter(!is.na(freq))