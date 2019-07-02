# connect to the database, add a new table, append to an existing one or overwrite the old table
add_to_db <- function(data_tbl, table_name, index_fields, dw, overwrite = F) {
  # add to database (set your PostgreSQL db credentials in startup.R)
  con <- DBI::dbConnect(RPostgres::Postgres(),
                        dbname = dw$database,
                        host = dw$server,
                        port = dw$port,
                        user = dw$uid,
                        password = dw$pwd)
  if (DBI::dbExistsTable(con, table_name)) {
    add_result <- dbWriteTable(con, table_name, data_tbl, append = T)
  } else if (overwrite) {
    add_result <- dbWriteTable(con, table_name, data_tbl, overwrite = T)
  } else {
    add_result <- dbWriteTable(con, table_name, data_tbl)
  }
  
  if (!is.null(index_fields)) {
    # optionally create an index to help query the data
    # build the sql query which will create the index statment
    index_query_raw <- paste('CREATE INDEX',
                             paste('"', table_name, '.index"', ' ON "', table_name, '" (',
                                   paste(index_fields, collapse = ", "), ');', sep = ''))
    index_query <- suppressWarnings(DBI::dbSendQuery(con, index_query_raw))
    # execute the query to build the index then clear the results
    index_create_result <- suppressWarnings(DBI::dbFetch(index_query))
    clear_results <- suppressWarnings(DBI::dbClearResult(index_query))
  } else {
    index_create_result <- NULL
  }
  
  # create a return list
  return_list <- list("add_result" = add_result,
                      "index_result" = index_create_result)
  return(return_list)
}


get_weather_station_data <- function(station_id, dw, pool = NULL,
                                     frequency = "d") {
  # add to database (set your PostgreSQL db credentials in startup.R)
  if (is.null(pool)) {
    con <- DBI::dbConnect(RPostgres::Postgres(),
                          dbname = dw$database,
                          host = dw$server,
                          port = dw$port,
                          user = dw$uid,
                          password = dw$pwd)
  } else {
    con <- pool
  }

  if (frequency == "d") {
    table_name_use <- "daily_weather_data"
  } else if (frequency == "h") {
    table_name_use <- "hourly_weather_data"
  }
  
  temp_tbl <- dplyr::tbl(con, table_name_use) %>%
    dplyr::filter(id == station_id) %>%
    dplyr::collect()
  if (nrow(temp_tbl) > 0) {
    temp_tbl[["freq"]] <- frequency
  }
  
  return(temp_tbl)
}