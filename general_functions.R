# connect to the database, add a new table, append to an existing one or overwrite the old table
add_to_db <- function(data_tbl, table_name, index_fields, credentials_list, overwrite = F) {
  # add to database (set your PostgreSQL db credentials in startup.R)
  con <- DBI::dbConnect(RPostgres::Postgres(),
                        dbname = credentials_list$db_db,
                        host = credentials_list$db_host,
                        port = credentials_list$db_port,
                        user = credentials_list$db_user,
                        password = credentials_list$db_password)
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


get_weather_station_data <- function(station_id, credentials_list, pool = NULL) {
  # add to database (set your PostgreSQL db credentials in startup.R)
  if (is.null(pool)) {
    con <- DBI::dbConnect(RPostgres::Postgres(),
                          dbname = credentials_list$db_db,
                          host = credentials_list$db_host,
                          port = credentials_list$db_port,
                          user = credentials_list$db_user,
                          password = credentials_list$db_password)
  } else {
    con <- pool
  }

  table_name_vec <- c("daily_weather_data", "hourly_weather_data")
  freq_vec <- c("d", "h")
  all_data_list <- purrr::map2(table_name_vec, freq_vec, function(table_name, freq) {
    temp_tbl <- dplyr::tbl(con, table_name) %>%
      dplyr::filter(id == station_id) %>%
      dplyr::collect()
    if (nrow(temp_tbl) > 0) {
      temp_tbl[["freq"]] <- freq
    }
    return(temp_tbl)
  })
  return(dplyr::bind_rows(all_data_list))
}