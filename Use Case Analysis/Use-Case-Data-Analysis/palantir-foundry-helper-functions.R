#' @title Connects to a table in Palantir Foundry
#' @description Creates a 'tbl' using the 'duckplyr' R package,
#' based on the Parquet files that represent a given table
#' in Palantir Foundry.
#' @param table_name The name of the table
#' @return A 'tbl' object with 'duckplyr'. This allows
#' you to use the familiar 'dplyr' verbs like 'mutate()' or 'summarise()'
#' @examples
#' person_omop_table <- connect_to_table("person")
#' person_omop_table |> 
#'   summarize(n_persons = n_distinct(person_id))
#'   
connect_to_table <- function(table_name) {
  parquet_files <- foundry::datasets.list_files(table_name) |>
    foundry::datasets.download_files(alias = table_name) |>
    unlist()
  tbl_from_parquet_files <- parquet_files |>
    duckplyr::read_parquet_duckdb() |>
    duckplyr::as_tbl()
  return(tbl_from_parquet_files)
}

#' @title Execute a SQL query through Python
#' @description Executes a SQL query using an R interface to Python,
#' which allows use of Palantir Foundry's internal 'containers_sql' module.
#' Returns an Arrow table.
#' @details
#' This function is useful because SQL queries through Python
#' can utilize Palantir Foundry's Spark computing resources. So you can
#' run computationally-demanding SQL queries that are difficult or infeasible
#' even with R's 'duckdb' or 'arrow' packages, 
#' due to limitations in the way that Palantir Foundry runs R.
#' @param sql_string A string containing the SQL code
#' @return Returns an Arrow Table. This can be converted to an R data frame
#' by calling `as.data.frame()` or `collect()`. It can also be further
#' manipulated using R packages such as 'arrow', 'dplyr', 'duckdb', or 'duckplyr'.
#' @examples 
#' 
#' execute_foundry_sql("SELECT * from `my_table` LIMIT 5")
#' 
execute_foundry_sql <- function(sql_string) {
  sql_string <- sql_string |> 
    stringr::str_replace_all("\\n", " ") |>
    stringr::str_trim()
  foundry_sql <- reticulate::import("containers_sql")$FoundrySdkSqlExecutor()
  query <- foundry_sql$execute(sql_string)
  query$fetch_results() |> arrow::as_arrow_table()
}