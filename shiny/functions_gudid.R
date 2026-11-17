#' Create a DuckDB Database from AccessGUDID Delimited Text Files
#'
#' Reads all pipe-delimited `.txt` files in an unpacked AccessGUDID release 
#' directory and writes each file as a table into a persistent DuckDB database 
#' file (`gudid.duckdb`) on disk.
#'
#' @param data_dir A single string specifying the path to the folder containing 
#'   the AccessGUDID `.txt` files.
#' @param out_dir A single string specifying the directory where the resulting 
#'   DuckDB file will be saved. Defaults to the project root via `here::here()`.
#' @param db_name A single string specifying the output DuckDB filename. 
#'   Defaults to `"gudid.duckdb"`.
#' @param overwrite A single logical value. If `FALSE` (default), aborts if the 
#'   target DuckDB file already exists. If `TRUE`, deletes the existing database 
#'   file before rebuilding.
#'
#' @return `NULL`, invisibly. Called for its side effect of creating the 
#'   DuckDB database file on disk.
#'
#' @importFrom DBI dbConnect dbDisconnect dbQuoteIdentifier dbQuoteString dbExecute
#' @importFrom duckdb duckdb
#' @importFrom purrr walk
#' @importFrom tools file_path_sans_ext
#' @importFrom cli cli_abort cli_inform cli_alert_success
#' @export
#'
#' @examples
#' \dontrun{
#' build_gudid_duckdb(
#'   data_dir  = here::here("AccessGUDID_Delimited_Full_Release_20260202"),
#'   out_dir   = here::here()
#' )
#' }
build_gudid_duckdb <- function(data_dir,
                               out_dir = here::here(),
                               db_name = "gudid.duckdb",
                               overwrite = TRUE) {
  if (!is.character(data_dir) || length(data_dir) != 1L || !dir.exists(data_dir)) {
    cli::cli_abort("{.arg data_dir} must be a valid path to an existing directory.")
  }
  if (!is.character(out_dir) || length(out_dir) != 1L) {
    cli::cli_abort("{.arg out_dir} must be a single directory path string.")
  }
  if (!is.character(db_name) || length(db_name) != 1L || !nzchar(db_name)) {
    cli::cli_abort("{.arg db_name} must be a non-empty string.")
  }
  if (!is.logical(overwrite) || length(overwrite) != 1L || is.na(overwrite)) {
    cli::cli_abort("{.arg overwrite} must be {.val TRUE} or {.val FALSE}.")
  }
  
  files <- list.files(data_dir, pattern = "\\.txt$", full.names = TRUE)
  if (length(files) == 0L) {
    cli::cli_abort("No {.file .txt} files found in {.path {data_dir}}.")
  }
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }
  
  db_path <- file.path(out_dir, db_name)
  
  if (file.exists(db_path)) {
    if (!overwrite) {
      cli::cli_abort(c(
        "Database file already exists at {.path {db_path}}.",
        "i" = "Set {.code overwrite = TRUE} to replace it."
      ))
    }
    cli::cli_inform("Removing existing database at {.path {db_path}}")
    file.remove(db_path)
    
    # Also clean up any leftover DuckDB write-ahead log (.wal) file
    wal_path <- paste0(db_path, ".wal")
    if (file.exists(wal_path)) {
      file.remove(wal_path)
    }
  }
  
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  
  purrr::walk(files, \(file_path) {
    table_name <- tools::file_path_sans_ext(basename(file_path))
    clean_path <- normalizePath(file_path, winslash = "/", mustWork = TRUE)
    
    query <- sprintf(
      "CREATE OR REPLACE TABLE %s AS SELECT * FROM read_csv(%s, delim = '|', header = true, quote = '', all_varchar = %s);",
      DBI::dbQuoteIdentifier(con, table_name),
      DBI::dbQuoteString(con, clean_path),
      "true"
    )
    
    cli::cli_inform("Importing table: {.field {table_name}}")
    DBI::dbExecute(con, query)
  })
  
  cli::cli_alert_success(
    "Created DuckDB database with {length(files)} table{?s} at {.path {db_path}}"
  )
  
  invisible(NULL)
}

#' Create Lazy Table References to an AccessGUDID Database Connection
#'
#' Validates a DBI database connection and returns a named list of lazy 
#' [dplyr::tbl()] references for all nine standard AccessGUDID tables, 
#' wrapped in the `"gudid_ref"` S3 class.
#'
#' @param con A valid, open `DBIConnection` object connected to an AccessGUDID 
#'   database (such as one created by `build_gudid_duckdb()`).
#'
#' @return An object of class `"gudid_ref"` (inheriting from `"list"`), 
#'   containing nine named `tbl_sql` references: `contacts`, `device`, 
#'   `deviceSizes`, `environmentalConditions`, `gmdnTerms`, `identifiers`, 
#'   `premarketSubmissions`, `productCodes`, and `sterilizationMethodTypes`.
#'
#' @importFrom DBI dbIsValid dbListTables
#' @importFrom dplyr tbl
#' @importFrom purrr map
#' @importFrom rlang set_names
#' @importFrom cli cli_abort
#' @export
#'
#' @examples
#' \dontrun{
#' db <- DBI::dbConnect(
#'   duckdb::duckdb(),
#'   dbdir = here::here("gudid.duckdb"),
#'   read_only = TRUE
#' )
#' gudid <- gudidFromCon(db)
#' gudid$device
#' }
gudidFromCon <- function(con) {
  if (!inherits(con, "DBIConnection") || !DBI::dbIsValid(con)) {
    cli::cli_abort("{.arg con} must be a valid, open {.cls DBIConnection} object.")
  }
  
  gudid_tables <- rlang::set_names(c(
    "contacts",
    "device",
    "deviceSizes",
    "environmentalConditions",
    "gmdnTerms",
    "identifiers",
    "premarketSubmissions",
    "productCodes",
    "sterilizationMethodTypes"
  ))
  
  existing_tables <- DBI::dbListTables(con)
  missing_tables <- setdiff(gudid_tables, existing_tables)
  
  if (length(missing_tables) > 0L) {
    cli::cli_abort(c(
      "Database connection is missing expected AccessGUDID table{?s}: {.field {missing_tables}}.",
      "i" = "Did you build the database with {.fn build_gudid_duckdb}?"
    ))
  }
  
  gudid_ref <- purrr::map(gudid_tables, \(x) dplyr::tbl(src = con, from = x))
  
  structure(gudid_ref, class = c("gudid_ref", class(gudid_ref)))
}
