#' Export database tables to another format such as csv
#'
#' @param path Defaults to NULL. If NULL, path is set to the
#'   project/website/export/file_format folder.
#' @param file_format Defaults to "csv.gz", i.e. compressed csv files. All
#'   formats supported by [readr::write_csv()] are valid.
#' @param tables Defaults to NULL. If NULL, all database tables are exported. If
#'   given, names of the database tables to export. See
#'   `vignette("castarter-database")` for details.
#' @inheritParams cas_read_from_db
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' if (interactive) {
#'   cas_export_tables(file_format = "csv")
#' }
#' }
cas_export_tables <- function(
  path = NULL,
  file_format = "csv.gz",
  tables = NULL,
  db_connection = NULL,
  disconnect_db = FALSE,
  db_folder = NULL,
  ...
) {
  if (cas_check_use_db(...) == FALSE) {
    cli::cli_abort(
      "Database not set. Set the database connection with {.fun cas_set_options} or pass database connection with the argument {.arg db_connection}."
    )
  }

  db <- cas_connect_to_db(
    db_connection = db_connection,
    read_only = TRUE,
    ...
  )

  cas_options_l <- cas_get_options(...)

  if (is.null(path)) {
    path <- fs::path(
      cas_get_base_folder(level = "website"),
      "export",
      "tables",
      file_format |>
        stringr::str_replace(
          pattern = stringr::fixed("."),
          replacement = "_"
        ),
      fs::path_sanitize(Sys.time() |> format("%Y-%m-%d-%H_%M_%S"))
    )
  }

  if (fs::file_exists(path)) {
    cli::cli_abort(
      "The folder {.path {path}} already exists. Please remove or rename it before exporting."
    )
  }

  fs::dir_create(path = path)

  if (is.null(tables)) {
    tables_v <- DBI::dbListTables(conn = db)
  } else {
    tables_v <- tables
  }

  purrr::walk(
    .x = tables_v,
    .f = function(current_table) {
      DBI::dbReadTable(conn = db, name = current_table) |>
        dplyr::collect() |>
        readr::write_csv(
          file = fs::path(
            path,
            stringr::str_c(
              current_table,
              "-",
              cas_options_l$website,
              ".",
              file_format
            ) |>
              fs::path_sanitize()
          )
        )
    }
  )

  cas_disconnect_from_db(
    db_connection = db,
    disconnect_db = disconnect_db
  )

  cli::cli_inform(c(
    v = "Tables {.field {stringr::str_flatten_comma(tables_v)}} have been successfully exported in {.field {file_format}} format",
    i = "Exported files are available in the following folder: {.path {path}}"
  ))
}
