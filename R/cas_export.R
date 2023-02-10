#' Export all database table to another format such as csv
#'
#' @param path Defaults to NULL. If NULL, path is set to the project/website/export folder.
#' @param file_format Defaults to "csv.gz", i.e. compressed csv files. All formats supported by `readr::write_csv()` are valid.
#' @param tables Defaults to NULL. If NULL, all database tables are exported. If given, names of the database tables to export. See `vignette("castarter-database")` for details.
#' @inheritParams cas_read_from_db
#' @return
#' @export
#'
#' @examples
cas_export <- function(path = NULL,
                       file_format = "csv.gz",
                       tables = NULL,
                       db_connection = NULL,
                       db_folder = NULL,
                       ...) {
  if (cas_check_use_db(...) == FALSE) {
    usethis::ui_stop("Database not set. Set the database connection with `cas_set_options()` or pass database connection with the parameter `db_connection`.")
  }

  db <- cas_connect_to_db(
    db_connection = db_connection,
    read_only = FALSE,
    ...
  )

  cas_options_l <- cas_get_options(...)

  if (is.null(path) == TRUE) {
    path <- fs::path(
      cas_get_base_path(...) %>%
        fs::path_dir(),
      "export",
      fs::path_sanitize(Sys.time(),
        replacement = "_"
      ) %>%
        stringr::str_replace(
          pattern = " ",
          replacement = "-"
        )
    )
  }

  fs::dir_create(path = path)

  if (is.null(tables) == TRUE) {
    tables_v <- DBI::dbListTables(conn = db)
  } else {
    tables_v <- tables
  }

  purrr::walk(
    .x = tables_v,
    .f = function(current_table) {
      DBI::dbReadTable(conn = db, name = current_table) %>%
        readr::write_csv(file = fs::path(
          path,
          stringr::str_c(
            current_table,
            "-",
            cas_options_l$website,
            ".",
            file_format
          )
        ) %>%
          fs::path_sanitize())
    }
  )

  cas_disconnect_from_db(
    db_connection = db,
    ...
  )

  cli::cli_inform(c(
    v = "Tables {.field {stringr::str_flatten_comma(tables_v)}} have been successfully exported in {.field {file_format}} format",
    i = "Exported files are available in the {.path {path}} folder"
  ))
}
