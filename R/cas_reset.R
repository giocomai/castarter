#' Delete a specific table from database
#'
#' @param table Name of the table. Yuu can use
#'   `DBI::dbListTables(cas_connect_to_db())` to see currently available tables.
#'   See `vignette("castarter-database")` for more information about the
#'   contents and structure of each table.
#' @param ask Logical, defaults to TRUE. If set to FALSE, the relevant table
#'   will be deleted without asking for confirmation from the user.
#' @inheritParams cas_write_to_db
#'
#' @return
#' @export
#'
#' @examples
cas_reset_db <- function(table,
                         db_connection = NULL,
                         disconnect_db = FALSE,
                         db_folder = NULL,
                         ask = TRUE,
                         ...) {
  if (cas_check_use_db(...) == FALSE) {
    usethis::ui_stop("Database not set. Set the database connection with `cas_set_options()` or pass database connection with the parameter `db_connection`.")
  }

  db <- cas_connect_to_db(
    db_connection = db_connection,
    read_only = FALSE,
    ...
  )

  if (DBI::dbExistsTable(conn = db, name = table) == FALSE) {
    # do nothing: if table does not exist, previous data cannot be there
  } else {
    if (isFALSE(ask)) {
      DBI::dbRemoveTable(conn = db, name = table)
      cli::cli_alert_success("The table {.field {sQuote(table)}} has been removed from the local database")
    } else if (usethis::ui_yeah(x = paste0("Are you sure you want to remove from the local database the following table: ", sQuote(table), "?"))) {
      DBI::dbRemoveTable(conn = db, name = table)
      cli::cli_alert_success("The table {.field {sQuote(table)}} has been removed from the local database")
    }
  }

  cas_disconnect_from_db(
    db_connection = db,
    disconnect_db = disconnect_db
  )
}

#' Removes from the local database the folder where extracted data are stored
#'
#' @inheritParams cas_reset_db
#'
#' @return
#' @export
#'
#' @examples
cas_reset_db_contents_data <- function(db_connection = NULL,
                                       db_folder = NULL,
                                       ask = TRUE,
                                       ...) {
  cas_reset_db(
    table = "contents_data",
    db_connection = db_connection,
    db_folder = db_folder,
    ask = ask,
    ...
  )
}



#' Removes from the local database the folder where links to contents associated
#' with their id are stored
#'
#' @inheritParams cas_reset_db
#'
#' @return
#' @export
#'
#' @examples
cas_reset_db_contents_id <- function(db_connection = NULL,
                                     db_folder = NULL,
                                     ask = TRUE,
                                     ...) {
  cas_reset_db(
    table = "contents_id",
    db_connection = db_connection,
    db_folder = db_folder,
    ask = ask,
    ...
  )
}


#' Removes from the local database the table where links to index urls are stored
#'
#' @inheritParams cas_reset_db
#'
#' @return
#' @export
#'
#' @examples
cas_reset_db_index_id <- function(db_connection = NULL,
                                  db_folder = NULL,
                                  ask = TRUE,
                                  ...) {
  cas_reset_db(
    table = "index_id",
    db_connection = db_connection,
    db_folder = db_folder,
    ask = ask,
    ...
  )
}

#' Removes from the local database all identifiers included in the ignore list
#'
#' @inheritParams cas_reset_db
#'
#' @return
#' @export
#'
#' @examples
#' cas_set_options(
#'   base_folder = fs::path(tempdir(), "R", "cas_reset_db_ignore_id"),
#'   db_folder = fs::path(tempdir(), "R", "cas_reset_db_ignore_id"),
#'   project = "example_project",
#'   website = "example_website"
#' )
#' cas_enable_db()
#'
#'
#' cas_write_db_ignore_id(id = sample(x = 1:100, size = 10))
#'
#' cas_read_db_ignore_id()
#'
#' cas_reset_db_ignore_id(ask = FALSE)
#'
#' cas_read_db_ignore_id()
cas_reset_db_ignore_id <- function(db_connection = NULL,
                                   db_folder = NULL,
                                   ask = TRUE,
                                   ...) {
  cas_reset_db(
    table = "contents_ignore",
    db_connection = db_connection,
    db_folder = db_folder,
    ask = ask,
    ...
  )
}

#' Delete all files and database records for the index pages of the current
#' website
#'
#' @param batch Defaults to NULL. If given, only files and records related to
#'   the given batch are removed. If not given, all index files are removed.
#' @inheritParams cas_reset_db
#'
#' @return
#' @export
#'
#' @examples
cas_reset_download_index <- function(batch = NULL,
                                     file_format = "html",
                                     db_connection = NULL,
                                     db_folder = NULL,
                                     ask = TRUE,
                                     ...) {
  if (is.null(batch) == TRUE) {
    if (usethis::ui_yeah("Do you wish to delete all files and all {usethis::ui_field('index')} download files and records in the database for the the website {usethis::ui_field(cas_get_options(...)$website)}?")) {
      folder_path <- cas_get_base_path(index = TRUE, file_format = file_format)
      n_folders <- fs::dir_ls(
        path = folder_path,
        recurse = FALSE,
        type = "directory"
      ) %>%
        length()
      n_files <- fs::dir_ls(
        path = folder_path,
        recurse = TRUE,
        type = "file"
      ) %>%
        length()
      cli::cli_alert_danger(c("All files and folder within {.path {folder_path}} will be deleted. More specifically, {scales::number(n_folders)} {ifelse(n_folders==1, 'folder', 'folders')} and {scales::number(n_files)} {ifelse(n_files==1, 'file', 'files')} will be deleted, and all records of the download will be removed from the local database"))
      if (usethis::ui_yeah("Do you really wish to delete all {usethis::ui_field('index')} download files and records in the database for the the website {usethis::ui_field(cas_get_options(...)$website)}?")) {
        fs::dir_delete(path = folder_path)
        cas_reset_db(
          table = "index_download",
          db_connection = db_connection,
          db_folder = db_folder,
          ask = FALSE,
          ...
        )
      }
    }
  } else {
    cas_delete_from_db(
      table = "index_download",
      batch = batch,
      ask = ask,
      db_folder = db_folder,
      db_connection = db_connection,
      ...
    )
    index_folder_path <- cas_get_base_path(
      index = TRUE,
      file_format = file_format
    )

    if (fs::file_exists(path = index_folder_path)) {
      batch_folder_to_remove <- fs::path(index_folder_path, batch)

      n_files <- fs::dir_ls(
        path = batch_folder_to_remove,
        recurse = TRUE,
        type = "file"
      ) %>%
        length()

      if (usethis::ui_yeah(glue::glue("Do you really wish to delete all {usethis::ui_field('index')} downloaded files in the batch {usethis::ui_value(batch)} for the the website {usethis::ui_field(cas_get_options(...)$website)}? If you confirm the folder {usethis::ui_path(batch_folder_to_remove)} and all the {scales::number(n_files)} files it contains will be removed."))) {
        fs::dir_delete(path = batch_folder_to_remove)
      }
    }
  }
}


#' Delete all files and database records for the contents pages of the current
#' website
#'
#' @param batch Defaults to NULL. If given, only files and records related to
#'   the given batch are removed. If not given, all contents files are removed.
#' @inheritParams cas_reset_db
#'
#' @return
#' @export
#'
#' @examples
cas_reset_download_contents <- function(batch = NULL,
                                        file_format = "html",
                                        db_connection = NULL,
                                        db_folder = NULL,
                                        ask = TRUE,
                                        ...) {
  if (is.null(batch) == TRUE) {
    if (usethis::ui_yeah("Do you wish to delete all files and all {usethis::ui_field('contents')} download files and records in the database for the the website {usethis::ui_field(cas_get_options(...)$website)}?")) {
      folder_path <- cas_get_base_path(index = FALSE, file_format = file_format)
      n_folders <- fs::dir_ls(
        path = folder_path,
        recurse = FALSE,
        type = "directory"
      ) %>%
        length()
      n_files <- fs::dir_ls(
        path = folder_path,
        recurse = TRUE,
        type = "file"
      ) %>%
        length()
      cli::cli_alert_danger(c("All files and folder within {.path {folder_path}} will be deleted. More specifically, {scales::number(n_folders)} {ifelse(n_folders==1, 'folder', 'folders')} and {scales::number(n_files)} {ifelse(n_files==1, 'file', 'files')} will be deleted, and all records of the download will be removed from the local database."))
      if (usethis::ui_yeah("Do you really wish to delete all {usethis::ui_field('contents')} download files and records in the database for the the website {usethis::ui_field(cas_get_options(...)$website)}?")) {
        fs::dir_delete(path = folder_path)
        cas_reset_db(
          table = "contents_download",
          db_connection = db_connection,
          db_folder = db_folder,
          ask = FALSE,
          ...
        )
      }
    }
  } else {
    cas_delete_from_db(
      table = "contents_download",
      batch = batch,
      ask = ask,
      db_folder = db_folder,
      db_connection = db_connection,
      ...
    )

    contents_folder_path <- cas_get_base_path(
      index = FALSE,
      file_format = file_format
    )

    if (fs::file_exists(path = contents_folder_path)) {
      batch_folder_to_remove <- fs::path(contents_folder_path, batch)

      n_files <- fs::dir_ls(
        path = batch_folder_to_remove,
        recurse = TRUE,
        type = "file"
      ) %>%
        length()

      if (usethis::ui_yeah(glue::glue("Do you really wish to delete all {usethis::ui_field('contents')} downloaded files in the batch {usethis::ui_value(batch)} for the the website {usethis::ui_field(cas_get_options(...)$website)}? If you confirm the folder {usethis::ui_path(batch_folder_to_remove)} and all the {scales::number(n_files)} files it contains will be removed."))) {
        fs::dir_delete(path = batch_folder_to_remove)
      }
    }
  }
}
