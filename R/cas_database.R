#' Creates the base folder where `castarter` stores the project database.
#'
#' @param ask Logical, defaults to TRUE. If FALSE, and database folder does not exist, it just creates it without asking (useful for non-interactive sessions).
#'
#' @family database functions
#'
#' @return Nothing, used for its side effects.
#' @export
#'
#' @examples
#' cas_create_db_folder(path = fs::path(fs::path_temp(), "cas_data"))
cas_create_db_folder <- function(path = NULL,
                                 ask = TRUE,
                                 ...) {
  db_path <- cas_get_db_folder(
    path = path,
    ...
  )

  if (fs::file_exists(db_path) == FALSE) {
    if (ask == FALSE) {
      fs::dir_create(path = db_path, recurse = TRUE)
    } else {
      usethis::ui_info(glue::glue("The database folder {{usethis::ui_path(cas_get_db_folder())}} does not exist. If you prefer to store database files elsewhere, reply negatively and set your preferred database folder with `cas_set_db_folder()`"))
      check <- usethis::ui_yeah(glue::glue("Do you want to create {{usethis::ui_path(cas_get_db_folder())}} for storing data in a local database?"))
      if (check == TRUE) {
        fs::dir_create(path = db_path, recurse = TRUE)
      }
    }
    if (fs::file_exists(db_path) == FALSE) {
      usethis::ui_stop("This function requires a valid database folder.")
    }
  }
}


#' Set folder for storing the database
#'
#' Consider using a folder out of your current project directory, e.g. `cas_set_db_folder("~/R/cas_data/")`: you will be able to use the same database in different projects, and prevent database files from being sync-ed if you use services such as Nextcloud or Dropbox.
#'
#' @param path A path to a location used for storing the database. If the folder does not exist, it will be created.
#'
#' @family database functions
#'
#' @return The path to the database folder, if previously set; the same path as given to the function; or the default, `cas_data` is none is given.
#' @export
#' @examples
#' cas_set_db_folder(fs::path(fs::path_home_r(), "R", "cas_data"))
#'
#' cas_set_db_folder(fs::path(fs::path_temp(), "cas_data"))
cas_set_db_folder <- function(path = NULL,
                              ...) {
  if (is.null(path)) {
    path <- Sys.getenv("castarter_database_folder")
  } else {
    Sys.setenv(castarter_database_folder = path)
  }
  if (path == "") {
    path <- cas_get_base_folder(
      level = "website",
      ...
    )
  }
  invisible(path)
}

#' @rdname cas_set_db_folder
#' @examples
#' cas_get_db_folder()
#' @export
cas_get_db_folder <- function(path = NULL,
                              ...) {
  if (is.null(path)) {
    path <- Sys.getenv("castarter_database_folder")
  }

  if (path == "") {
    path <- cas_get_base_folder(
      level = "website",
      ...
    )
  }

  invisible(path)
}


#' Set database connection settings for the session
#'
#' @param db_settings A list of database connection settings (see example)
#' @param driver A database driver. Common database drivers include `MySQL`, `PostgreSQL`, and `MariaDB`. See `unique(odbc::odbcListDrivers()[[1]])` for a list of locally available drivers.
#' @param host Host address, e.g. "localhost".
#' @param port Port to use to connect to the database.
#' @param database Database name.
#' @param user Database user name.
#' @param pwd Password for the database user.
#'
#' @family database functions
#'
#' @return A list with all given parameters (invisibly).
#' @export
#'
#' @examples
#' \donttest{
#' if (interactive()) {
#'   # Settings can be provided either as a list
#'   db_settings <- list(
#'     driver = "MySQL",
#'     host = "localhost",
#'     port = 3306,
#'     database = "castarter",
#'     user = "secret_username",
#'     pwd = "secret_password"
#'   )
#'
#'   cas_set_db(db_settings)
#'
#'   # or as parameters
#'
#'   cas_set_db(
#'     driver = "MySQL",
#'     host = "localhost",
#'     port = 3306,
#'     database = "castarter",
#'     user = "secret_username",
#'     pwd = "secret_password"
#'   )
#' }
#' }
cas_set_db <- function(db_settings = NULL,
                       driver = NULL,
                       host = NULL,
                       port,
                       database,
                       user,
                       pwd) {
  if (is.null(db_settings) == TRUE) {
    if (is.null(driver) == FALSE) Sys.setenv(castarter_db_driver = driver)
    if (is.null(host) == FALSE) Sys.setenv(castarter_db_host = host)
    if (is.null(port) == FALSE) Sys.setenv(castarter_db_port = port)
    if (is.null(database) == FALSE) Sys.setenv(castarter_db_database = database)
    if (is.null(user) == FALSE) Sys.setenv(castarter_db_user = user)
    if (is.null(pwd) == FALSE) Sys.setenv(castarter_db_pwd = pwd)
    return(invisible(
      list(
        driver = driver,
        host = host,
        port = port,
        database = database,
        user = user,
        pwd = pwd
      )
    ))
  } else {
    Sys.setenv(castarter_db_driver = db_settings$driver)
    Sys.setenv(castarter_db_host = db_settings$host)
    Sys.setenv(castarter_db_port = db_settings$port)
    Sys.setenv(castarter_db_database = db_settings$database)
    Sys.setenv(castarter_db_user = db_settings$user)
    Sys.setenv(castarter_db_pwd = db_settings$pwd)
    return(invisible(db_settings))
  }
}

#' Get database connection settings from the environment
#'
#' Typically set with `cas_set_db()`
#'
#' @family database functions
#'
#' @return A list with all database parameters as stored in environment variables.
#' @export
#'
#' @examples
#'
#' cas_get_db_settings()
cas_get_db_settings <- function() {
  list(
    driver = Sys.getenv("castarter_db_driver"),
    host = Sys.getenv("castarter_db_host"),
    port = Sys.getenv("castarter_db_port"),
    database = Sys.getenv("castarter_db_database"),
    user = Sys.getenv("castarter_db_user"),
    pwd = Sys.getenv("castarter_db_pwd")
  )
}



#' Gets location of database file
#'
#' @params db_type Defaults to "DuckDB". Valid values include "SQLite".
#'
#' @return A character vector of length one with location of the SQLite database file.
#' @export
#'
#' @examples
#'
#' cas_set_db_folder(path = tempdir())
#' db_file_location <- cas_get_db_file(project = "test-project") # outputs location of database file
#' db_file_location
cas_get_db_file <- function(db_folder = NULL,
                            ...) {
  db_folder <- cas_get_db_folder(path = db_folder,
                                 ...)

  cas_options_l <- cas_get_options(...)

  fs::path(
    db_folder,
    stringr::str_c(
      "cas_",
      cas_options_l$project,
      "_",
      cas_options_l$website,
      "_db.",
      stringr::str_to_lower(cas_options_l$db_type)
    ) %>%
      fs::path_sanitize()
  )
}

#' Enable caching for the current session
#'
#' @inheritParams cas_get_db_file
#'
#' @family database functions
#'
#' @return Nothing, used for its side effects.
#' @export
#' @examples
#' \donttest{
#' if (interactive()) {
#'   cas_enable_db()
#' }
#' }
cas_enable_db <- function(db_type = "SQLite") {
  Sys.setenv(castarter_database = TRUE)
  Sys.setenv(castarter_db_type = db_type)
}


#' Disable caching for the current session
#'
#' @family database functions
#'
#' @return Nothing, used for its side effects.
#' @export
#' @examples
#' \donttest{
#' if (interactive()) {
#'   cas_disable_db()
#' }
#' }
cas_disable_db <- function() {
  Sys.setenv(castarter_database = FALSE)
}

#' Check caching status in the current session, and override it upon request
#'
#' Mostly used internally in functions, exported for reference.
#'
#' @param use_db Defaults to NULL. If NULL, checks current use_db settings. If given, returns given value, ignoring use_db.
#'
#' @family database functions
#'
#' @return Either TRUE or FALSE, depending on current use_db settings.
#' @export
#' @examples
#' cas_check_use_db()
cas_check_use_db <- function(use_db = NULL,
                             ...) {
  if (is.null(use_db) == FALSE) {
    return(as.logical(use_db))
  }
  current_database <- Sys.getenv("castarter_database")
  if (current_database == "") {
    as.logical(FALSE)
  } else {
    as.logical(current_database)
  }
}

#' Checks if database folder exists, if not returns an informative message
#'
#' @family database functions
#'
#' @return If the database folder exists, returns TRUE. Otherwise throws an error.
#' @export
#'
#' @examples
#'
#' # If database folder does not exist, it throws an error
#' tryCatch(cas_check_db_folder(),
#'   error = function(e) {
#'     return(e)
#'   }
#' )
#'
#' # Create database folder
#' cas_set_db_folder(path = fs::path(
#'   tempdir(),
#'   "cas_db_folder"
#' ))
#' cas_create_db_folder(ask = FALSE)
#'
#' cas_check_db_folder()
cas_check_db_folder <- function() {
  if (fs::file_exists(cas_get_db_folder()) == FALSE) {
    usethis::ui_stop(paste(
      "Database folder does not exist. Set it with",
      usethis::ui_code("cas_set_db_folder()"),
      "and create it with",
      usethis::ui_code("cas_create_db_folder()")
    ))
  }
  TRUE
}



#' Return a connection to be used for caching
#'
#' @param db_connection Defaults to NULL. If NULL, uses local SQLite database.
#'   If given, must be a connection object or a list with relevant connection
#'   settings (see example).
#' @param use_db Defaults to NULL. If given, it should be given either TRUE or
#'   FALSE. Typically set with `cas_enable_db()` or `cas_disable_db()`.
#' @param read_only Defaults to FALSE. Passed to `DBI::dbConnect`.
#' @param ... Passed to `cas_get_db_file()`.
#'
#' @family database functions
#'
#' @return A connection object.
#' @export
#'
#' @examples
#' \donttest{
#' if (interactive()) {
#'   db_connection <- DBI::dbConnect(
#'     RSQLite::SQLite(), # or e.g. odbc::odbc(),
#'     Driver = ":memory:", # or e.g. "MariaDB",
#'     Host = "localhost",
#'     database = "example_db",
#'     UID = "example_user",
#'     PWD = "example_pwd"
#'   )
#'   cas_connect_to_db(db_connection)
#'
#'
#'   db_settings <- list(
#'     driver = "MySQL",
#'     host = "localhost",
#'     port = 3306,
#'     database = "castarter",
#'     user = "secret_username",
#'     pwd = "secret_password"
#'   )
#'
#'   cas_connect_to_db(db_settings)
#' }
#' }
#'
cas_connect_to_db <- function(db_connection = NULL,
                              use_db = NULL,
                              db_type = NULL,
                              read_only = FALSE,
                              ...) {
  if (isFALSE(x = cas_check_use_db(use_db))) {
    return(NULL)
  }

  if (inherits(db_connection, "DBIConnection")) {
    if (DBI::dbIsValid(db_connection)) {
      return(db_connection)
    }
  }

  if (is.null(db_connection) == FALSE & is.list(db_connection) == FALSE) {
    if (DBI::dbIsValid(db_connection) == FALSE) {
      db_connection <- NULL
    }
  }

  if (is.null(db_type)) {
    db_type <- cas_get_options()$db_type
  }

  if (is.null(db_connection)) {
    db_file <- cas_get_db_file(db_type = db_type,
                               ...)
    if (fs::file_exists(db_file) == FALSE) {
      cas_create_db_folder(
        path = fs::dir_create(
          path = fs::path_dir(db_file),
          recurse = TRUE
        ),
        ask = FALSE
      )
      cli::cli_inform(message = c(i = "Folder {.path {fs::path_dir(db_file)}} for storing project and website files created."))
    }

    if (stringr::str_to_lower(db_type) == "duckdb") {
      if (requireNamespace("duckdb", quietly = TRUE) == FALSE) {
        usethis::ui_stop(x = "To use DuckDB databases you need to install the package `duckdb`.")
      }
      db <- DBI::dbConnect(
        drv = duckdb::duckdb(),
        dbdir = db_file,
        read_only = read_only
      )
      return(db)
    } else if (stringr::str_to_lower(db_type) == "sqlite") {
      if (requireNamespace("RSQLite", quietly = TRUE) == FALSE) {
        usethis::ui_stop(x = "To use SQLite databases you need to install the package `RSQLite`.")
      }
      db <- DBI::dbConnect(
        drv = RSQLite::SQLite(),
        dbname = db_file
      )
      return(db)
    } else {
      db_connection <- cas_get_db_settings()

      if (db_connection[["driver"]] == "SQLite") {
        drv <- RSQLite::SQLite()
      } else {
        if (requireNamespace("odbc", quietly = TRUE) == FALSE) {
          usethis::ui_stop(x = "To use custom databases you need to install the package `odbc`, or provide your connection directly to all functions.")
        }
        drv <- odbc::odbc()
      }

      db <- DBI::dbConnect(
        drv = drv,
        driver = db_connection[["driver"]],
        host = db_connection[["host"]],
        port = as.integer(db_connection[["port"]]),
        database = db_connection[["database"]],
        user = db_connection[["user"]],
        pwd = db_connection[["pwd"]]
      )
      return(db)
    }
  } else {
    if (is.list(db_connection)) {
      if (db_connection[["driver"]] == "SQLite") {
        if (requireNamespace("RSQLite", quietly = TRUE) == FALSE) {
          usethis::ui_stop(x = "To use SQLite databases you need to install the package `RSQLite`.")
        }
        drv <- RSQLite::SQLite()
      } else {
        if (requireNamespace("odbc", quietly = TRUE) == FALSE) {
          usethis::ui_stop(x = "To use custom databases you need to install the package `odbc`, or provide your connection directly to all functions.")
        }
        drv <- odbc::odbc()
      }

      db <- DBI::dbConnect(
        drv = drv,
        driver = db_connection[["driver"]],
        host = db_connection[["host"]],
        port = as.integer(db_connection[["port"]]),
        database = db_connection[["database"]],
        dbname = db_connection[["database"]],
        user = db_connection[["user"]],
        pwd = db_connection[["pwd"]]
      )
      return(db)
    } else {
      return(db_connection)
    }
  }
}




#' Ensure that connection to database is disconnected consistently
#'
#' @param use_db Defaults to NULL. If given, it should be given either TRUE or FALSE. Typically set with `cas_enable_db()` or `cas_disable_db()`.
#' @param db_connection Defaults to NULL. If NULL, and database is enabled, `castarter` will use a local sqlite database. A custom connection to other databases can be given (see vignette `castarter_db_management` for details).
#' @param disconnect_db Defaults to TRUE. If FALSE, leaves the connection to database open.
#'
#' @family database functions
#'
#' @return Nothing, used for its side effects.
#' @export
#'
#' @examples
#' cas_disconnect_from_db()
cas_disconnect_from_db <- function(db_connection = NULL,
                                   db_type = NULL,
                                   use_db = NULL,
                                   disconnect_db = FALSE) {
  if (isFALSE(disconnect_db)) {
    return(invisible(NULL))
  }

  if (isFALSE(x = cas_check_use_db(use_db))) {
    return(invisible(NULL))
  }


  if (is.null(db_type)) {
    db_type <- cas_get_options()$db_type
  }

  db <- cas_connect_to_db(
    db_connection = db_connection,
    use_db = use_db,
    db_type = db_type
  )

  if (DBI::dbIsValid(dbObj = db)) {
    if (inherits(db, "Pool")) {
      if (db_type == "DuckDB") {
        db_unpooled <- pool::poolCheckout(pool = db)
        DBI::dbDisconnect(db_unpooled, shutdown = TRUE)
        pool::poolReturn(db_unpooled)
      } else {
        pool::poolClose(db)
      }
    } else {
      DBI::dbDisconnect(db)
    }
  }
}


#' Generic function for writing to database
#'
#' @param df A data frame. Must correspond with the type of data expected for each table.
#' @param table Name of the table. See readme for details.
#' @param overwrite Logical, defaults to FALSE. If TRUE, checks if matching data are previously held in the table and overwrites them. This should be used with caution, as it may overwrite completely the selected table.
#'
#' @family database functions
#'
#' @inheritParams cas_connect_to_db
#' @inheritParams cas_disconnect_from_db
#'
#' @return If successful, returns silently the same data frame provided as input and written to the database. Returns silently NULL, if nothing is added, e.g. because `use_db` is set to FALSE.
#' @export
#'
#' @examples
#'
#' cas_set_options(
#'   base_folder = fs::path(tempdir(), "R", "castarter_data"),
#'   project = "example_project",
#'   website = "example_website"
#' )
#' cas_enable_db()
#'
#'
#' urls_df <- cas_build_urls(
#'   url = "https://www.example.com/news/",
#'   start_page = 1,
#'   end_page = 10
#' )
#'
#' cas_write_to_db(
#'   df = urls_df,
#'   table = "index_id"
#' )
cas_write_to_db <- function(df,
                            table,
                            overwrite = FALSE,
                            db_connection = NULL,
                            ...) {
  if (cas_check_use_db(...) == FALSE) {
    return(invisible(NULL))
  }

  db <- cas_connect_to_db(
    db_connection = db_connection,
    ...
  )

  if (DBI::dbExistsTable(conn = db, name = table) == FALSE) {
    # do nothing: if table does not exist, previous data cannot be there
  } else {
    if (overwrite == TRUE) {
      # TODO
    }
  }

  if (table == "index_id") {
    if (identical(colnames(df), colnames(casdb_empty_index_id)) & identical(sapply(df, class), sapply(casdb_empty_index_id, class))) {
      DBI::dbWriteTable(db,
        name = table,
        value = df,
        append = TRUE
      )
    } else {
      usethis::ui_stop("Incompatible data frame passed to `index_id`. `df` should have a numeric `id` column, and a character `url` and `type` column.")
    }
  } else if (table == "contents_id") {
    if (identical(colnames(df), colnames(casdb_empty_contents_id)) & identical(sapply(df, class), sapply(casdb_empty_contents_id, class))) {
      DBI::dbWriteTable(db,
        name = table,
        value = df,
        append = TRUE
      )
    } else {
      usethis::ui_stop("Incompatible data frame passed to `contents_id`.")
    }
  } else {
    # Write generic table without additional checks
    DBI::dbWriteTable(
      conn = db,
      name = table,
      value = df,
      append = TRUE
    )
  }

  cas_disconnect_from_db(
    db_connection = db,
    ...
  )
}


#' Reads data from local database
#'
#' @family database functions
#'
#' @inheritParams cas_write_to_db
#'
#' @return
#' @export
#'
#' @examples
#' cas_set_options(
#'   base_folder = fs::path(tempdir(), "R", "castarter_data"),
#'   project = "example_project",
#'   website = "example_website"
#' )
#' cas_enable_db()
#'
#'
#' urls_df <- cas_build_urls(
#'   url = "https://www.example.com/news/",
#'   start_page = 1,
#'   end_page = 10
#' )
#'
#' cas_write_to_db(
#'   df = urls_df,
#'   table = "index_id"
#' )
#'
#' cas_read_from_db(table = "index_id")
cas_read_from_db <- function(table,
                             db_folder = NULL,
                             db_connection = NULL,
                             ...) {
  if (cas_check_use_db(...) == FALSE) {
    usethis::ui_stop("Database not set. Set the database connection with `cas_set_options()` or pass database connection with the parameter `db_connection`.")
  }

  db <- cas_connect_to_db(
    db_connection = db_connection,
    read_only = TRUE,
    ...
  )

  if (DBI::dbExistsTable(conn = db, name = table) == FALSE) {
    # do nothing: if table does not exist, previous data cannot be there
    output_df <- NULL
  } else {
    output_df <- dplyr::tbl(db, table)
  }

  cas_disconnect_from_db(
    db_connection = db,
    ...
  )

  output_df
}
