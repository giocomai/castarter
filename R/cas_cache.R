#' Creates the base cache folder where `castarter` caches data.
#'
#' @param ask Logical, defaults to TRUE. If FALSE, and cache folder does not exist, it just creates it without asking (useful for non-interactive sessions).
#'
#' @return Nothing, used for its side effects.
#' @export
#'
#' @examples
#' \donttest{
#' if (interactive()) {
#'   cas_create_cache_folder()
#' }
#' }
cas_create_cache_folder <- function(ask = TRUE) {
  if (fs::file_exists(castarter::cas_get_cache_folder()) == FALSE) {
    if (ask == FALSE) {
      fs::dir_create(path = castarter::cas_get_cache_folder(), recurse = TRUE)
    } else {
      usethis::ui_info(glue::glue("The cache folder {{usethis::ui_path(cas_get_cache_folder())}} does not exist. If you prefer to cache files elsewhere, reply negatively and set your preferred cache folder with `cas_set_cache_folder()`"))
      check <- usethis::ui_yeah(glue::glue("Do you want to create {{usethis::ui_path(cas_get_cache_folder())}} for caching data?"))
      if (check == TRUE) {
        fs::dir_create(path = castarter::cas_get_cache_folder(), recurse = TRUE)
      }
    }
    if (fs::file_exists(castarter::cas_get_cache_folder()) == FALSE) {
      usethis::ui_stop("This function requires a valid cache folder.")
    }
  }
}


#' Set folder for caching data
#'
#' Consider using a folder out of your current project directory, e.g. `cas_set_cache_folder("~/R/cas_data/")`: you will be able to use the same cache in different projects, and prevent cached files from being sync-ed if you use services such as Nextcloud or Dropbox.
#'
#' @param path A path to a location used for caching data. If the folder does not exist, it will be created.
#'
#' @return The path to the caching folder, if previously set; the same path as given to the function; or the default, `cas_data` is none is given.
#' @export

#' @examples
#' \donttest{
#' if (interactive()) {
#'   cas_set_cache_folder(fs::path(fs::path_home_r(), "R", "cas_data"))
#' }
#' }
cas_set_cache_folder <- function(path = NULL) {
  if (is.null(path)) {
    path <- Sys.getenv("castarter_cache_folder")
  } else {
    Sys.setenv(castarter_cache_folder = path)
  }
  if (path == "") {
    path <- fs::path("castarter_data")
  }
  invisible(path)
}

#' @rdname cas_set_cache_folder
#' @examples
#' cas_get_cache_folder()
#' @export
cas_get_cache_folder <- cas_set_cache_folder



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
#' @return A list with all given parameters (invisibly).
#' @export
#'
#' @examples
#' \donttest{
#' if (interactive()) {
#'
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
#'   cas_set_cache_db(db_settings)
#'
#'   # or as parameters
#'
#'   cas_set_cache_db(
#'     driver = "MySQL",
#'     host = "localhost",
#'     port = 3306,
#'     database = "castarter",
#'     user = "secret_username",
#'     pwd = "secret_password"
#'   )
#' }
#' }
cas_set_cache_db <- function(db_settings = NULL,
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
#' Typically set with `cas_set_cache_db()`
#'
#' @return A list with all database parameters as stored in environment variables.
#' @export
#'
#' @examples
#'
#' cas_get_cache_db()
cas_get_cache_db <- function() {
  list(
    driver = Sys.getenv("castarter_db_driver"),
    host = Sys.getenv("castarter_db_host"),
    port = Sys.getenv("castarter_db_port"),
    database = Sys.getenv("castarter_db_database"),
    user = Sys.getenv("castarter_db_user"),
    pwd = Sys.getenv("castarter_db_pwd")
  )
}


#' Enable caching for the current session
#'
#' @param SQLite Logical, defaults to TRUE. Set to FALSE to use custom database options. See `cas_set_cache_db()` for details.
#'
#' @return Nothing, used for its side effects.
#' @export
#' @examples
#' \donttest{
#' if (interactive()) {
#'   cas_enable_cache()
#' }
#' }
cas_enable_cache <- function(SQLite = TRUE) {
  Sys.setenv(castarter_cache = TRUE)
  Sys.setenv(castarter_cache_SQLite = SQLite)
}


#' Disable caching for the current session
#'
#' @return Nothing, used for its side effects.
#' @export

#' @examples
#' \donttest{
#' if (interactive()) {
#'   cas_disable_cache()
#' }
#' }
cas_disable_cache <- function() {
  Sys.setenv(castarter_cache = FALSE)
}

#' Check caching status in the current session, and override it upon request
#'
#' Mostly used internally in functions, exported for reference.
#'
#' @param cache Defaults to NULL. If NULL, checks current cache settings. If given, returns given value, ignoring cache.
#'
#' @return Either TRUE or FALSE, depending on current cache settings.
#' @export

#' @examples
#' \donttest{
#' if (interactive()) {
#'   cas_check_cache()
#' }
#' }
cas_check_cache <- function(cache = NULL) {
  if (is.null(cache) == FALSE) {
    return(as.logical(cache))
  }
  current_cache <- Sys.getenv("castarter_cache")
  if (current_cache == "") {
    as.logical(FALSE)
  } else {
    as.logical(current_cache)
  }
}

#' Checks if cache folder exists, if not returns an informative message
#'
#' @return If the cache folder exists, returns TRUE. Otherwise throws an error.
#' @export
#'
#' @examples
#'
#' # If cache folder does not exist, it throws an error
#' tryCatch(cas_check_cache_folder(),
#'   error = function(e) {
#'     return(e)
#'   }
#' )
#'
#' # Create cache folder
#' cas_set_cache_folder(path = fs::path(
#'   tempdir(),
#'   "cas_cache_folder"
#' ))
#' cas_create_cache_folder(ask = FALSE)
#'
#' cas_check_cache_folder()
cas_check_cache_folder <- function() {
  if (fs::file_exists(cas_get_cache_folder()) == FALSE) {
    usethis::ui_stop(paste(
      "Cache folder does not exist. Set it with",
      usethis::ui_code("cas_get_cache_folder()"),
      "and create it with",
      usethis::ui_code("cas_create_cache_folder()")
    ))
  }
  TRUE
}



#' Return a connection to be used for caching
#'
#' @param connection Defaults to NULL. If NULL, uses local SQLite database. If given, must be a connection object or a list with relevant connection settings (see example).
#' @param RSQLite Defaults to NULL, expected either NULL or logical. If set to `FALSE`, details on the database connection must be given either as a named list in the connection parameter, or with `cas_set_cache_db()` as environment variables.
#' @param language Defaults to language set with `cas_set_language()`; if not set, "en". Use "all_available" to keep all languages. For available language values, see https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all
#' @param cache Defaults to NULL. If given, it should be given either TRUE or FALSE. Typically set with `cas_enable_cache()` or `cas_disable_cache()`.
#'
#' @return A connection object.
#' @export
#'
#' @examples
#' \donttest{
#' if (interactive()) {
#'   cache_connection <- pool::dbPool(
#'     RSQLite::SQLite(), # or e.g. odbc::odbc(),
#'     Driver =  ":memory:", # or e.g. "MariaDB",
#'     Host = "localhost",
#'     database = "example_db",
#'     UID = "example_user",
#'     PWD = "example_pwd"
#'   )
#'   cas_connect_to_cache(cache_connection)
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
#'   cas_connect_to_cache(db_settings)
#' }
#' }
#'
cas_connect_to_cache <- function(connection = NULL,
                                 RSQLite = NULL,
                                 language = NULL,
                                 cache = NULL) {
  if (isFALSE(x = cas_check_cache(cache))) {
    return(NULL)
  }

  if (is.null(connection) == FALSE & is.list(connection) == FALSE) {
    if (DBI::dbIsValid(connection) == FALSE) {
      connection <- NULL
    }
  }

  if (is.null(connection)) {
    if (is.null(language) == FALSE) {
      language <- cas_get_language()
    }

    if (is.null(RSQLite)) {
      RSQLite <- as.logical(Sys.getenv(x = "cas_cache_SQLite", unset = TRUE))
    }

    if (isTRUE(RSQLite)) {
      cas_check_cache_folder()
      db_file <- cas_get_cache_file(
        language = language
      )

      if (fs::file_exists(db_file) == FALSE) {
        db <- DBI::dbConnect(
          drv = RSQLite::SQLite(),
          db_file
        )
      }

      db <- pool::dbPool(
        drv = RSQLite::SQLite(),
        dbname = db_file
      )
      return(db)
    } else {
      connection <- cas_get_cache_db()

      if (connection[["driver"]] == "SQLite") {
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

      db <- pool::dbPool(
        drv = drv,
        driver = connection[["driver"]],
        host = connection[["host"]],
        port = as.integer(connection[["port"]]),
        database = connection[["database"]],
        user = connection[["user"]],
        pwd = connection[["pwd"]]
      )
      return(db)
    }
  } else {
    if (is.list(connection)) {
      if (connection[["driver"]] == "SQLite") {
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

      db <- pool::dbPool(
        drv = drv,
        driver = connection[["driver"]],
        host = connection[["host"]],
        port = as.integer(connection[["port"]]),
        database = connection[["database"]],
        dbname = connection[["database"]],
        user = connection[["user"]],
        pwd = connection[["pwd"]]
      )
      return(db)
    } else {
      return(connection)
    }
  }
}




#' Ensure that connection to cache is disconnected consistently
#'
#' @param cache Defaults to NULL. If given, it should be given either TRUE or FALSE. Typically set with `cas_enable_cache()` or `cas_disable_cache()`.
#' @param cache_connection Defaults to NULL. If NULL, and caching is enabled, `castarter` will use a local sqlite database. A custom connection to other databases can be given (see vignette `caching` for details).
#' @param disconnect_db Defaults to TRUE. If FALSE, leaves the connection to cache open.
#' @param language Defaults to language set with `cas_set_language()`; if not set, "en". Use "all_available" to keep all languages. For available language values, see https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all
#'
#' @return Nothing, used for its side effects.
#' @export
#'
#' @examples
#' if (interactive()) {
#'   cas_get(
#'     id = c("Q180099"),
#'     language = "en"
#'   )
#'   cas_disconnect_from_cache()
#' }
cas_disconnect_from_cache <- function(cache = NULL,
                                      cache_connection = NULL,
                                      disconnect_db = TRUE,
                                      language = castarter::cas_get_language()) {
  if (isFALSE(disconnect_db)) {
    return(invisible(NULL))
  }

  if (isTRUE(cas_check_cache(cache))) {
    db <- cas_connect_to_cache(
      connection = cache_connection,
      language = language,
      cache = cache
    )

    if (pool::dbIsValid(dbObj = db)) {
      if ("Pool" %in% class(db)) {
        pool::poolClose(db)
      } else {
        DBI::dbDisconnect(db)
      }
    }
  }
}
