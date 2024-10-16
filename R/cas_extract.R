#' Extract fields and contents from downloaded files
#'
#' @param extractors A named list of functions. See examples for details.
#' @param post_processing Defaults to NULL. If given, it must be a function that
#'   takes a data frame as input (logically, a row of the dataset) and returns
#'   it with additional or modified columns.
#' @param id Defaults to NULL, identifiers to process when extracting. If given,
#'   must be a numeric vector, logically corresponding to the identifiers in the
#'   `id` column, e.g. as returned by ` cas_read_db_contents_id()`
#' @param ignore_id Defaults to TRUE. If TRUE, it checks if identifiers have
#'   been added to the local ignore list, typically with `cas_ignore_id()`, and
#'   as retrieved with `cas_read_db_ignore_id()`. It can also be a numeric
#'   vector of identifiers: the given identifiers will not be processed. If
#'   FALSE, items will be processed normally.
#' @param store_as_character Logical, defaults to TRUE. If TRUE, it converts to
#'   character all extracted contents before writing them to database. This
#'   reduces issues of type conversions with the default database backend (for
#'   example, SQLite automatically converts dates to numeric) or using different
#'   backends. This implies you will need to set data types when you read the
#'   database, but it also means that you can consistently expect all columns to
#'   be character vectors, which in one form or another are consistently
#'   implemented across database backends. Set to FALSE if you want to remain in
#'   control of column types.
#' @param check_previous Logical, defaults to TRUE. If FALSE, no check will be
#'   conducted to verify if the same content had been previously extracted. If
#'   FALSE, `write_to_db` must be set (or will be set) to FALSE, to prevent
#'   duplication of data.
#' @param keep_if_status Defaults to 200. Keep only if recorded download status
#'   matches the given status.
#' @inheritParams cas_download
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' if (interactive) {
#'   ### Post-processing example ####
#'   # For example, in order to add a column called `internal_id`
#'   # that takes the ending digits of the url (assuming the url ends with digits)
#'   # a function such as the following would be passed to cas_extract
#'   pp <- function(df) {
#'     df |>
#'       dplyr::mutate(internal_id = stringr::str_extract(url, "[[:digit:]]+$"))
#'   }
#' }
#'
#' cas_extract(
#'   extractors = extractors_l, # assuming it has already been set
#'   post_processing = pp
#' )
#' }
cas_extract <- function(extractors,
                        post_processing = NULL,
                        id = NULL,
                        ignore_id = TRUE,
                        custom_path = NULL,
                        index = FALSE,
                        store_as_character = TRUE,
                        check_previous = TRUE,
                        db_connection = NULL,
                        file_format = "html",
                        sample = FALSE,
                        write_to_db = FALSE,
                        keep_if_status = 200,
                        encoding = "UTF-8",
                        readability = FALSE,
                        ...) {
  ellipsis::check_dots_unnamed()

  db <- cas_connect_to_db(
    db_connection = db_connection,
    ...
  )

  available_files_to_extract_df <- cas_get_files_to_extract(
    id = id,
    ignore_id = ignore_id,
    custom_path = custom_path,
    index = index,
    store_as_character = store_as_character,
    check_previous = check_previous,
    db_connection = db,
    file_format = file_format,
    sample = sample,
    keep_if_status = keep_if_status,
    ...
  )

  if (write_to_db == FALSE) {
    cas_disconnect_from_db(
      db_connection = db,
      disconnect_db = TRUE
    )

    db <- duckdb::dbConnect(duckdb::duckdb(), ":memory:")
  }

  purrr::walk(
    .progress = "Extracting",
    .x = purrr::transpose(available_files_to_extract_df),
    function(x) {
      current_html_document <- xml2::read_html(
        x = x$path,
        options = c("RECOVER", "NOERROR", "NOBLANKS", "HUGE"),
        encoding = encoding
      )

      if (inherits(x = current_html_document, what = "xml_node") == FALSE) {
        return(NULL)
      }

      if (isTRUE(readability)) {
        readability_list <- castarter.readability::cas_extract_readability(
          html = as.character(current_html_document),
          url = x$url
        )
        # TODO prevent processing rather than process and drop
        readability_list[[c("content")]] <- NULL
        readability_list[[c("siteName")]] <- NULL
        readability_list[[c("lang")]] <- NULL
        readability_list[[c("dir")]] <- NULL
        readability_list[[c("length")]] <- NULL

        readability_list[purrr::map_lgl(
          .x = readability_list,
          .f = \(x) is.null(x)
        )] <- NA_character_
        current_df <- readability_list |>
          tibble::as_tibble() |>
          dplyr::rename(text = textContent) |>
          dplyr::mutate(
            id = as.numeric(x[["id"]]),
            url = as.character(x[["url"]])
          ) |>
          dplyr::relocate(title, text) |>
          dplyr::select("id", "url", dplyr::everything())
      } else {
        current_df <- names(extractors) %>%
          purrr::set_names() %>%
          purrr::map(.f = function(current_function) {
            current_function <- extractors[[current_function]](current_html_document)
          }) %>%
          tibble::as_tibble() %>%
          dplyr::mutate(
            id = as.numeric(x[["id"]]),
            url = as.character(x[["url"]])
          ) %>%
          dplyr::select("id", "url", dplyr::everything())
      }


      if (is.null(post_processing) == FALSE) {
        if (is.function(post_processing) == FALSE) {
          cli::cli_abort("When given, {.val post_processing} must be a function.")
        }
        current_df <- post_processing(current_df)
      }

      if (store_as_character == TRUE) {
        current_df <- current_df %>%
          dplyr::mutate(dplyr::across(
            .cols = dplyr::everything(),
            .fns = as.character
          ))
      }

      cas_write_to_db(
        df = current_df,
        table = "contents_data",
        db_connection = db,
        disconnect_db = FALSE,
        ...
      )
    }
  )

  if (write_to_db == FALSE) {
    output_df <- cas_read_db_contents_data(
      db_connection = db,
      ...
    )
    return(output_df)
  }
}


#' Extracts scripts from an html page
#'
#' @param script_type Defaults to NULL. Type of script. Common script types
#'   include `application/ld+json`, `text/template`, etc.
#' @param match Default to NULL. If given, used to filter extracted scripts.
#'   Must be a named vector in the format `c(`@type` = "NewsArticle")` for a
#'   script of type "NewsArticle".
#' @param accessors Defaults to NULL. If given, a vector of accessors passed to
#'   `purrr::pluck` in order to extract sub-components of the list resulting
#'   from reading the with `jsonlite` the result of the previous steps and
#'   filter.
#' @param remove_from_script Defaults to NULL. If given, removed after the
#'   script has been extracted but before processing the json.
#'
#' @inheritParams cas_extract_html
#'
#' @return May return a list or a character vector. If no match is found, returns `NA_character_`
#' @export
#'
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   url <- "https://www.digi24.ro/stiri/externe/casa-alba-pune-capat-isteriei-globale-nu-exista-indicii-ca-obiectele-zburatoare-doborate-de-rachetele-sua-ar-fi-extraterestre-2250863"
#'
#'   html_document <- rvest::read_html(x = url)
#'
#'   cas_extract_script(
#'     html_document = html_document,
#'     script_type = "application/ld+json"
#'   )
#'
#'   # get date published
#'   cas_extract_script(
#'     html_document = html_document,
#'     script_type = "application/ld+json",
#'     match = c(`@type` = "NewsArticle"),
#'     accessors = "datePublished"
#'   )
#'
#'   # get title
#'   cas_extract_script(
#'     html_document = html_document,
#'     script_type = "application/ld+json",
#'     match = c(`@type` = "NewsArticle"),
#'     accessors = "headline"
#'   )
#'
#'   # get nested element, e.g. url of the logo of the publisher
#'
#'   cas_extract_script(
#'     html_document = html_document,
#'     script_type = "application/ld+json",
#'     match = c(`@type` = "NewsArticle"),
#'     accessors = c("publisher", "logo", "url")
#'   )
#' }
#' }
cas_extract_script <- function(html_document,
                               script_type = NULL,
                               match = NULL,
                               accessors = NULL,
                               remove_from_script = NULL) {
  if (is.null(script_type) == TRUE) {
    script_pre <- html_document |>
      rvest::html_elements("script")
  } else {
    script_pre <- html_document |>
      rvest::html_elements(stringr::str_c("script[type='", script_type, "']"))
  }

  script_l <- purrr::map(
    .x = script_pre,
    .f = function(x) {
      if (is.null(remove_from_script)) {
        x |>
          rvest::html_text2() |>
          stringr::str_conv(encoding = "UTF-8") |>
          stringr::str_remove_all(stringr::fixed("\\")) |>
          jsonlite::parse_json()
      } else {
        x %>%
          rvest::html_text2() %>%
          stringr::str_remove_all(pattern = remove_from_script) |>
          jsonlite::parse_json()
      }
    }
  )

  if (is.null(match) == FALSE) {
    matched_pre <- purrr::map_chr(
      .x = script_l,
      .f = function(x) {
        x %>%
          purrr::pluck(names(match))
      }
    )

    match_index_v <- which(matched_pre == match)

    if (length(match_index_v) == 0) {
      return(NA_character_)
    } else {
      script_l <- script_l[[match_index_v]]
    }
  }

  if (is.null(accessors) == FALSE) {
    script_l %>%
      purrr::pluck(!!!accessors)
  } else {
    script_l
  }
}
