#' Extract fields and contents from downloaded files
#'
#' @param extractors A named list of functions. See examples for details.
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
cas_extract <- function(extractors,
                        id = NULL,
                        index = FALSE,
                        store_as_character = TRUE,
                        check_previous = TRUE,
                        db_connection = NULL,
                        file_format = "html",
                        sample = FALSE,
                        write_to_db = TRUE,
                        keep_if_status = 200,
                        encoding = "UTF-8",
                        ...) {
  ellipsis::check_dots_unnamed()

  db <- cas_connect_to_db(
    db_connection = db_connection,
    ...
  )

  path <- cas_get_base_path(...)

  previous_download_df <- cas_read_db_download(
    index = index,
    db_connection = db,
    disconnect_db = FALSE,
    ...
  ) %>%
    dplyr::arrange(dplyr::desc(datetime)) %>%
    dplyr::distinct(id, .keep_all = TRUE) %>%
    dplyr::arrange(id, batch, datetime) %>%
    dplyr::filter(status %in% keep_if_status)

  stored_files_df <- previous_download_df %>%
    dplyr::select("id", "batch") %>%
    dplyr::mutate(path = fs::path(
      path,
      batch,
      stringr::str_c(id, "_", batch, ".", file_format)
    )) %>%
    dplyr::select("id", "path")

  if (store_as_character == TRUE) {
    stored_files_df <- stored_files_df %>%
      dplyr::mutate(id = as.character(id))
  }


  if (check_previous == FALSE) {
    files_to_extract_pre_df <- stored_files_df
    write_to_db <- FALSE
  } else {
    # Do not process previously extracted
    previously_extracted_df <- cas_read_db_contents_data(
      db_connection = db,
      disconnect_db = FALSE,
      ...
    )

    if (is.null(previously_extracted_df) == FALSE) {
      previously_extracted_df <- previously_extracted_df %>%
        dplyr::select(id) %>%
        dplyr::collect()

      if (store_as_character == TRUE) {
        previously_extracted_df <- previously_extracted_df %>%
          dplyr::mutate(id = as.character(id))
      }
    }

    if (is.null(previously_extracted_df) == FALSE) {
      files_to_extract_pre_df <- dplyr::anti_join(
        x = stored_files_df,
        y = previously_extracted_df,
        by = "id"
      )
    } else {
      files_to_extract_pre_df <- stored_files_df
    }
  }


  if (nrow(files_to_extract_pre_df) == 0) {
    # TODO return consistently data frame or S3 object
    return(invisible(NULL))
  }

  contents_id_df <- cas_read_db_contents_id(
    db_connection = db,
    disconnect_db = FALSE,
    ...
  ) %>%
    dplyr::collect()

  if (store_as_character == TRUE) {
    contents_id_df <- contents_id_df %>%
      dplyr::mutate(id = as.character(id))
  }

  files_to_extract_df <- files_to_extract_pre_df %>%
    dplyr::left_join(
      y = contents_id_df,
      by = "id"
    )

  if (is.null(id) == FALSE) {
    id_to_keep <- id
    files_to_extract_df <- files_to_extract_df %>%
      dplyr::filter(id %in% id_to_keep)
  }

  if (is.numeric(sample) == TRUE) {
    if (sample > nrow(files_to_extract_df)) {
      sample <- nrow(files_to_extract_df)
    }

    files_to_extract_df <- files_to_extract_df %>%
      dplyr::slice_sample(n = sample)
  } else if (isTRUE(sample)) {
    files_to_extract_df <- files_to_extract_df %>%
      dplyr::slice_sample(p = 1)
  }

  if (write_to_db == FALSE) {
    cas_disconnect_from_db(
      db_connection = db,
      ...
    )

    db <- duckdb::dbConnect(duckdb::duckdb(), ":memory:")
  }

  pb <- progress::progress_bar$new(total = nrow(files_to_extract_df))

  purrr::walk(
    .x = purrr::transpose(files_to_extract_df),
    function(x) {
      pb$tick()

      current_html_document <- xml2::read_html(
        x = x$path,
        options = c("RECOVER", "NOERROR", "NOBLANKS", "HUGE"),
        encoding = encoding
      )

      if (inherits(x = current_html_document, what = "xml_node") == FALSE) {
        return(NULL)
      }

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


#' Facilitates extraction of contents from an html file
#'
#' @param html_document An html document parsed with `xml2::read_html()`.
#' @param container Defaults to NULL. Type of html container from where links
#'   are to be extracted, such as "div", "ul", and others. Either
#'   `container_class` or `container_id` must also be provided.
#' @param container_class Defaults to NULL. If provided, also `container` must
#'   be given (and `container_id` must be NULL). Only text found inside the
#'   provided combination of container/class will be extracted.
#' @param container_id Defaults to NULL. If provided, also `container` must be
#'   given (and `container_id` must be NULL). Only text found inside the
#'   provided combination of container/class will be extracted.
#' @param container_itemprop Defaults to NULL. If provided, also `container`
#'   must be given (and `container_id` and `container_class` must be NULL or
#'   will be silently ignored). Only text found inside the provided combination
#'   of container/itemprop will be extracted.
#' @param container_instance Defaults to NULL. If given, it must be an integer.
#'   If a given combination is found more than once in the same page, the
#'   relevant occurrence is kept. Use with caution, as not all pages always
#'   include the same number of elements of the same class/with the same id.
#' @param sub_element Defaults to NULL. If provided, also `container` must be
#'   given. Only text within elements of given type under the chosen combination
#'   of container/containerClass will be extracted. When given, it will
#'   tipically be "p", to extract all p elements inside the selected div.
#' @param no_children Defaults to FALSE, i.e. by default all subelements of the
#'   selected combination (e.g. div with given class) are extracted. If TRUE,
#'   only text found under the given combination (but not its subelements) will
#'   be extracted. Corresponds to the xpath string `/node()[not(self::div)]`.
#' @param attribute Defaults to NULL. If given, type of attribute to extract.
#'   Typically used in combination with container, as in
#'   `cas_extract_html(container = "time", attribute = "datetime")`.
#' @param custom_Xpath Defaults to NULL. If given, all other parameters are
#'   ignored and given Xpath used instead.
#' @param custom_CSSpath Defaults to NULL. If given, all other parameters are
#'   ignored and given CSSpath used instead.
#' @param keep_everything Defaults to FALSE. If TRUE, all text included in the
#'   page is returned as a single string.
#' @param trim Defaults to TRUE. If TRUE, applies `stringr::str_trim()` to
#'   output, removing whitespace from start and end of string.
#' @param squish Defaults to FALSE. If TRUE, applies `stringr::str_squish()` to
#'   output, removing whitespace from start and end of string, and replacing
#'   any whitespace (including new lines) with a single space.
#' @param no_match Defaults to "". A common alternative would be NA. Value to
#'   return when the given container, selector or element is not found.
#'
#' @return A character vector of length one.
#' @export
#'
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   url <- "https://example.com"
#'   html_document <- rvest::read_html(x = url)
#'
#'   # example for a tag that looks like:
#'   # <meta name="twitter:title" content="Example title" />
#'
#'   cas_extract_html(
#'     html_document = html_document,
#'     container = "meta",
#'     container_name = "twitter:title",
#'     attribute = "content"
#'   )
#'
#'
#'   # example for a tag that looks like:
#'   # <meta name="keywords" content="various;keywords;">
#'   cas_extract_html(
#'     html_document = html_document,
#'     container = "meta",
#'     container_name = "keywords",
#'     attribute = "content"
#'   )
#'
#'   # example for a tag that looks like:
#'   # <meta property="article:published_time" content="2016-10-29T13:09+03:00"/>
#'   cas_extract_html(
#'     html_document = html_document,
#'     container = "meta",
#'     container_property = "article:published_time",
#'     attribute = "content"
#'   )
#' }
#' }
cas_extract_html <- function(html_document,
                             container = NULL,
                             container_class = NULL,
                             container_id = NULL,
                             container_name = NULL,
                             container_property = NULL,
                             container_itemprop = NULL,
                             container_instance = NULL,
                             attribute = NULL,
                             sub_element = NULL,
                             no_children = NULL,
                             trim = TRUE,
                             squish = FALSE,
                             no_match = "",
                             custom_Xpath = NULL,
                             custom_CSSpath = NULL,
                             keep_everything = FALSE) {
  if (keep_everything == TRUE) {
    output <- html_document %>%
      rvest::html_text2()
  } else if (is.null(custom_Xpath) == FALSE) {
    nodes <- html_document %>%
      rvest::html_nodes(xpath = custom_Xpath)
    if (is.null(sub_element) == TRUE) {
      output <- nodes %>%
        rvest::html_nodes(sub_element) %>%
        rvest::html_text2()
    } else {
      output <- output %>%
        rvest::html_nodes(sub_element) %>%
        rvest::html_text2()
    }
  } else if (is.null(custom_CSSpath) == FALSE) {
    nodes <- html_document %>%
      rvest::html_nodes(css = custom_CSSpath)
    if (is.null(sub_element) == TRUE) {
      output <- nodes %>%
        rvest::html_nodes(sub_element) %>%
        rvest::html_text2()
    } else {
      output <- nodes %>%
        rvest::html_nodes(sub_element) %>%
        rvest::html_text2()
    }
  } else if (is.null(container_itemprop) == FALSE) {
    if (is.null(attribute)) {
      output <- html_document %>%
        rvest::html_nodes(xpath = stringr::str_c(
          "//",
          container,
          "[@itemprop='",
          container_itemprop, "']"
        )) %>%
        rvest::html_text2()
    } else {
      output <- html_document %>%
        rvest::html_nodes(xpath = stringr::str_c(
          "//",
          container,
          "[@itemprop='",
          container_itemprop, "']"
        )) %>%
        rvest::html_attr(name = attribute)
    }
  } else if (is.null(container_class) == TRUE & is.null(container_id) == TRUE) {
    if (is.null(sub_element) == TRUE) {
      if (is.null(container_name) == TRUE) {
        if (is.null(container_property)) {
          output <- html_document %>%
            rvest::html_nodes(container) %>%
            rvest::html_text2()
        } else {
          output <- html_document %>%
            rvest::html_nodes(xpath = stringr::str_c(
              "//",
              container,
              "[@property='",
              container_property, "']"
            )) %>%
            rvest::html_attr(name = attribute)
        }
      } else {
        if (is.null(attribute)) {
          output <- html_document %>%
            rvest::html_nodes(xpath = stringr::str_c(
              "//",
              container,
              "[@name='",
              container_name, "']"
            ))
        } else {
          output <- html_document %>%
            rvest::html_nodes(xpath = stringr::str_c(
              "//",
              container,
              "[@name='",
              container_name, "']"
            )) %>%
            rvest::html_attr(name = attribute)
        }
      }
    } else {
      output <- html_document %>%
        rvest::html_nodes(container) %>%
        rvest::html_nodes(sub_element) %>%
        rvest::html_text2()
    }
  } else if (is.null(container_class) == FALSE & is.null(attribute) == FALSE) {
    if (is.null(container_name)) {
      output <- html_document %>%
        rvest::html_nodes(xpath = stringr::str_c(
          "//",
          container
        )) %>%
        rvest::html_attr(name = attribute)
    } else {
      output <- html_document %>%
        rvest::html_nodes(xpath = stringr::str_c(
          "//",
          container,
          "[@name='",
          container_name, "']"
        )) %>%
        rvest::html_attr(name = attribute)
    }
  } else if (is.null(container_class) == FALSE & is.null(container_id) == TRUE) {
    if (is.null(sub_element) == TRUE) {
      output <- html_document %>%
        rvest::html_nodes(xpath = stringr::str_c(
          "//",
          container,
          "[@class='",
          container_class, "']"
        )) %>%
        rvest::html_text2()
    } else {
      output <- html_document %>%
        rvest::html_nodes(xpath = stringr::str_c(
          "//",
          container,
          "[@class='",
          container_class,
          "']"
        )) %>%
        rvest::html_nodes(sub_element) %>%
        rvest::html_text2() %>%
        stringr::str_c(collapse = "\n")
    }
  } else if (is.null(container_class) == TRUE & is.null(container_id) == FALSE) {
    if (is.null(sub_element) == TRUE) {
      output <- html_document %>%
        rvest::html_nodes(xpath = stringr::str_c("//", container, "[@id='", container_id, "']")) %>%
        rvest::html_text2()
    } else {
      output <- html_document %>%
        rvest::html_nodes(xpath = stringr::str_c("//", container, "[@id='", container_id, "']")) %>%
        rvest::html_nodes(sub_element) %>%
        rvest::html_text2()
    }
  }


  if (length(output) > 1) {
    if (is.null(container_instance) == FALSE) {
      output <- output[container_instance]
    } else {
      output <- stringr::str_c(output, collapse = "\n")
    }
  } else if (length(output) == 0) {
    output <- as.character(no_match)
  }

  if (trim == TRUE) {
    output <- stringr::str_trim(string = output)
  }

  if (squish == TRUE) {
    output <- stringr::str_squish(string = output)
  }

  output
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
                               accessors = NULL) {
  if (is.null(script_type) == TRUE) {
    script_pre <- html_document %>%
      rvest::html_elements("script")
  } else {
    script_pre <- html_document %>%
      rvest::html_elements(stringr::str_c("script[type='", script_type, "']"))
  }

  script_l <- purrr::map(
    .x = script_pre,
    .f = function(x) {
      x %>%
        rvest::html_text2() %>%
        stringr::str_remove_all(pattern = stringr::fixed("\\")) %>%
        stringr::str_remove_all(pattern = stringr::fixed("\r")) %>%
        jsonlite::parse_json()
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
