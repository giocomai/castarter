#' Extract fields and contents from downloaded files
#'
#' @param extractors A named list of functions. See examples for details.
#' @inheritParams cas_download
#'
#' @return
#' @export
#'
#' @examples
cas_extract <- function(extractors,
                        id = NULL,
                        index = FALSE,
                        db_connection = NULL,
                        file_format = "html",
                        random = FALSE,
                        write_to_db = TRUE,
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
    dplyr::arrange(id, batch, datetime)

  stored_files_df <- previous_download_df %>%
    dplyr::select("id", "batch") %>%
    dplyr::mutate(path = fs::path(
      path,
      batch,
      stringr::str_c(id, "_", batch, ".", file_format)
    )) %>%
    dplyr::select("id", "path")


  # Do not process previously extracted
  previously_extracted_df <- cas_read_db_contents_data(
    db_connection = db,
    disconnect_db = FALSE,
    ...
  )

  if (is.null(previously_extracted_df) == FALSE) {
    files_to_extract_pre_df <- dplyr::anti_join(
      x = stored_files_df,
      y = previously_extracted_df,
      by = "id"
    )
  } else {
    files_to_extract_pre_df <- stored_files_df
  }

  if (nrow(files_to_extract_pre_df) == 0) {
    # TODO return consistently data frame or S3 object
    return(invisible(NULL))
  }

  files_to_extract_df <- files_to_extract_pre_df %>%
    dplyr::left_join(
      y = cas_read_db_contents_id(
        db_connection = db,
        disconnect_db = FALSE,
        ...
      ),
      by = "id",
      copy = TRUE
    )

  if (is.null(id) == FALSE) {
    id_to_keep <- id
    files_to_extract_df <- files_to_extract_df %>%
      dplyr::filter(id %in% id_to_keep)
  }

  if (is.numeric(random) == TRUE) {
    if (random > nrow(files_to_extract_df)) {
      random <- nrow(files_to_extract_df)
    }

    files_to_extract_df <- files_to_extract_df %>%
      dplyr::slice_sample(n = random)
  } else if (isTRUE(random)) {
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
        options = c("RECOVER", "NOERROR", "NOBLANKS", "HUGE")
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
#' @param custom_Xpath Defaults to NULL. If given, all other parameters are
#'   ignored and given Xpath used instead.
#' @param custom_CSSpath Defaults to NULL. If given, all other parameters are
#'   ignored and given CSSpath used instead.
#' @param keep_everything Defaults to FALSE. If TRUE, all text included in the
#'   page is returned as a single string.
#' @param squish Defaults to TRUE. If TRUE, applies `stringr::str_squish()` to
#'   output, removing whitespace from start and end of string, and replacing
#'   repeated whitespace with a single space.
#'
#' @return A character vector of length one.
#' @export
#'
#' @examples
cas_extract_html <- function(html_document,
                             container = NULL,
                             container_class = NULL,
                             container_id = NULL,
                             container_instance = NULL,
                             sub_element = NULL,
                             no_children = NULL,
                             squish = TRUE,
                             encoding = "UTF-8",
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
  } else if (is.null(container_class) == TRUE & is.null(container_id) == TRUE) {
    if (is.null(sub_element) == TRUE) {
      output <- html_document %>%
        rvest::html_nodes(container) %>%
        rvest::html_text2()
    } else {
      output <- html_document %>%
        rvest::html_nodes(container) %>%
        rvest::html_nodes(sub_element) %>%
        rvest::html_text2()
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
    output <- as.character(NA)
  }

  if (squish == TRUE) {
    output <- stringr::str_squish(string = output)
  }
  output
}
