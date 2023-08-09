#' Extract direct links to individual content pages from index pages
#'
#' @param domain Defaults to "". Web domain of the website. It is added at the
#'   beginning of each link found. If links in the page already include the full
#'   web address this should be ignored.
#' @param id Defaults to NULL. If provided, it should be a vector of integers.
#'   Only html files corresponding to given id will be processed.
#' @param include_when Part of URL found only in links of individual articles to
#'   be downloaded. If more than one provided, it includes all links that
#'   contains either of the strings provided.
#' @param exclude_when If an URL includes this string, it is excluded from the
#'   output. One or more strings may be provided.
#'
#' @inheritParams cas_write_db_index
#' @inheritParams cas_extract_html
#'
#' @param match Defaults to NULL. Used when extracting json files. Name of
#'   property from where url is to be extracted. N.B. Only partly implemented,
#'   please report issues along with specific example where it emerged.
#' @param output_index Defaults to FALSE. If FALSE, new links are added to the
#'   contents table. If TRUE, the links extracted will be stored again as
#'   index, using `output_index_group` as `index_group`.
#' @param output_index_group Defaults to NULL. Relevant only when `output_index`
#'   is set to TRUE. Used to store new index urls in the database with reference
#'   to the appropriate group.
#' @param attribute_type Defaults to "href". Type of attribute to extract from
#'   links.
#' @param min_length If a link is shorter than the number of characters given in
#'   min_length, it is excluded from the output.
#' @param max_length If a link is longer than the number of characters given in
#'   max_length, it is excluded from the output.
#' @param append_string If provided, appends given string to the extracted
#'   articles. Typically used to create links for print or mobile versions of
#'   the extracted page.
#' @param remove_string If provided, remove given string (or strings) from
#'   links.
#' @param reverse_order Logical, defaults to FALSE. If TRUE, index files are
#'   processed in reverse order of `id` and `batch`, which may give more
#'   meaningful order to content id. The difference is ultimately cosmetic, and
#'   has no substantive impact either way.
#' @param keep_only_within_domain Logical, defaults to TRUE. If TRUE, and domain
#'   given, links to external websites are dropped.
#' @param check_previous Defaults to TRUE. If TRUE, checks if newly found links
#'   are previously stored in database, and if they are, it discards them. If
#'   FALSE, and `write_to_db` is also set to FALSE, it does not check for
#'   previously stored links.
#' @return A data frame.
#' @export
#' @examples
#' \dontrun{
#' links <- cas_extract_links(domain = "http://www.example.com/")
#' }
cas_extract_links <- function(id = NULL,
                              batch = "latest",
                              domain = NULL,
                              index = TRUE,
                              index_group = NULL,
                              output_index = FALSE,
                              output_index_group = NULL,
                              include_when = NULL,
                              exclude_when = NULL,
                              container = NULL,
                              container_class = NULL,
                              container_id = NULL,
                              custom_xpath = NULL,
                              custom_css = NULL,
                              match = NULL,
                              min_length = NULL,
                              max_length = NULL,
                              attribute_type = "href",
                              append_string = NULL,
                              remove_string = NULL,
                              write_to_db = TRUE,
                              file_format = "html",
                              keep_only_within_domain = TRUE,
                              sample = FALSE,
                              check_previous = TRUE,
                              encoding = "UTF-8",
                              reverse_order = FALSE,
                              db_connection = NULL,
                              disconnect_db = FALSE,
                              ...) {
  if (is.null(domain) == FALSE) {
    if (domain == "" | is.na(domain) == TRUE) {
      domain <- NULL
    }
  }

  db <- cas_connect_to_db(
    db_connection = db_connection,
    ...
  )

  local_files_df <- cas_get_path_to_files(
    id = id,
    batch = batch,
    index = index,
    db_connection = db,
    file_format = file_format,
    ...
  ) %>%
    dplyr::collect()

  if (sum(local_files_df$available) < nrow(local_files_df)) {
    cli::cli_warn(message = "Missing files: {nrow(local_files_df %>% dplyr::filter(!available))}")

    local_files_df <- local_files_df %>%
      dplyr::filter(available) %>%
      dplyr::select(-"available")

    if (nrow(local_files_df) == 0) {
      return(NULL)
    } else {
      cli::cli_inform(c(i = "Links will be extracted from the {nrow(local_files_df)} files available."))
    }
  } else {
    local_files_df <- local_files_df %>%
      dplyr::select(-"available")
  }

  if (check_previous == FALSE & write_to_db == FALSE) {
    if (output_index == TRUE) {
      previous_links_df <- casdb_empty_index_id
    } else {
      previous_links_df <- casdb_empty_contents_id
    }
  } else {
    if (output_index == TRUE) {
      previous_links_df <- cas_read_db_index(
        index_group = output_index_group,
        db_connection = db,
        disconnect_db = FALSE,
        ...
      ) %>%
        dplyr::collect()
    } else {
      previous_links_df <- cas_read_db_contents_id(
        db_connection = db,
        disconnect_db = FALSE,
        ...
      ) %>%
        dplyr::collect()
    }
  }


  if (nrow(previous_links_df) == 0) {
    start_id <- 1
  } else {
    start_id <- sum(1, max(previous_links_df$id))
  }

  if (output_index == TRUE) {
    # do nothing, as source is not kept for index urls
  } else {
    local_files_df <- local_files_df %>%
      dplyr::anti_join(
        y = previous_links_df %>%
          dplyr::select(-"id") %>%
          dplyr::rename(
            id = source_index_id,
            batch = source_index_batch
          ),
        by = c(
          "id",
          "batch"
        )
      )
  }

  if (is.null(index_group) == FALSE) {
    previous_index_links_df <- cas_read_db_index(
      db_connection = db,
      disconnect_db = FALSE,
      ...
    ) %>%
      dplyr::collect()

    local_files_df <- local_files_df %>%
      dplyr::left_join(
        y = previous_index_links_df %>%
          dplyr::select("id", "index_group"),
        by = "id"
      )

    local_files_df <- local_files_df %>%
      dplyr::filter(index_group %in% {{ index_group }})
  }


  if (is.numeric(sample) == TRUE) {
    local_files_df <- local_files_df %>%
      dplyr::slice_sample(n = sample)
  } else if (isTRUE(sample)) {
    local_files_df <- local_files_df %>%
      dplyr::slice_sample(p = 1)
  } else {
    if (reverse_order == TRUE) {
      local_files_df <- local_files_df %>%
        dplyr::arrange(dplyr::desc(id), dplyr::desc(batch))
    } else {
      local_files_df <- local_files_df %>%
        dplyr::arrange(id, batch)
    }
  }

  if (nrow(local_files_df) == 0) {
    cas_disconnect_from_db(
      db_connection = db,
      disconnect_db = disconnect_db
    )
    return(invisible(NULL))
  }

  # if (write_to_db == FALSE) {
  #   cas_disconnect_from_db(
  #     db_connection = db,
  #     ...
  #   )
  #   db <- duckdb::dbConnect(duckdb::duckdb(), ":memory:")
  # }

  pb <- progress::progress_bar$new(total = nrow(local_files_df))

  new_links_df <- purrr::reduce(
    .x = purrr::transpose(local_files_df),
    .init = start_id,
    .f = function(new_links_df, x) {
      pb$tick()

      if (is.data.frame(new_links_df)) {
        start_id <- sum(max(new_links_df[["id"]]), 1)
      } else {
        start_id <- new_links_df
        if (output_index == TRUE) {
          new_links_df <- casdb_empty_index_id
        } else {
          new_links_df <- casdb_empty_contents_id
        }
      }

      if (file_format == "json") {
        temp <- jsonlite::read_json(path = x$path)

        if (is.null(match) == FALSE) {
          if (is.null(names(match))) {
            matched_pre <- purrr::map_chr(
              .x = temp,
              .f = function(x) {
                x %>%
                  purrr::pluck(match)
              }
            )
          } else {
            matched_pre <- purrr::map_chr(
              .x = temp,
              .f = function(x) {
                x %>%
                  purrr::pluck(names(match))
              }
            )
          }
          links_df <- tibble::tibble(
            url = matched_pre,
            link_text = NA_character_
          )
        } else {
          cli::cli_abort(message = "Parameter {.code match} must be given when file format is set to {.code json}")
        }
      } else {
        # effectively, expect html or xml
        temp <- xml2::read_html(
          x = x$path,
          options = c("RECOVER", "NOERROR", "NOBLANKS", "HUGE"),
          encoding = encoding
        )

        if (inherits(x = temp, what = "xml_node") == FALSE) {
          return(NULL)
        }

        if (is.null(custom_xpath) == FALSE) {
          a_xml_nodeset <- temp %>%
            rvest::html_elements(xpath = custom_xpath)
        } else if (is.null(custom_css) == FALSE) {
          a_xml_nodeset <- temp %>%
            rvest::html_elements(css = custom_css)
        } else if (is.null(container)) {
          a_xml_nodeset <- temp %>%
            rvest::html_elements("a")
        } else if (is.null(container_id) == TRUE & is.null(container_class) == FALSE) {
          a_xml_nodeset <- temp %>%
            rvest::html_elements(xpath = paste0("//", container, "[@class='", container_class, "']//a"))
        } else if (is.null(container_class) == TRUE & is.null(container_id) == FALSE) {
          a_xml_nodeset <- temp %>%
            rvest::html_elements(xpath = paste0("//", container, "[@id='", container_id, "']//a"))
        } else if (is.null(container_class) & is.null(container_id)) {
          a_xml_nodeset <- temp %>%
            rvest::html_elements(xpath = paste0("//", container, "//a"))
        }

        if (file_format == "xml" | file_format == "xml.gz") {
          links_df <- tibble::tibble(
            url = a_xml_nodeset %>%
              rvest::html_text() %>%
              stringr::str_squish(),
            link_text = NA_character_
          )
        } else {
          # effectively, expect html
          links_df <- tibble::tibble(
            url = a_xml_nodeset %>%
              xml2::xml_attr(attribute_type),
            link_text = a_xml_nodeset %>%
              rvest::html_text() %>%
              stringr::str_squish()
          )
        }
      }

      if (is.null(include_when) == FALSE) {
        links_df <- links_df %>%
          dplyr::filter(stringr::str_detect(
            string = url,
            pattern = stringr::str_c(include_when, collapse = "|")
          ))
      }

      if (is.null(exclude_when) == FALSE) {
        links_df <- links_df %>%
          dplyr::filter(!stringr::str_detect(
            string = url,
            pattern = stringr::str_c(exclude_when, collapse = "|")
          ))
      }

      if (is.null(domain) == FALSE) {
        links_df <- links_df %>%
          dplyr::mutate(
            url = dplyr::case_when(
              stringr::str_starts(
                string = url,
                pattern = "https://|http://"
              ) ~ url,
              stringr::str_starts(
                string = url,
                pattern = stringr::fixed("/")
              ) & stringr::str_ends(
                string = domain,
                pattern = stringr::fixed("/")
              ) ~ stringr::str_c(
                domain,
                stringr::str_remove(
                  string = url,
                  pattern = "/"
                )
              ),
              TRUE ~ stringr::str_c(
                domain,
                url
              )
            )
          )

        if (keep_only_within_domain == TRUE) {
          links_df <- links_df %>%
            dplyr::filter(stringr::str_starts(
              string = url,
              pattern = stringr::fixed(domain)
            ))
        }
      }

      if (is.null(append_string) == FALSE) {
        links_df <- links_df %>%
          dplyr::mutate(url = stringr::str_c(url, append_string))
      }

      if (is.null(remove_string) == FALSE) {
        links_df <- links_df %>%
          dplyr::mutate(url = stringr::str_remove_all(
            string = url,
            pattern = remove_string
          ))
      }

      if (is.null(min_length) == FALSE) {
        links_df <- links_df %>%
          dplyr::filter(nchar(url) > min_length)
      }

      if (is.null(max_length) == FALSE) {
        links_df <- links_df %>%
          dplyr::filter(nchar(url) < max_length)
      }

      if (check_previous == TRUE) {
        links_df <- links_df %>%
          dplyr::anti_join(
            y = previous_links_df,
            by = "url"
          ) %>%
          dplyr::anti_join(
            y = new_links_df,
            by = "url"
          ) %>%
          dplyr::distinct(url, .keep_all = TRUE)
      }

      if (nrow(links_df) > 0) {
        end_id <- sum(start_id, nrow(links_df) - 1)

        links_to_store_df <- links_df %>%
          dplyr::mutate(
            url = stringr::str_remove_all(string = url, pattern = "\\s"),
            source_index_id = as.numeric(x$id),
            source_index_batch = as.numeric(x$batch),
            id = as.numeric(start_id:end_id)
          ) %>%
          dplyr::select(
            "id",
            "url",
            "link_text",
            "source_index_id",
            "source_index_batch"
          )

        if (write_to_db == TRUE) {
          if (output_index == TRUE) {
            cas_write_db_index(
              urls = links_to_store_df %>%
                dplyr::select("id", "url") %>%
                dplyr::mutate(index_group = output_index_group),
              db_connection = db,
              disconnect_db = FALSE,
              ...
            )
          } else {
            cas_write_db_contents_id(
              contents_id_df = links_to_store_df,
              db_connection = db,
              disconnect_db = FALSE,
              quiet = TRUE,
              check_previous = FALSE,
              ...
            )
          }
        }

        return(dplyr::bind_rows(
          new_links_df,
          links_to_store_df
        ))
      } else {
        if (nrow(new_links_df) > 0) {
          return(new_links_df)
        } else {
          return(start_id)
        }
      }
    }
  )

  # all_links_df <- cas_read_db_contents_id(
  #   db_connection = db,
  #   ...
  # )

  # if (write_to_db == TRUE) {
  #   usethis::ui_done("Urls added to {usethis::ui_field('contents_id')} table: {usethis::ui_value(nrow(all_links_df)-nrow(previous_links_df))}")
  # }

  if (is.data.frame(new_links_df) == FALSE) {
    if (output_index == TRUE) {
      new_links_df <- casdb_empty_index_id
    } else {
      new_links_df <- casdb_empty_contents_id
    }
  }

  new_links_df
}
