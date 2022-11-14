#' Extract direct links to individual content pages from index pages
#'
#' @param domain Defaults to "". Web domain of the website. It is added at the beginning of each link found. If links in the page already include the full web address this should be ignored.
#' @param id Defaults to NULL. If provided, it should be a vector of integers. Only html files corresponding to given id in the relevant will be processed.
#' @param include_when Part of URL found only in links of individual articles to be downloaded. If more than one provided, it includes all links that contains either of the strings provided.
#' @param exclude_when If an URL includes this string, it is excluded from the output. One or more strings may be provided.
#'
#' @inheritParams cas_write_db_index
#'
#' @param container Type of html container from where links are to be extracted, such as "div", "ul", and others. container_class or container_id must also be provided.
#' @param attribute_type Defaults to "href". Type of attribute to extract from links.
#' @param min_length If a link is shorter than the number of characters given in min_length, it is excluded from the output.
#' @param max_length If a link is longer than the number of characters given in max_length, it is excluded from the output.
#' @param append_string If provided, appends given string to the extracted articles. Typically used to create links for print or mobile versions of the extracted page.
#' @param remove_string If provided, remove given string (or strings) from links.
#' @return A data frame.
#' @export
#' @examples
#' \dontrun{
#' links <- cas_extract_links(domain = "http://www.example.com/")
#' }
cas_extract_links <- function(id = NULL,
                              domain = NULL,
                              index = TRUE,
                              include_when = NULL,
                              exclude_when = NULL,
                              container = NULL,
                              container_class = NULL,
                              container_id = NULL,
                              min_length = NULL,
                              max_length = NULL,
                              attribute_type = "href",
                              append_string = NULL,
                              remove_string = NULL,
                              ...) {
  local_files_df <- cas_get_path_to_files(
    index = index,
    ...
  )

  if (sum(local_files_df$available) < nrow(local_files_df)) {
    usethis::ui_warn(x = "Missing files: {nrow(local_files_df %>% dplyr::filter(!available))}")

    local_files_df <- local_files_df %>%
      dplyr::filter(available) %>%
      dplyr::select(-"available")

    if (nrow(local_files_df) == 0) {
      return(NULL)
    } else {
      usethis::ui_info(x = "Links will be extracted from the {nrow(local_files_df)} file available.")
    }
  } else {
    local_files_df <- local_files_df %>%
      dplyr::select(-"available")
  }

  pb <- progress::progress_bar$new(total = nrow(local_files_df))

  all_links_df <- purrr::map_dfr(
    .x = purrr::transpose(local_files_df),
    .f = function(x) {
      pb$tick()

      temp <- xml2::read_html(
        x = x$path,
        options = c("RECOVER", "NOERROR", "NOBLANKS", "HUGE")
      )

      if (inherits(x = temp, what = "xml_node") == FALSE) {
        return(NULL)
      }

      if (is.null(container)) {
        a_xml_nodeset <- temp %>%
          rvest::html_nodes("a")
      } else if (is.null(container_id) == TRUE & is.null(container_class) == FALSE) {
        a_xml_nodeset <- temp %>%
          rvest::html_nodes(xpath = paste0("//", container, "[@class='", container_class, "']//a"))
      } else if (is.null(container_class) == TRUE & is.null(container_id) == FALSE) {
        a_xml_nodeset <- temp %>%
          rvest::html_nodes(xpath = paste0("//", container, "[@id='", container_id, "']//a"))
      } else if (is.null(container_class) & is.null(container_id)) {
        a_xml_nodeset <- temp %>%
          rvest::html_nodes(xpath = paste0("//", container, "//a"))
      }

      links_df <- tibble::tibble(
        link = a_xml_nodeset %>%
          xml2::xml_attr(attribute_type),
        title = a_xml_nodeset %>%
          rvest::html_text()
      )



      if (is.null(include_when) == FALSE) {
        links_df <- links_df %>%
          dplyr::filter(stringr::str_detect(
            string = link,
            pattern = include_when
          ))
      }

      if (is.null(exclude_when) == FALSE) {
        links_df <- links_df %>%
          dplyr::filter(!stringr::str_detect(
            string = link,
            pattern = include_when
          ))
      }

      if (is.null(domain) == FALSE) {
        links_df <- links_df %>%
          dplyr::mutate(link = stringr::str_c(domain, link))
      }

      if (is.null(append_string) == FALSE) {
        links_df <- links_df %>%
          dplyr::mutate(link = stringr::str_c(link, append_string))
      }

      if (is.null(remove_string) == FALSE) {
        links_df <- links_df %>%
          dplyr::mutate(link = stringr::str_remove(string = link, pattern = remove_string))
      }

      if (is.null(min_length) == FALSE) {
        links_df <- links_df %>%
          dplyr::filter(nchar(link) > min_length)
      }

      if (is.null(max_length) == FALSE) {
        links_df <- links_df %>%
          dplyr::filter(nchar(link) < max_length)
      }

      links_df %>%
        dplyr::mutate(
          source_index_id = x$id,
          source_batch_id = x$batch
        )
    }
  )


  links_df
}
