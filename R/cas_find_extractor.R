#' Facilitate finding extractors, typically  to be used with
#' `cas_extract_html()`
#'
#' This may or may not work, but it may be worth giving this a quick a try
#' before looking for alternatives. The parameters returned first should work
#' best.
#'
#' @param pattern A text string to be matched.
#' @param containers Containers to be parsed for best matches. By default:
#'   `c("h1", "h2", "h3", "h4", "span", "td", "p", "div")`. The order matters,
#'   as results are returned in this order (e.g. if a match of the same length
#'   is found both in a "h1" and in a "div", "h1" is returned first).
#' @inheritParams cas_extract_html
#'
#' @return A data frame list with container and class or id of values that
#'   should work if passed to `cas_extract_html()`.
#' @export
#'
#' @examples
#' \dontrun{
#' if (interactive) {
#'   # not ideal example, but you'll get the gist, see additonal example below
#'   library("castarter")
#'   url <- "https://www.nasa.gov/news-release/nasa-sets-coverage-for-roscosmos-spacewalk-outside-space-station/"
#'
#'   html_page <- rvest::read_html(url)
#'
#'   cas_find_extractor(
#'     html_document = html_page,
#'     pattern = "NASA Sets Coverage for Roscosmos Spacewalk Outside Space Station"
#'   )
#'
#'   cas_find_extractor(
#'     html_document = html_page,
#'     pattern = "Oct 23, 2023"
#'   )
#'
#'   cas_find_extractor(
#'     html_document = html_page,
#'     pattern = "Roxana Bardan"
#'   )
#'
#'   cas_find_extractor(
#'     html_document = html_page,
#'     pattern = "RELEASE"
#'   )
#'
#'   ## Use this information to extract contents
#'
#'
#'   library("castarter")
#'   url <- "https://www.state.gov/designating-russian-virtual-currency-money-launderer/"
#'
#'   html_page <- rvest::read_html(url)
#'
#'   cas_find_extractor(
#'     html_document = html_page,
#'     pattern = "Designating Russian Virtual Currency Money Launderer"
#'   )
#'
#'   cas_extract_html(
#'     html_document = html_page,
#'     container = "span",
#'     container_class = "bc_current collapse"
#'   )
#'
#'   cas_extract_html(
#'     html_document = html_page,
#'     container = "h1",
#'     container_class = "featured-content__headline stars-above"
#'   )
#'
#'
#'   cas_find_extractor(
#'     html_document = html_page,
#'     pattern = "Press Statement"
#'   )
#'
#'   cas_extract_html(
#'     html_document = html_page,
#'     container = "p",
#'     container_class = "article-meta doctype-meta"
#'   )
#'
#'
#'   cas_find_extractor(
#'     html_document = html_page,
#'     pattern = "Matthew Miller, Department Spokesperson"
#'   )
#'
#'   cas_extract_html(
#'     html_document = html_page,
#'     container = "p",
#'     container_class = "article-meta__author-bureau"
#'   )
#'
#'   cas_find_extractor(
#'     html_document = html_page,
#'     pattern = "November 3, 2023"
#'   )
#'
#'   cas_extract_html(
#'     html_document = html_page,
#'     container = "p",
#'     container_class = "article-meta__publish-date"
#'   )
#'
#'   cas_find_extractor(
#'     html_document = html_page,
#'     pattern = "The United States is sanctioning Ekaterina Zhdanova",
#'     exclude_css_path = "script"
#'   )
#'
#'
#'   cas_extract_html(
#'     html_document = html_page,
#'     container = "div",
#'     container_class = "entry-content",
#'     exclude_css_path = "script"
#'   )
#' }
#' }
#'
cas_find_extractor <- function(html_document,
                               pattern,
                               containers = c(
                                 "h1",
                                 "h2",
                                 "h3",
                                 "h4",
                                 "span",
                                 "td",
                                 "p",
                                 "div"
                               ),
                               exclude_css_path = NULL) {
  if (is.null(exclude_css_path) == FALSE) {
    html_document <- html_document %>%
      rvest::html_elements(css = stringr::str_c(
        ":not(",
        exclude_css_path,
        ")"
      ))
  }

  all_matches_df <- purrr::map(
    .x = containers,
    .f = function(current_container) {
      tibble::tibble(
        source_container = current_container,
        xml_nodes = purrr::map(
          .x = rvest::html_elements(html_document,
            css = current_container
          ),
          .f = function(x) {
            x
          }
        )
      ) |>
        dplyr::mutate(text = purrr::map_chr(.x = xml_nodes, .f = function(x) {
          rvest::html_text2(x)
        })) |>
        dplyr::filter(stringr::str_detect(string = text, pattern = pattern))
    }
  ) |>
    purrr::list_rbind()

  if (NROW(all_matches_df) == 0) {
    cli::cli_inform(c(x = "No match found."))
  }

  all_matches_df <- all_matches_df |>
    dplyr::arrange(nchar(text))

  tibble::tibble(
    container = purrr::map_chr(
      .x = all_matches_df[["xml_nodes"]],
      .f = function(x) {
        rvest::html_name(x)
      }
    ),
    container_qualifier = purrr::map_chr(
      .x = all_matches_df[["xml_nodes"]],
      .f = function(x) {
        node_name <- names(rvest::html_attrs(x)) %>% head(1)
        if (length(node_name) == 0) {
          node_name <- ""
        }
        node_name
      }
    ),
    container_details = purrr::map_chr(
      .x = all_matches_df[["xml_nodes"]],
      .f = function(x) {
        node_attr <- rvest::html_attrs(x) %>% head(1)
        if (length(node_attr) == 0) {
          node_attr <- ""
        }
        node_attr
      }
    ),
    nchar_text = nchar(all_matches_df[["text"]] %>% stringr::str_squish()),
    text = all_matches_df[["text"]]
  ) %>%
    dplyr::mutate(
      nchar_container_qualifier = nchar(container_qualifier),
      container = factor(container, levels = rev(containers))
    ) %>%
    dplyr::arrange(nchar_text, container, nchar_container_qualifier) %>%
    dplyr::select(-nchar_text, -nchar_container_qualifier)
}
