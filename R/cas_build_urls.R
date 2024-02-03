#' URL builder
#'
#' Convenience function typically used to generate urls to index pages listing
#' articles.
#'
#' @section Date formats: It is not uncommon in particular for index pages to
#'   include dates in the URL, along the lines of
#'   `example.com/archive/2022-01-01`, `example.com/archive/2022-01-02`, etc. To
#'   build such urls, \code{cas_build_urls} needs a `start_date` and `end_date`.
#'   The formatting of the date can be defined either by providing to the
#'   parameter `date_format` a string that \code{\link{strptime}} is able to
#'   interpret directly, or a simplified string (such as "Ymd", without the
#'   "%"),adding a `date_separator` such as "-" as needed.
#'
#'
#' @param url First part of index link that does not change in other
#'   index pages.
#' @param glue Logical, defaults to FALSE. If TRUE, the url is parsed with
#'   `glue`, enabling custom or repeated location for the variable part of the
#'   url. If `glue` is set to TRUE, it is expected that the url will include the
#'   string `{here}` within curly brackets, e.g.
#'   `https://example.com/archive/?from_date={here}&to_date={here}`.
#' @param url_ending Part of index link appneded after the part of the link that
#'   varies. If not relevant, may be left empty.
#' @param start_page If the urls include a numerical component, define first
#'   number of the sequence. start_page defaults to 1.
#' @param end_page If the urls include a numerical component, define first
#'   number of the sequence. end_page defaults to 10.
#' @param increase_by Defines by how much the number in the link should be
#'   increased in the numerical sequence. Defaults to 1.
#' @param date_format A character string, defaults to "YMD". Check
#'   \code{\link{strptime}} for valid values used to define the format of the
#'   date that is part of the URL. Simplified formats such as the following are
#'   also accepted: "Y" (e.g. 2022), "Ym" (2022-10), "Ymd" (e.g. 2022-10-24).
#'   See details.
#' @param start_date Defaults to NULL. If given, a date, or a character vector
#'   of length one coercible to date with \code{\link{as.Date}}. When given,
#'   urls are built based on dates, and parameters `start_page`, `end_page`, and
#'   `increase_by`, are ignored.
#' @param end_date Defaults to \code{Sys.Date()}. If given, a date, or a
#'   character vector of length one coercible to date with
#'   \code{\link{as.Date}}.
#' @param increase_date_by Defaults to "day". See \code{\link{seq.Date}} for
#'   valid values.
#' @param reversed_order Logical, defaults to FALSE. If TRUE, the order of urls
#'   in the output.
#' @param index_group A character vector, defaults to "index". Used for
#'   differentiating among different types of index or links in local databases.
#' @param index Defaults to TRUE. Relevant only if `write_to_db` is also set to
#'   TRUE. If TRUE, urls are stored in the local database in the index table,
#'   otherwise they are stored in the contents table.
#' @param write_to_db Defaults to FALSE. If set to TRUE, stores the newly
#'   created URLs to the local database.
#' @return A data frame with three columns, `id`, `url`, and `index_group`.
#'   Typically, `url` corresponds to a vector of unique urls.
#' @export
#' @examples
#' cas_build_urls(
#'   url = "https://www.example.com/news/",
#'   start_page = 1,
#'   end_page = 10
#' )
#'
#' cas_build_urls(
#'   url = "https://example.com/news/?skip=",
#'   start_page = 0,
#'   end_page = 100,
#'   increase_by = 10
#' )
#'
#'
#' cas_build_urls(
#'   url = "https://example.com/archive/",
#'   start_date = "2022-01-01",
#'   end_date = "2022-12-31",
#'   date_separator = "-"
#' ) %>%
#'   head()
#'
#' cas_build_urls(
#'   url = "https://example.com/archive/?from={here}&to={here}",
#'   glue = TRUE,
#'   start_date = "2011-01-01",
#'   end_date = "2022-12-31",
#'   date_separator = ".",
#'   date_format = "dmY",
#'   index_group = "news"
#' )
cas_build_urls <- function(url,
                           url_ending = "",
                           glue = FALSE,
                           start_page = NULL,
                           end_page = NULL,
                           increase_by = 1,
                           date_format = "Ymd",
                           start_date = NULL,
                           end_date = Sys.Date() - 1,
                           date_separator = NULL,
                           increase_date_by = "day",
                           reversed_order = FALSE,
                           index_group = "index",
                           index = TRUE,
                           write_to_db = FALSE,
                           ...) {
  if (is.null(start_date) == FALSE) {
    # allow for simplified date_format
    if (stringr::str_detect(string = date_format, pattern = "%", negate = TRUE)) {
      date_format <- stringr::str_c(
        "%",
        c(stringr::str_split(
          string = date_format,
          pattern = "",
          simplify = TRUE
        ))
      ) %>%
        stringr::str_c(collapse = "")
    }

    if (is.null(date_separator) == FALSE) {
      date_format <- stringr::str_replace_all(
        string = date_format,
        pattern = "(?!^)%",
        replacement = stringr::str_c(date_separator, "%")
      )
    }

    variable_part <- base::format(
      base::seq.Date(as.Date(start_date),
        as.Date(end_date),
        by = increase_date_by
      ),
      date_format
    ) %>%
      base::unique()
  } else if (is.null(start_page) | is.null(end_page)) {
    variable_part <- ""
  } else {
    variable_part <- format(
      base::seq(start_page, end_page, increase_by),
      scientific = FALSE
    ) %>%
      stringr::str_trim()
  }

  if (glue == TRUE) {
    here <- variable_part
    urls <- glue::glue(url)
  } else {
    urls <- stringr::str_c(
      url,
      variable_part,
      url_ending
    )
  }

  if (reversed_order == TRUE) {
    urls <- base::rev(urls)
  }

  output_df <- tibble::tibble(url = as.character(urls %>%
    base::unique() %>%
    stringr::str_trim())) %>%
    dplyr::mutate(
      index_group = index_group,
      id = as.numeric(dplyr::row_number())
    ) %>%
    dplyr::select(
      "id",
      "url",
      "index_group"
    )

  if (write_to_db == TRUE) {
    cas_write_db_urls(
      urls = output_df,
      index = index,
      ...
    )
  }
  output_df
}
