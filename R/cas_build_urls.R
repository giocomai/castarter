#' Generates urls to index pages listing individual articles..
#'
#' Generates urls to index pages listing individual articles.
#'
#' @section Date formats: It is not uncommon in particular for index pages to
#'   include dates in the URL, along the lines of
#'   `example.com/archive/2022-01-01`, `example.com/archive/2022-01-02`, etc. To
#'   build such urls, \code{cas_build_urls} needs a `start_date` and `end_date`.
#'   The formatting of the date can be defined either by providing to the
#'   parameter `date_format` a string that \code{\link{strptime}} is able to
#'   interpret directly, or a simplified string (such as "YMD", without the
#'   "%"), and add a `date_separator` such as "-" as needed.
#'
#'   
#' @param url_first_part First part of index link that does not change in other
#'   index pages.
#' @param url_second_part Part of index link appneded after the part of the link
#'   that varies. If not relevant, may be left empty.
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
#'   Read more in details.
#' @param start_date,end_date Defaults to NULL. If given, a date, or a character
#'   vector coercible to date with \code{\link{as.Date}}.
#' @param project Name of 'castarter' project. Must correspond to the name of a
#'   folder in the current working directory. Not required if previously set
#'   with SetCastarter(project = "project", website = "website")
#' @param website Name of a website included in a 'castarter' project. Must
#'   correspond to the name of a sub-folder of the project folder. Not required
#'   if previously set with SetCastarter(project = "project", website =
#'   "website")
#' @return A character vector of urls to index pages.
#' @export
#' @examples
#' \dontrun{
#' index_urls <- cas_build_urls("http://www.example.com/news/")
#' }
cas_build_urls <- function(url_first_part,
                            url_second_part = NULL,
                            start_page = 1,
                            end_page = 10,
                            increase_by = 1,
                            date_format = "YMD",
                            start_date = NULL,
                            end_date = NULL,
                            date_separator = NULL,
                            reversed_order = FALSE,
                            project = NULL,
                            website = NULL, 
                            cache = NULL, 
                            cache_connection = NULL) {
  
  # allow for simplified date_format
  if (stringr::str_detect(string = date_format, pattern = "%", negate = TRUE)) {

    date_format <- stringr::str_c("%",
                   c(stringr::str_split(string = date_format,
                                        pattern = "",
                                        simplify = TRUE))) %>% 
      stringr::str_c(collapse = "")
  }
  
  if (is.null(date_separator)==FALSE) {
    date_format <- stringr::str_replace_all(string = date_format,
                                            pattern = "(?!^)%",
                                            replacement = stringr::str_c(date_separator, "%"))
  }
  

  
  if (is.null(date_format) == FALSE) {
    if (date_format == "ymd" | date_format == "Ymd" ) {
      dates <- base::seq(as.Date(start_date), as.Date(end_date), by = "day")
      dates <- base::format(as.Date(dates), paste("%Y", "%m", "%d", sep = date_separator))
    } else if (date_format == "YBd") {
      dates <- base::seq(as.Date(start_date), as.Date(end_date), by = "day")
      dates <- base::format(as.Date(dates), paste("%Y", "%B", "%d", sep = date_separator))
    }   else if (date_format == "ym" | date_format == "Ym") {
      dates <- base::seq(as.Date(start_date), as.Date(end_date), by = "month")
      dates <- base::format(as.Date(dates), paste("%Y", "%m", sep = date_separator))
    } else if (date_format == "Y"|date_format == "y"|date_format == "year") {
      dates <- base::seq(as.Date(start_date), as.Date(end_date), by = "year")
      dates <- base::format(as.Date(dates), paste("%Y", sep = date_separator))
    }  else if (date_format == "dmY") {
      dates <- base::seq(as.Date(start_date), as.Date(end_date), by = "day")
      dates <- base::format(as.Date(dates), paste("%d", "%m", "%Y", sep = date_separator))
    }
    urls <- paste0(url_first_part, dates)
  } else { # if not based on date, then create urls based on numbers
    urls <- base::paste0(url_first_part,
                         trimws(x = format(base::seq(start_page, end_page, increase_by),
                                           scientific = FALSE),
                                which = "left"))
  }
  # if url_second_part present, append
  if (is.null(url_second_part) == FALSE) {
    urls <- base::paste0(urls, url_second_part)
  }
  
  if (reversed_order == TRUE) {
    urls <- base::rev(urls)
  }
  
  urls
}
