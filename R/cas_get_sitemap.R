#' Checks for availability of a sitemap in xml format.
#'
#' Searches in common locations (namely, `example.com/sitemap.xml`, and
#' `example.com/sitemap_index.xml`) and then in robots.txt and returns a url to
#' the sitemap, along with the contents of the sitemap itself, if found.
#'
#' @param domain Defaults to `NULL`, but required unless `sitemap_url` given.
#'   Expected to be a full domain name. If input does not start with `http`,
#'   then `https://` is prepended automatically.
#' @param sitemap_url Defaults to `NULL`. If given, `domain` is ignored.
#'
#' @returns A data frame, including a `sitemap_url` column, the response as an
#'   httr2 object, and the body of the xml.
#' @export
#'
#' @examples
#' if (interactive()) {
#'   cas_get_sitemap("https://www.europeandatajournalism.eu/")
#' }
cas_get_sitemap <- function(
  domain = NULL,
  sitemap_url = NULL
) {
  sitemap_response <- NULL

  if (stringr::str_starts(string = domain, pattern = "http", negate = TRUE)) {
    domain <- stringr::str_flatten(string = c("https://", domain))
  }

  if (is.null(sitemap_url)) {
    url <- httr2::url_modify(url = domain, path = "sitemap.xml")

    req <- httr2::request(url) |>
      httr2::req_error(is_error = \(resp) FALSE)

    resp <- tryCatch(req |> httr2::req_perform(), error = \(e) FALSE)

    if (isFALSE(resp)) {
      sitemap_url <- NULL
    } else if (!httr2::resp_has_body(resp)) {
      sitemap_url <- NULL
    } else {
      sitemap_response <- resp
    }
  }

  if (is.null(sitemap_url)) {
    url <- httr2::url_modify(url = domain, path = "sitemap_index.xml")

    req <- httr2::request(url) |>
      httr2::req_error(is_error = \(resp) FALSE)

    resp <- tryCatch(req |> httr2::req_perform(), error = \(e) FALSE)

    if (isFALSE(resp)) {
      sitemap_url <- NULL
    } else if (!httr2::resp_has_body(resp)) {
      sitemap_url <- NULL
    } else {
      sitemap_response <- resp
    }
  }

  if (is.null(sitemap_url)) {
    url <- httr2::url_modify(url = domain, path = "robots.txt")

    req <- httr2::request(url) |>
      httr2::req_error(is_error = \(resp) FALSE)

    resp <- tryCatch(req |> httr2::req_perform(), error = \(e) FALSE)

    if (isFALSE(resp)) {
      return(invisible(NULL))
    } else if (!httr2::resp_has_body(resp)) {
      return(invisible(NULL))
    } else {
      body_string <- httr2::resp_body_string(resp)

      sitemap_df <- tibble::tibble(
        robots = stringr::str_split(
          string = body_string,
          pattern = "\n",
          simplify = TRUE
        ) |>
          as.character()
      ) |>
        dplyr::filter(stringr::str_starts(
          robots,
          stringr::fixed("Sitemap", ignore_case = TRUE)
        )) |>
        dplyr::transmute(
          sitemap = stringr::str_remove(robots, stringr::fixed("Sitemap:")) |>
            stringr::str_squish()
        )

      if (nrow(sitemap_df) == 0) {
        cli::cli_inform(message = "Sitemap not found in {.code robots.txt}")
        return(invisible(NULL))
      }
      sitemap_url <- sitemap_df |> dplyr::pull(sitemap)
    }
  }

  if (is.null(sitemap_response)) {
    req <- httr2::request(sitemap_url) |>
      httr2::req_error(is_error = \(resp) FALSE)

    sitemap_response <- tryCatch(
      req |> httr2::req_perform(),
      error = \(e) FALSE
    )
  }

  sitemap_df <- tibble::tibble(sitemap_url = sitemap_url) |>
    dplyr::mutate(
      status = httr2::resp_status(resp = resp),
      content_type = httr2::resp_content_type(resp = resp),
      file_format = stringr::str_extract(content_type, "[[:alpha:]]+$"),
      response = list(sitemap_response),
      xml_body = list(httr2::resp_body_xml(resp = sitemap_response))
    )

  sitemap_df
}
