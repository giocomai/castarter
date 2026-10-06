#' Retrieve data through Wordpress API
#'
#' See also
#' \href{https://developer.wordpress.org/rest-api/using-the-rest-api/}{Wordpress
#' REST API official documentation.}
#'
#' @param api_base_url Most correspond to the base URL of a Wordpress API
#'   endpoint.
#' @param api_endpoint The endpoint, defaults to "posts". Endpoints can be
#'   specific to each Wordpress deployment or be offered by plugins, but common
#'   endpoints include, for example, "author", "categories", "pages", etc.
#' @param per_page Defaults to 10. By default, this is capped at 100 by
#'   Wordpress server-side.
#' @param order_by Defaults to "id", available values depend on the given
#'   endpoint. Common options include "date".
#' @param order Defaults to "desc", for descending order. The only other valid
#'   value is "asc", for ascending order.
#'
#' @inheritParams cas_check_response
#' @inheritParams cas_connect_to_db
#'
#' @returns A data frame, likely including list columns, including API response.
#' @export
#'
#' @examples
cas_wp_api <- function(
  api_base_url,
  api_endpoint = "posts",
  per_page = 10,
  order_by = c("id", "date", "relevance", "include", "title", "slug"),
  order = c("desc", "asc"),

  output_only_newly_checked = FALSE,
  output_only_cached = FALSE,

  db_connection = NULL,
  disconnect_db = FALSE,
  check_db = TRUE,
  write_db = TRUE,

  ignore_ssl_certificates = FALSE,
  user_agent = NULL,
  wait = 1,
  ...
) {
  ssl_verifypeer_digit <- dplyr::if_else(
    condition = ignore_ssl_certificates,
    true = 0,
    false = 1
  )

  if (!check_db & !write_db) {
    # do nothing, as connection won't be needed
  } else {
    db <- cas_connect_to_db(
      db_connection = db_connection,
      ...
    )
    current_table <- stringr::str_c("wp_api_", api_endpoint)
    exists_table <- DBI::dbExistsTable(conn = db, name = current_table)
  }

  if (!exists_table) {
    previous_data_df <- NULL
    previous_id_v <- character()
  } else {
    previous_data_df <- DBI::dbReadTable(
      conn = db,
      name = current_table
    )
    previous_id_v <- previous_data_df |>
      dplyr::pull("id")
  }

  req <- httr2::request(base_url = api_base_url) |>
    httr2::req_url_path_append(api_endpoint) |>
    httr2::req_url_query(
      per_page = per_page,
      order_by = order_by[[1]],
      order = order[[1]]
    ) |>
    httr2::req_headers()

  resp <- httr2::req_perform(req = req)

  total_pages <- resp |>
    httr2::resp_header(header = "x-wp-totalpages") |>
    as.integer()

  total_items <- resp |>
    httr2::resp_header(header = "x-wp-total") |>
    as.integer()

  if (total_items == 0) {
    cli::cli_abort(
      message = "There are no items available at the endpoint {.var {api_endpoint}}."
    )
  }

  for (current_page in 1:total_pages) {
    req <- httr2::request(base_url = api_base_url) |>
      httr2::req_url_path_append(api_endpoint) |>
      httr2::req_url_query(
        per_page = per_page,
        order_by = order_by[[1]],
        order = order[[1]],
        page = current_page
      ) |>
      httr2::req_options(ssl_verifypeer = ssl_verifypeer_digit) |>
      httr2::req_user_agent(string = user_agent) |>
      httr2::req_error(is_error = \(resp) FALSE)

    resp <- tryCatch(
      req |> httr2::req_perform(),
      error = \(e) FALSE
    )

    out_df <- resp |>
      httr2::resp_body_string() |>
      yyjsonr::read_json_str() |>
      tibble::as_tibble() |>
      dplyr::mutate(
        dplyr::across(
          dplyr::where(is.list),
          ~ purrr::map(
            .x,
            ~ if (length(.x) == 0 || is.null(.x)) NA_character_ else .x
          )
        )
      ) |>
      dplyr::mutate(
        dplyr::across(
          dplyr::where(is.list),
          ~ purrr::map(.x, as.character)
        )
      )

    new_df <- out_df |>
      dplyr::filter_out(id %in% previous_id_v)

    if (nrow(new_df) == 0) {
      break
    }

    if (exists_table) {
      DBI::dbAppendTable(
        conn = db,
        name = current_table,
        value = new_df
      )
    } else {
      DBI::dbWriteTable(
        conn = db,
        name = current_table,
        value = new_df
      )
      exists_table <- TRUE
    }
  }

  all_data_df <- DBI::dbReadTable(
    conn = db,
    name = current_table
  )

  all_data_df |>
    dplyr::collect() |>
    tibble::as_tibble()
}
