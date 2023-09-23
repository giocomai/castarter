#' Update corpus
#'
#' Currently supports only update when re-downloading index urls is expected to
#' bring new articles. It takes the first urls for each index group, and
#' continues downloading new index pages as long as new links are found in each
#' page. If no new link is found, it stops downloading and moves to the next
#' index group.
#'
#'
#' @param extract_links_partial A partial function, typically created with
#'   `purrr::partial(.f = cas_extract_links)`, followed by the paramters
#'   originally used by `cas_extract_links()`. See examples.
#' @inheritParams cas_download
#' @inheritParams cas_extract
#'
#' @return
#' @export
#'
#' @examples
#'
#' # Example of extract_links_partial:
#' extract_links_partial <- purrr::partial(
#'   .f = cas_extract_links,
#'   reverse_order = TRUE,
#'   container = "div",
#'   container_class = "hentry h-entry hentry_event",
#'   exclude_when = c("/photos", "/videos"),
#'   domain = "http://en.kremlin.ru/"
#' )
#'
cas_update <- function(extract_links_partial,
                       extractors,
                       wait = 3,
                       user_agent = NULL,
                       ...) {
  index_to_update_df <- cas_get_files_to_download(
    index = TRUE,
    download_again = TRUE
  ) |>
    dplyr::left_join(
      y = cas_read_db_index() |>
        dplyr::collect() |>
        dplyr::select(-url),
      by = "id"
    )

  index_to_download_grouped_l <- index_to_update_df |>
    dplyr::group_by(index_group) |>
    dplyr::group_split()


  purrr::walk(
    .x = index_to_download_grouped_l,
    .f = function(current_index_download_df) {
      current_group <- current_index_download_df |>
        dplyr::distinct(index_group) |>
        dplyr::pull(index_group)

      cli::cli_inform(message = "Updating index files for group {.val  {current_group}}")

      for (i in seq_along(current_index_download_df[["id"]])) {
        current_download_df <- current_index_download_df |>
          dplyr::slice(i)

        current_batch <- current_download_df %>%
          dplyr::pull("batch")

        current_id <- current_download_df %>%
          dplyr::pull("id")

        cas_download_index(
          download_df = current_download_df,
          wait = wait,
          user_agent = user_agent
        )

        new_links_df <- extract_links_partial(
          id = current_id,
          batch = current_batch,
          write_to_db = TRUE,
          ...
        )

        Sys.sleep(wait)

        if (is.null(new_links_df) == TRUE) {
          break
        } else if (nrow(new_links_df) == 0) {
          break
        }
      }
    }
  )


  cas_download(
    wait = wait,
    user_agent = user_agent,
    ...
  )

  cas_extract(
    extractors = extractors,
    ...
  )
}
