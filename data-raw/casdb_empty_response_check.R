## code to prepare `casdb_empty_response_check` dataset goes here

casdb_empty_response_check <- tibble::tibble(
  url = NA_character_,
  status = NA_integer_,
  status_description = NA_character_,
  type = NA_character_,
  encoding = NA_character_,
  retry_after = NA_character_,
  available = FALSE,
  checked_at = NA,
) |>
  dplyr::slice(0)

casdb_empty_response_check$checked_at <- as.POSIXct(
  casdb_empty_response_check$checked_at
)

usethis::use_data(casdb_empty_response_check, overwrite = TRUE)
