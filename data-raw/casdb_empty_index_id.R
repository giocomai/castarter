## code to prepare `casdb_empty_index_id` dataset goes here


casdb_empty_index_id <- c("id", "url", "type") %>%
  purrr::map_dfc(setNames,
    object = list(character())
  )

casdb_empty_index_id$id <- as.numeric(casdb_empty_index_id$id)

casdb_empty_index_id <- tibble::as_tibble(casdb_empty_index_id)

usethis::use_data(casdb_empty_index_id, overwrite = TRUE)
