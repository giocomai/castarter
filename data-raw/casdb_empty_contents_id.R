casdb_empty_contents_id <- c("id", "url", "type") %>%
  purrr::map_dfc(setNames,
    object = list(character())
  )

casdb_empty_contents_id$id <- as.numeric(casdb_empty_contents_id$id)

casdb_empty_contents_id <- tibble::as_tibble(casdb_empty_contents_id)


usethis::use_data(casdb_empty_contents_id, overwrite = TRUE)
