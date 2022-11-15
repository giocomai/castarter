casdb_empty_contents_id <- c("id", "url", "link_text", "source_index_id", "source_index_batch") %>%
  purrr::map_dfc(setNames,
    object = list(character())
  )

casdb_empty_contents_id$id <- as.numeric(casdb_empty_contents_id$id)
casdb_empty_contents_id$source_index_id <- as.numeric(casdb_empty_contents_id$source_index_id)
casdb_empty_contents_id$source_index_batch <- as.numeric(casdb_empty_contents_id$source_index_batch)

casdb_empty_contents_id <- tibble::as_tibble(casdb_empty_contents_id)

usethis::use_data(casdb_empty_contents_id, overwrite = TRUE)
