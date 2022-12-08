## code to prepare `casdb_empty_ia_check` dataset goes here


casdb_empty_ia_check <- c("url", "status", "available", "ia_url", "timestamp", "checked_at") %>%
  purrr::map_dfc(setNames,
    object = list(character())
  )

casdb_empty_ia_check$available <- as.logical(casdb_empty_ia_check$available)

casdb_empty_ia_check$timestamp <- as.POSIXct(casdb_empty_ia_check$timestamp)
casdb_empty_ia_check$checked_at <- as.POSIXct(casdb_empty_ia_check$checked_at)

casdb_empty_ia_check <- tibble::as_tibble(casdb_empty_ia_check)

usethis::use_data(casdb_empty_ia_check, overwrite = TRUE)
