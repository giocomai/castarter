## code to prepare `casdb_empty_download` dataset goes here

casdb_empty_download <- c("id", "datetime", "status", "size") %>%
  purrr::map_dfc(setNames,
    object = list(character())
  )

casdb_empty_download$id <- as.numeric(casdb_empty_download$id)
casdb_empty_download$datetime <- lubridate::as_datetime(casdb_empty_download$datetime)
casdb_empty_download$status <- as.integer(casdb_empty_download$status)
casdb_empty_download$size <- fs::as_fs_bytes(casdb_empty_download$size)

casdb_empty_download <- tibble::as_tibble(casdb_empty_download)

usethis::use_data(casdb_empty_download, overwrite = TRUE)
