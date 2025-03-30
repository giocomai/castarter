## code to prepare `casdb_empty_sitemap` dataset goes here

casdb_empty_sitemap <- tibble::tibble(
  sitemap_url = character()
)

usethis::use_data(casdb_empty_sitemap, overwrite = TRUE)
