library("testthat")
library("castarter2")


test_that("If empty database, return data frame with zero row with cas_read_index", {
  expect_true(
    {
      folder <- cas_set_db_folder(fs::path(tempdir(), "R", "castarter_data", stringi::stri_rand_strings(n = 1, length = 12)))
      cas_create_db_folder(path = folder, ask = FALSE)
      db <- cas_connect_to_db(use_db = TRUE,
                              RSQLite = folder,
                              project = "example_project")
      
      index_df <- cas_read_index(use_db = TRUE,
                                 db_connection = db,
                                 disconnect_db = TRUE)
      
      nrow(index_df)==0
    })
})


test_that("cas_write_index writes, and cas_read_index returns data frame of same length", {
  expect_true(
    {
      folder <- cas_set_db_folder(fs::path(tempdir(), "R", "castarter_data", stringi::stri_rand_strings(n = 1, length = 12)))
      cas_create_db_folder(ask = FALSE)
      cas_check_db_folder()
      db <- cas_connect_to_db(use_db = TRUE,
                              RSQLite = folder,
                              project = "example_project")
      
      urls_df <- cas_build_urls(
        url_beginning = "https://www.example.com/news/",
        start_page = 1,
        end_page = 10
      )
      
      cas_write_index(urls = urls_df,
                      use_db = TRUE,
                      db_connection = db,
                      overwrite = FALSE,
                      disconnect_db = FALSE)
      
      index_df <- cas_read_index(use_db = TRUE,
                                 db_connection = db,
                                 disconnect_db = TRUE)
      
      nrow(index_df)==10
    })
})


test_that("cas_write_index tries to write twice same data, and cas_read_index returns data frame of same length", {
  expect_true(
    {
      folder <- cas_set_db_folder(fs::path(tempdir(), "R", "castarter_data", stringi::stri_rand_strings(n = 1, length = 12)))
      cas_create_db_folder(ask = FALSE)
      cas_check_db_folder()
      db <- cas_connect_to_db(use_db = TRUE,
                              RSQLite = folder,
                              project = "example_project")
      
      urls_df <- cas_build_urls(
        url_beginning = "https://www.example.com/news/",
        start_page = 1,
        end_page = 10
      )
      
      cas_write_index(urls = urls_df,
                      use_db = TRUE,
                      db_connection = db,
                      overwrite = FALSE,
                      disconnect_db = FALSE)
      
      cas_write_index(urls = urls_df,
                      use_db = TRUE,
                      db_connection = db,
                      overwrite = FALSE,
                      disconnect_db = FALSE)
      
      index_df <- cas_read_index(use_db = TRUE,
                                 db_connection = db,
                                 disconnect_db = TRUE)
      
      nrow(index_df)==10
    })
})




test_that("cas_write_index tries to write new set of urls, but with already used id", {
  expect_true(
    {
      folder <- cas_set_db_folder(fs::path(tempdir(), "R", "castarter_data", stringi::stri_rand_strings(n = 1, length = 12)))
      cas_create_db_folder(ask = FALSE)
      cas_check_db_folder()
      db <- cas_connect_to_db(use_db = TRUE,
                              RSQLite = folder,
                              project = "example_project")
      
      urls_df <- cas_build_urls(
        url_beginning = "https://www.example.com/news/",
        start_page = 1,
        end_page = 10
      )
      
      cas_write_index(urls = urls_df,
                      use_db = TRUE,
                      db_connection = db,
                      overwrite = FALSE,
                      disconnect_db = FALSE)
      
      urls_df <- cas_build_urls(
        url_beginning = "https://www.example.com/news/",
        start_page = 10,
        end_page = 20
      )
      
      cas_write_index(urls = urls_df,
                      use_db = TRUE,
                      db_connection = db,
                      overwrite = FALSE,
                      disconnect_db = FALSE)
      
      index_df <- cas_read_index(use_db = TRUE,
                                 db_connection = db,
                                 disconnect_db = TRUE)
      
      sum(nrow(index_df)==20,
          length(unique(index_df$index_id))==20)==2
    })
})


