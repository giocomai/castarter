## code to prepare `cas_google_client` dataset goes here

cas_google_client <- gargle::gargle_oauth_client_from_json("client.json")

usethis::use_data(cas_google_client, overwrite = TRUE)
