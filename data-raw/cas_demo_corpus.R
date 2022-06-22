## code to prepare `cas_demo_corpus` dataset goes here
cas_demo_corpus <- corpus_df %>%
  dplyr::filter(date > as.Date("1999-12-31"), date < as.Date("2002-01-01")) %>%
  dplyr::filter(location == "The Kremlin, Moscow")

usethis::use_data(cas_demo_corpus, overwrite = TRUE)
