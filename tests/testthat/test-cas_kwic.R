library("dplyr", warn.conflicts = TRUE)
library("testthat")

corpus <- tibble::tibble(
  text = c("The quick brown fox jumps over the lazy dog.", # standard
           "The fox jumps.", # short both sides
           "Long long story before getting to when the quick brown fox jumps over the lazy dog.", # long both sides
           "The quick brown cat jumps over the lazy dog.", # no fox
           "The quick brown cat jumps over the lazy dog.", # no text
           "fox", # only fox
           "The, quick, brown fox jumps over ,the lazy dog.",
           "The quick brown fox.",
           "fox jumps over the lazy dog.",
           "The quick brown fox jumps over the lazy dog, and then another black fox arrives."))



# i <- 1
# corpus = tibble::tibble(text = "The quick brown fox jumps over the lazy dog.")
# string = "fox"
# current_all_words_location_l <- all_words_location_l[[i]]
# current_string_location_l <- string_location_l[[i]]
# current_text <- corpus$text[i]
# ignore_case = TRUE
#


test_that("cas_kwic works with one string", {
  expect_equal(
    castarter2::cas_kwic(corpus = tibble::tibble(text = "The quick brown fox jumps over the lazy dog."),
                         string = "fox"),

    tibble::tribble(~text, ~before, ~string, ~after,
                    "The quick brown fox jumps over the lazy dog.", "The quick brown ", "fox", " jumps over the lazy dog"))
})


test_that("cas_kwic works when match is the last word", {
  expect_equal(
    castarter2::cas_kwic(corpus = tibble::tibble(text = "The quick brown fox jumps over the lazy dog."),
                         string = "dog"),

    tibble::tribble(~text, ~before, ~string, ~after,
                    "The quick brown fox jumps over the lazy dog.", "fox jumps over the lazy ", "dog", ""))
})



test_that("cas_kwic works with two strings", {
  expect_equal(
    castarter2::cas_kwic(corpus = tibble::tibble(text = "The quick brown fox jumps over the lazy dog."),
                         string = c("fox", "dog")),

    tibble::tribble(~text, ~before, ~string, ~after,
                    "The quick brown fox jumps over the lazy dog.", "The quick brown ", "fox", " jumps over the lazy dog",
                    "The quick brown fox jumps over the lazy dog.", "fox jumps over the lazy ", "dog", ""))
})




