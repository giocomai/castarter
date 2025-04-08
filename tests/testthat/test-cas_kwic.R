library("dplyr", warn.conflicts = TRUE)
library("testthat")
library("castarter")

corpus <- tibble::tibble(
  text = c(
    "The quick brown fox jumps over the lazy dog.", # standard
    "The quick brown fox jumps over the lazy dog", # standard, but without period at the end
    "The fox jumps.", # short both sides
    "Long long story before getting to when the quick brown fox jumps over the lazy dog.", # long both sides
    "The quick brown cat jumps over the lazy dog.", # no fox
    "The quick brown cat jumps over the lazy dog.", # no text
    "fox", # only fox
    "The, quick, brown fox jumps over ,the lazy dog.",
    "The quick brown fox.",
    "fox jumps over the lazy dog.",
    "The quick brown fox jumps over the lazy dog, and then another black fox arrives."
  )
)


# i <- 1
# corpus = tibble::tibble(text = "The quick brown fox jumps over the lazy dog.")
# pattern = "fox"
# current_all_words_location_l <- all_words_location_l[[i]]
# current_pattern_location_l <- pattern_location_l[[i]]
# current_text <- corpus$text[i]
# ignore_case = TRUE
#

test_that("cas_kwic works with one pattern", {
  expect_equal(
    cas_kwic(
      corpus = tibble::tibble(
        text = "The quick brown fox jumps over the lazy dog."
      ),
      pattern = "fox"
    ),
    tibble::tribble(
      ~text,
      ~before,
      ~pattern,
      ~after,
      "The quick brown fox jumps over the lazy dog.",
      "The quick brown",
      "fox",
      "jumps over the lazy dog."
    )
  )
})


test_that("cas_kwic works when match is the last word", {
  expect_equal(
    cas_kwic(
      corpus = tibble::tibble(
        text = "The quick brown fox jumps over the lazy dog."
      ),
      pattern = "dog"
    ),
    tibble::tribble(
      ~text,
      ~before,
      ~pattern,
      ~after,
      "The quick brown fox jumps over the lazy dog.",
      "fox jumps over the lazy",
      "dog",
      "."
    )
  )
})


test_that("cas_kwic works with two patterns", {
  expect_equal(
    cas_kwic(
      corpus = tibble::tibble(
        text = "The quick brown fox jumps over the lazy dog."
      ),
      pattern = c("fox", "dog")
    ),
    tibble::tribble(
      ~text,
      ~before,
      ~pattern,
      ~after,
      "The quick brown fox jumps over the lazy dog.",
      "The quick brown",
      "fox",
      "jumps over the lazy dog.",
      "The quick brown fox jumps over the lazy dog.",
      "fox jumps over the lazy",
      "dog",
      "."
    )
  )
})




test_that("cas_kwic works with pattern of two words", {
  expect_equal(
    cas_kwic(
      corpus = tibble::tibble(
        text = "The quick brown fox jumps over the lazy dog."
      ),
      pattern = "brow[:alpha:]+ fox"
    ),
    tibble::tribble(
      ~text,
      ~before,
      ~pattern,
      ~after,
      "The quick brown fox jumps over the lazy dog.",
      "The quick",
      "brown fox",
      "jumps over the lazy dog."
    )
  )
})




test_that("cas_kwic works with pattern of two words", {
  expect_equal(
    cas_kwic(
      corpus = tibble::tibble(
        text = "The quick brown fox jumps over the lazy dog, and then another browny fox arrives."
      ),
      pattern = "brow[:alpha:]+ fox"
    ),
    tibble::tribble(
      ~text,
      ~before,
      ~pattern,
      ~after,
      "The quick brown fox jumps over the lazy dog, and then another browny fox arrives.",
      "The quick",
      "brown fox",
      "jumps over the lazy dog",
      
      "The quick brown fox jumps over the lazy dog, and then another browny fox arrives.",
      "lazy dog, and then another",
      "browny fox",
      "arrives."
    )
  )
})
