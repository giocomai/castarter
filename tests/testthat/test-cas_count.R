library("testthat")

test_that("cas_count with one word works", {
  expect_equal(
    object = {
      df <- cas_count(
        corpus = cas_demo_corpus,
        pattern = c("Europe")
      )
      list(
        n_words = length(unique(df$pattern)),
        total_n = sum(df$n) > 0
      )
    },
    expected = list(
      n_words = 1,
      total_n = TRUE
    )
  )
})

test_that("cas_count with more than one word works", {
  expect_equal(
    object = {
      df <- cas_count(
        corpus = cas_demo_corpus,
        pattern = c("Europe", "Asia")
      )

      list(
        n_words = length(unique(df$pattern)),
        total_n = sum(df$n) > 0
      )
    },
    expected = list(
      n_words = 2,
      total_n = TRUE
    )
  )
})

test_that("cas_count sets custom column names as expected", {
  expect_equal(
    object = {
      df <- cas_count(
        corpus = cas_demo_corpus,
        pattern = c("Europe"),
        pattern_column_name = pattern,
        n_column_name = value
      )

      colnames(df)
    },
    expected = c("date", "pattern", "value")
  )
})

