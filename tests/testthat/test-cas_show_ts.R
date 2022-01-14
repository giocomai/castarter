library("testthat")
test_that("cas_show_ts_dygraph outputs dygraph", {
  expect_equal(
    object = {
      count_df <- castarter2::cas_count(
        corpus = castarter2::cas_demo_corpus,
        words = c("russia", "moscow")
      ) %>%
        cas_summarise(before = 15, after = 15)

      dy <- cas_show_ts_dygraph(count_df)

      class(dy)
    },
    expected = {
      c("dygraphs", "htmlwidget")
    }
  )
})
