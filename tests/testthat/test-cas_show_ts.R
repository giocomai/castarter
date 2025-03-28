library("testthat")
test_that("cas_show_ts_dygraph outputs dygraph", {
  expect_equal(
    object = {
      count_df <- cas_count(
        corpus = cas_demo_corpus,
        pattern = c("russia", "moscow")
      ) |>
        cas_summarise(before = 15, after = 15)

      dy <- cas_show_ts_dygraph(count_df)

      class(dy)
    },
    expected = {
      c("dygraphs", "htmlwidget")
    }
  )
})
