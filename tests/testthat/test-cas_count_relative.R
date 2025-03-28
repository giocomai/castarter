test_that("cas_count_relative() works when more than one string given", {
  expect_equal(
    object = {
      cas_count_relative(
        corpus = cas_demo_corpus,
        pattern = c("putin", "russia"),
        group_by = date
      ) |>
        dplyr::filter(pattern == "russia")
    },
    expected = {
      cas_count_relative(
        corpus = cas_demo_corpus,
        pattern = c("russia"),
        group_by = date
      )
    }
  )
})
