library("testthat")
test_that("cas_summarise works when single word given", {
  expect_equal(object = {
    castarter2::cas_count(corpus = castarter2::cas_demo_corpus,
                          words = c("moscow")) %>%
      cas_summarise(f = sum) %>%
      dplyr::pull(n) %>%
      sum()
  },
  expected =  {
    castarter2::cas_count(corpus = castarter2::cas_demo_corpus,
                          words = c("moscow")) %>%
      #  cas_summarise(f = sum) %>%
      dplyr::pull(n) %>%
      sum()
  })
})

test_that("cas_summarise works when more than one word given", {
  expect_equal(object = {
    castarter2::cas_count(corpus = castarter2::cas_demo_corpus,
                          words = c("moscow", "russia")) %>%
      cas_summarise(f = sum) %>%
      dplyr::pull(n) %>%
      sum()
  },
  expected =  {
    castarter2::cas_count(corpus = castarter2::cas_demo_corpus,
                          words = c("moscow", "russia")) %>%
      #  cas_summarise(f = sum) %>%
      dplyr::pull(n) %>%
      sum()
  })
})


test_that("cas_summarise works when more than one word given and period is not NULL", {
  expect_equal(object = {
    castarter2::cas_count(corpus = castarter2::cas_demo_corpus,
                          words = c("moscow", "russia")) %>%
      cas_summarise(f = sum, period = "month") %>%
      dplyr::pull(n) %>%
      sum()
  },
  expected =  {
    castarter2::cas_count(corpus = castarter2::cas_demo_corpus,
                          words = c("moscow", "russia")) %>%
     # cas_summarise(f = sum, period = "month") %>%
      dplyr::pull(n) %>%
      sum()
  })
})


test_that("cas_summarise works when one word given, rolling mean, and period is not NULL", {
  expect_false(object = {
    summurised_sum <- castarter2::cas_count(corpus = castarter2::cas_demo_corpus,
                          words = c("moscow", "russia")) %>%
      cas_summarise(f = sum, period = "month", before = 3, after = 3) %>%
      dplyr::pull(n) %>%
      sum()

    counted_sum <- castarter2::cas_count(corpus = castarter2::cas_demo_corpus,
                                         words = c("moscow", "russia")) %>%
      dplyr::pull(n) %>%
      sum()

    summurised_sum==counted_sum

  })
})



test_that("cas_summarise works when one word given, rolling mean, and period is NULL", {
  expect_false(object = {
    summurised_sum <- castarter2::cas_count(corpus = castarter2::cas_demo_corpus,
                                            words = c("moscow", "russia")) %>%
      cas_summarise(f = sum, before = 3, after = 3) %>%
      dplyr::pull(n) %>%
      sum()

    counted_sum <- castarter2::cas_count(corpus = castarter2::cas_demo_corpus,
                                         words = c("moscow", "russia")) %>%
      dplyr::pull(n) %>%
      sum()

    summurised_sum==counted_sum

  })
})
