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

