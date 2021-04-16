test_that("strings used as inputs in shiny app are property split in a vector", {
  expect_equal(object = cass_split(string = "Dogs,    cats, horses",
                                   squish = TRUE,
                                   to_lower = TRUE),

               expected =  c("dogs", "cats", "horses"))
})


test_that("vector of words is properly transformed into a pattern for string matching", {
  expect_equal(object = cass_combine_into_pattern(c("dogs", "cats", "horses")),

               expected =  c("\\bdogs\\b|\\bcats\\b|\\bhorses\\b"))
})
