# library("castarter")
# test_that("app ui", {
#   ui <- castarter:::cass_explorer_app_ui()
#   golem::expect_shinytaglist(ui)
#   # Check that formals have not been removed
#   fmls <- formals(castarter:::cass_explorer_app_ui)
#   for (i in c("request")) {
#     expect_true(i %in% names(fmls))
#   }
# })
# 
# test_that("app server", {
#   server <- castarter:::cass_explorer_app_server
#   expect_is(server, "function")
#   # Check that formals have not been removed
#   fmls <- formals(castarter:::cass_explorer_app_server)
#   for (i in c("input", "output", "session")) {
#     expect_true(i %in% names(fmls))
#   }
# })
# 
# # Configure this test to fit your need
# test_that(
#   "app launches",
#   {
#     golem::expect_running(sleep = 5)
#   }
# )
