library("testthat")

test_that("actually outputs girage object", {
  expect_equal(object = {
    count_df <- castarter::cas_count(
      corpus = castarter::cas_demo_corpus,
      pattern = c("moscow", "russia")
    ) %>%
      cas_summarise(period = "quarter", f = sum)

    barchart_gg <- count_df %>%
      # dplyr::mutate(date = date %>% as.character() %>%
      #                 forcats::as_factor() %>%
      #                 forcats::fct_inorder()) %>%
      ggplot2::ggplot(mapping = ggplot2::aes(x = date, y = n, fill = word)) +
      ggplot2::geom_col(position = ggplot2::position_dodge()) +
      ggplot2::scale_x_discrete(name = "", guide = ggplot2::guide_axis(n.dodge = 2)) +
      ggplot2::scale_y_continuous(name = "", labels = scales::number) +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.title = ggplot2::element_blank())

    ggobj <- count_df %>%
      dplyr::mutate(date = date %>% as.character() %>%
        forcats::as_factor() %>%
        forcats::fct_inorder()) %>%
      ggplot2::ggplot(mapping = ggplot2::aes(
        x = date,
        y = n,
        fill = word
      ))


    castarter::cas_count(
      corpus = castarter::cas_demo_corpus,
      pattern = c("moscow")
    ) %>%
      cas_summarise(period = "quarter", f = sum) %>%
      cas_show_barchart_gg_base() %>%
      cas_show_barchart_ggiraph()

    class(ggiraph::girafe(ggobj = ggobj +
      ggiraph::geom_col_interactive(
        mapping = ggplot2::aes(
          data_id = word,
          tooltip = n
        ),
        position = ggplot2::position_dodge()
      )))
  }, expected = {
    c("girafe", "htmlwidget")
  })
})
