## code to prepare `cass_cicerone_help` dataset goes here

cass_cicerone_help <- cicerone::Cicerone$
  new()$
  step(
  el = "string",
  title = "Type the text you want to search here",
  description = "You can use commas to compare frequency of different terms"
)$
  step(
  el = "go",
  title = "Click here!",
  description = "Press 'enter' or click on the 'Go!' button to update graphs"
)$
  step(
  el = "column_selector_UI",
  title = "Select the columns of the corpus to be used for each operation",
  description = "Should the string be matched in the text column, or some other column? Do you want to aggregate by date or something else? Do you want to further aggregate dates by month or year?"
)$
  step(
  el = "freq",
  title = "Absolute or relative frequency?",
  description = "In a corpus, the quantity of text available may vary over time. While 'absolute frequency' is a plain count of matches, 'relative frequency' divides the result by the total number of words in the same unit."
)$
  step(
  el = "moving_type_selector_UI",
  title = "For time-based corpora, to increase readability it is common to show results with moving (or rolling) averages",
  description = "A 31-day moving average means that the data point shown for each day corresponds to the average of that day, and the 15 days before and after it. For lengthy datasets covering many years, 91 days may work best."
)$
  step(
  el = "date_range_input_UI",
  title = "For time-based corpora, filter the dataset in order to include only contents from a given period",
  description = "This affects all parts of this interface, including graph, tables, etc. "
)$
  step(
  el = "pre_submit_help_text_UI",
  title = "This text summarises in narrative form the choices you have selected. Remember to click on 'Go!' to apply updated settings.",
  description = "It should help ensuring you fully understand what the graphs and tables mean"
)


usethis::use_data(cass_cicerone_help, overwrite = TRUE)
