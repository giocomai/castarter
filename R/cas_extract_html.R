#' Facilitates extraction of contents from an html file
#'
#' @param html_document An html document parsed with `xml2::read_html()` or
#'   `rvest::read_html()`.
#' @param container Defaults to NULL. Type of html container from where links
#'   are to be extracted, such as "div", "ul", and others. Either
#'   `container_class` or `container_id` must also be provided.
#' @param container_class Defaults to NULL. If provided, also `container` must
#'   be given (and `container_id` must be NULL). Only text found inside the
#'   provided combination of container/class will be extracted.
#' @param container_id Defaults to NULL. If provided, also `container` must be
#'   given (and `container_id` must be NULL). Only text found inside the
#'   provided combination of container/class will be extracted.
#' @param container_itemprop Defaults to NULL. If provided, also `container`
#'   must be given (and `container_id` and `container_class` must be NULL or
#'   will be silently ignored). Only text found inside the provided combination
#'   of container/itemprop will be extracted.
#' @param container_instance Defaults to NULL. If given, it must be an integer.
#'   If a given combination is found more than once in the same page, the
#'   relevant occurrence is kept. Use with caution, as not all pages always
#'   include the same number of elements of the same class/with the same id.
#' @param sub_element Defaults to NULL. If provided, also `container` must be
#'   given. Only text within elements of given type under the chosen combination
#'   of container/containerClass will be extracted. When given, it will
#'   tipically be "p", to extract all p elements inside the selected div.
#' @param no_children Defaults to FALSE, i.e. by default all subelements of the
#'   selected combination (e.g. div with given class) are extracted. If TRUE,
#'   only text found under the given combination (but not its subelements) will
#'   be extracted. Corresponds to the xpath string `/node()[not(self::div)]`.
#' @param attribute Defaults to NULL. If given, type of attribute to extract.
#'   Typically used in combination with container, as in
#'   `cas_extract_html(container = "time", attribute = "datetime")`.
#' @param exclude_CSSpath Defaults to NULL. To remove script, for example, use
#'    `script`, which is transformed to `:not(script)`. May cause issues, use
#'    with caution.
#' @param remove_Xpath Defaults to NULL. A common pattern when extracting text
#'   would be `//script|//iframe|//img`, as it is assumed that these containers
#'   (javascript contents, iframes, and images) are most likely undesirable when
#'   extracting text. Customise as needed. For example, if besides the above you
#'   also want to remove a `div` of class `related-articles`, you may use
#'   `//script|//iframe|//img|//div[@class='related-articles']`Be careful when
#'   using `remove_Xpath` as the relevant Xpath is removed from the original
#'   objext passed to `cas_extract_html()`. To be clear, the input object is
#'   changed, and, for example, if used once in one of the extractors these
#'   containers won't be available to other extractors.
#' @param custom_Xpath Defaults to NULL. If given, all other parameters are
#'   ignored and given Xpath used instead.
#' @param custom_CSSpath Defaults to NULL. If given, all other parameters are
#'   ignored and given CSSpath used instead.
#' @param keep_everything Defaults to FALSE. If TRUE, all text included in the
#'   page is returned as a single string.
#' @param trim Defaults to TRUE. If TRUE, applies `stringr::str_trim()` to
#'   output, removing whitespace from start and end of string.
#' @param squish Defaults to FALSE. If TRUE, applies `stringr::str_squish()` to
#'   output, removing whitespace from start and end of string, and replacing
#'   any whitespace (including new lines) with a single space.
#' @param no_match Defaults to "". A common alternative would be NA. Value to
#'   return when the given container, selector or element is not found.
#' @param extract_text Defaults to TRUE. If TRUE, text is extracted.
#' @param as_character Defaults to TRUE. If FALSE, and if `extract_text` is set
#'   to FALSE, then an `xml_nodeset` object is returned.
#'
#' @return A character vector of length one.
#' @export
#'
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   url <- "https://example.com"
#'   html_document <- rvest::read_html(x = url)
#'
#'   # example for a tag that looks like:
#'   # <meta name="twitter:title" content="Example title" />
#'
#'   cas_extract_html(
#'     html_document = html_document,
#'     container = "meta",
#'     container_name = "twitter:title",
#'     attribute = "content"
#'   )
#'
#'
#'   # example for a tag that looks like:
#'   # <meta name="keywords" content="various;keywords;">
#'   cas_extract_html(
#'     html_document = html_document,
#'     container = "meta",
#'     container_name = "keywords",
#'     attribute = "content"
#'   )
#'
#'   # example for a tag that looks like:
#'   # <meta property="article:published_time" content="2016-10-29T13:09+03:00"/>
#'   cas_extract_html(
#'     html_document = html_document,
#'     container = "meta",
#'     container_property = "article:published_time",
#'     attribute = "content"
#'   )
#' }
#' }
cas_extract_html <- function(html_document,
                             container = NULL,
                             container_class = NULL,
                             container_id = NULL,
                             container_name = NULL,
                             container_property = NULL,
                             container_itemprop = NULL,
                             container_instance = NULL,
                             attribute = NULL,
                             sub_element = NULL,
                             no_children = NULL,
                             trim = TRUE,
                             squish = FALSE,
                             no_match = "",
                             exclude_CSSpath = NULL,
                             remove_Xpath = NULL,
                             custom_Xpath = NULL,
                             custom_CSSpath = NULL,
                             keep_everything = FALSE,
                             extract_text = TRUE,
                             as_character = TRUE) {
  if (keep_everything == TRUE) {
    output <- html_document
  } else if (is.null(custom_Xpath) == FALSE) {
    output <- html_document %>%
      rvest::html_elements(xpath = custom_Xpath)
  } else if (is.null(custom_CSSpath) == FALSE) {
    output <- html_document %>%
      rvest::html_elements(css = custom_CSSpath)
  } else if (is.null(container_itemprop) == FALSE) {
    if (is.null(attribute)) {
      output <- html_document %>%
        rvest::html_elements(xpath = stringr::str_c(
          "//",
          container,
          "[@itemprop='",
          container_itemprop, "']"
        ))
    } else {
      output <- html_document %>%
        rvest::html_elements(xpath = stringr::str_c(
          "//",
          container,
          "[@itemprop='",
          container_itemprop, "']"
        ))
    }
  } else if (is.null(container_class) == TRUE & is.null(container_id) == TRUE) {
    if (is.null(container_name) == TRUE) {
      if (is.null(container_property)) {
        output <- html_document %>%
          rvest::html_elements(container)
      } else {
        output <- html_document %>%
          rvest::html_elements(xpath = stringr::str_c(
            "//",
            container,
            "[@property='",
            container_property, "']"
          ))
      }
    } else {
      if (is.null(attribute)) {
        output <- html_document %>%
          rvest::html_elements(xpath = stringr::str_c(
            "//",
            container,
            "[@name='",
            container_name, "']"
          ))
      } else {
        output <- html_document %>%
          rvest::html_elements(xpath = stringr::str_c(
            "//",
            container,
            "[@name='",
            container_name, "']"
          ))
      }
    }
  } else if (is.null(container_class) == FALSE & is.null(attribute) == FALSE) {
    if (is.null(container_name)) {
      output <- html_document %>%
        rvest::html_elements(xpath = stringr::str_c(
          "//",
          container
        ))
    } else {
      output <- html_document %>%
        rvest::html_elements(xpath = stringr::str_c(
          "//",
          container,
          "[@name='",
          container_name, "']"
        ))
    }
  } else if (is.null(container_class) == FALSE & is.null(container_id) == TRUE) {
    output <- html_document %>%
      rvest::html_elements(xpath = stringr::str_c(
        "//",
        container,
        "[@class='",
        container_class, "']"
      ))
  } else if (is.null(container_class) == TRUE & is.null(container_id) == FALSE) {
    output <- html_document %>%
      rvest::html_elements(xpath = stringr::str_c("//", container, "[@id='", container_id, "']"))
  }

  if (is.null(remove_Xpath) == FALSE) {
    xml2::xml_remove(output %>% xml2::xml_find_all(xpath = remove_Xpath))
  }

  if (is.null(attribute) == FALSE) {
    output <- output %>%
      rvest::html_attr(name = attribute)
  } else {
    if (is.null(exclude_CSSpath) == FALSE) {
      output <- output %>%
        rvest::html_elements(css = stringr::str_c(
          ":not(",
          exclude_CSSpath,
          ")"
        ))
    }

    if (is.null(sub_element) == FALSE) {
      output <- output %>%
        rvest::html_elements(sub_element)
    }

    if (extract_text == TRUE) {
      output <- output %>%
        rvest::html_text2()
    } else {
      if (as_character == TRUE) {
        output <- output %>%
          as.character()
      } else {
        return(output)
      }
    }
  }

  if (length(output) > 1) {
    if (is.null(container_instance) == FALSE) {
      output <- output[container_instance]
    } else {
      output <- stringr::str_c(output, collapse = "\n")
    }
  } else if (length(output) == 0) {
    output <- as.character(no_match)
  }

  if (trim == TRUE) {
    output <- stringr::str_trim(string = output)
  }

  if (squish == TRUE) {
    output <- stringr::str_squish(string = output)
  }

  output
}
