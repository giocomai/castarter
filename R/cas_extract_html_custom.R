#' Facilitates extraction of contents from an html file
#'
#' @param container_match String to be used for filtering nodes in combination with `container_type`.
#' @inheritParams cas_extract_html
#'
#' @returns A character vector.
#' @export
#'
#' @examples
#' \dontrun{
#' ## extract a canonical link
#' cas_extract_html_custom(
#'  html_document = x,
#'  container = "link",
#'  container_type = "rel",
#'  container_name = "canonical",
#'  attribute = "href"
#'  )
#' }
cas_extract_html_custom <- function(
  html_document,
  container,
  container_type,
  container_match,
  attribute = NULL
) {
  xml_nodeset <- rvest::html_elements(
    x = html_document,
    xpath = stringr::str_c(
      "//",
      container,
      "[@",
      container_type,
      "='",
      container_match,
      "']"
    )
  )

  if (!is.null(attribute)) {
    output <- xml_nodeset |>
      xml2::xml_attr(attribute)
  } else {
    output <- xml_nodeset |>
      rvest::html_text2()
  }

  if (length(output) == 0) {
    return(NA_character_)
  } else {
    output
  }
}
