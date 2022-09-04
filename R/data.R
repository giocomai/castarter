#' Empty data frame with the same format as data stored in the `index_id` table
#'
#' @format A data frame with 0 rows and 3 columns:
#' \describe{
#'   \item{id}{Numeric. Column meant for unique integer identifier corresponding to a unique url}
#'   \item{url}{Character. A url.}
#'   \item{type}{Character. A textual string, by default `index`.}
#' }
"casdb_empty_index_id"
