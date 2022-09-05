#' Downloads html pages based on a vector of links
#'
#' Downloads html pages based on a vector of links.
#'
#' @param url A character vector of urls, or a data frame with at least two columns named `id` and `url`.
#' @param type Accepted values are either "contents" (default), "index".
#' @param custom_folder Defaults to NULL. If given, overrides the "type" param and stores files in given path as a subfolder of project/website. Folder must already exist, and should be empty.
#' @param path Defaults to NULL. If given, overrides the "type" and "custom_folder" param and stores files in given path.
#' @param wait Defaults to 1. Number of seconds to wait between downloading one page and the next. Can be increased to reduce server load, or can be set to 0 when this is not an issue.
#' @param project Name of 'castarter2' project. Must correspond to the name of a folder in the current working directory.
#' @param website Name of a website included in a 'castarter2' project. Must correspond to the name of a sub-folder of the project folder.
#' @param method Defaults to "auto". Method is passed to the function utils::download.file(); available options are "internal", "wininet" (Windows only) "libcurl", "wget" and "curl". For more information see ?utils::download.file()
#' @param missing_pages Logical, defaults to TRUE. If TRUE, verifies if a downloaded html file exists for each element in articlesLinks; when there is no such file, it downloads it.
#' @param url_to_download Defaults to NULL. If given, expected to be a logical vector to be applied to the given urls. If given, it takes precedence over `missing_pages` and `size`.
#' @param size Defaults to 500. It represents the minimum size in bytes that downloaded html files should have: files that are smaller will be downloaded again. Used only when missing_pages == FALSE.
#' @param wget_system Logical, defaults to FALSE. Calls wget as a system command through the system() function. Wget must be previously installed on the system.
#' @param start Integer. Only url with position higher than start in the url vector will be downloaded: `url[start:length(url)]`
#' @param ignore_ssl_certificates Logical, defaults to FALSE. If TRUE it uses wget to download the page, and does not check if the SSL certificate is valid. Useful, for example, for https pages with expired or mis-configured SSL certificate.
#' @param use_headless_chromium Logical, defaults to FALSE. If TRUE uses the `crrri` package to download pages. Useful in particular when web pages are generated via javascript. See in particular: https://github.com/RLesur/crrri#system-requirements
#' @param headless_chromium_wait Numeric, in seconds. How long should headless chrome wait after loading page?
#' @param create_script Logical, defaults to FALSE. Tested on Linux only. If TRUE, creates a downloadPages.sh executable file that can be used to download all relevant pages from a terminal.
#' @return By default, returns nothing, used for its side effects (downloads html files in relevant folder). Download files can then be imported in a vector with the function ImportHtml.
#' @export
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   cas_download(url)
#' }
#' }
#'
cas_download <- function(url,
                         type = "contents",
                         custom_folder = NULL,
                         custom_path = NULL,
                         file_format = "html",
                         url_to_download = NULL,
                         size = 500,
                         wget_system = FALSE,
                         method = "auto",
                         missing_pages = TRUE,
                         start = 1,
                         wait = 1,
                         ignore_ssl_certificates = FALSE,
                         use_headless_chromium = FALSE,
                         headless_chromium_wait = 1,
                         use_phantomjs = FALSE,
                         create_script = FALSE,
                         project = NULL,
                         website = NULL,
                         base_folder = NULL) {
  if (use_headless_chromium == TRUE) {
    if (requireNamespace("crrri", quietly = TRUE) == FALSE) {
      stop("You need to install the `crrri` package to download pages with headless chrome/chromium. For details, see: https://github.com/RLesur/crrri. Make sure to read the note on system requirements: https://github.com/RLesur/crrri#system-requirements")
    }
  }

  if (is.data.frame(url) == FALSE) {
    url_df <- tibble::tibble(
      id = seq_along(url),
      url = url
    )
  } else {
    if (sum(c("id", "url") %in% names(url)) == 2) {
      url_df <- url
    } else {
      usethis::ui_stop("url must either be a character vector or a data frame with at least two columns named id and url")
    }
    if (is.numeric(url_df$id) == FALSE) {
      usethis::ui_stop("If given, the {usethis::ui_code('id')} column must be numeric.")
    }
  }



  if (fs::file_exists(path) == FALSE) {
    fs::dir_create(path = path)
    usethis::ui_info(stringr::str_c("The folder",
      usethis::ui_path(path),
      "has been created.",
      sep = " "
    ))
  }


  if (is.null(url_to_download) == TRUE) {
    if (missing_pages == TRUE) {


    } else if (missing_pages == FALSE) {
      smallFiles <- htmlFilesList[htmlFileSize < size]
      smallFilesId <- as.integer(stringr::str_extract(
        string = smallFiles,
        pattern = paste0("[[:digit:]]+[[:punct:]]", file_format)
      ) %>%
        stringr::str_sub(start = 1L, end = -(nchar(file_format) + 2)))
      url_to_download <- rep(x = FALSE, times = length(url))
      url_to_download[smallFilesId] <- TRUE
      if (is.null(url_to_check) == FALSE) {
        url_to_download <- Reduce(f = "&", x = list(url_to_download, url_to_check))
      }
    }
  } else if (is.null(url_to_download) == FALSE) {
    # do nothing
  }
  url_to_download[1:start - 1] <- FALSE

  temp <- 1
  if (create_script == TRUE) {
    wget_system <- TRUE
  }
  if (wget_system == TRUE) {
    if (create_script == TRUE) {
      if (file.exists(fs::path(baseFolder, project, website, "downloadPages.sh")) == TRUE) {
        file.remove(fs::path(baseFolder, project, website, "downloadPages.sh"))
      }
      write(
        x = paste0(
          "wget '", url[url_to_download], "' -O '",
          fs::path("..", "..", "..", htmlFilePath, paste0(articlesId[url_to_download], ".", file_format)), "'",
          " -t 1 -T 20", "; ", "sleep ", wait
        ),
        file = fs::path(baseFolder, project, website, "downloadPages.sh"), append = TRUE
      )
      system(paste("chmod +x", fs::path(baseFolder, project, website, "downloadPages.sh")))
    } else {
      options(useFancyQuotes = FALSE)
      for (i in url[url_to_download]) {
        articleId <- articlesId[url_to_download][temp]
        system(paste("wget '", i, "' -O", fs::path(htmlFilePath, paste0(articleId, ".", file_format))))
        message(paste("Downloaded item", temp, "of", length(url[url_to_download]), ". ID: ", articleId), quote = FALSE)
        temp <- temp + 1
        Sys.sleep(wait)
      }
    }
  } else {
    for (i in url[url_to_download]) {
      articleId <- articlesId[url_to_download][temp]
      if (use_headless_chromium == TRUE) {
        # based on example from: https://github.com/RLesur/crrri
        crrri::perform_with_chrome(function(client) {
          Network <- client$Network
          Page <- client$Page
          Runtime <- client$Runtime
          Network$enable() %...>%
            {
              Page$enable()
            } %...>%
            {
              Network$setCacheDisabled(cacheDisabled = TRUE)
            } %...>%
            {
              Page$navigate(url = i)
            } %...>%
            {
              Page$loadEventFired()
            } %>% wait(delay = headless_chromium_wait) %...>%
            {
              Runtime$evaluate(
                expression = "document.documentElement.outerHTML"
              )
            } %...>% (function(result) {
              writeLines(
                text = result$result$value,
                con = fs::path(
                  htmlFilePath,
                  paste0(articleId, ".", file_format)
                ),
                sep = "\n"
              )
            })
        })
      } else if (use_phantomjs == TRUE) {
        options(useFancyQuotes = FALSE)
        if (fs::file_exists("save.js") == FALSE) {
          download.file(url = "https://raw.githubusercontent.com/giocomai/castarter/master/inst/save.js", destfile = "save.js")
        }
        system(paste("phantomjs save.js", sQuote(i), fs::path(htmlFilePath, paste0(articleId, ".", file_format))))
      } else {
        if (ignore_ssl_certificates == TRUE) {
          try(utils::download.file(url = i, destfile = fs::path(htmlFilePath, paste0(articleId, ".", file_format)), method = "wget", extra = "--no-check-certificate"))
        } else {
          try(utils::download.file(
            url = i,
            destfile = fs::path(
              htmlFilePath,
              paste0(
                articleId,
                ".",
                file_format
              )
            ),
            method = method
          ))
        }
      }
      message(paste("Downloaded item", temp, "of", length(url[url_to_download]), ". ID: ", articleId))
      temp <- temp + 1
      Sys.sleep(wait)
    }
  }
}



#' Downloads files, and stores details about the download in a local database
#'
#' @param download_df A data frame with one row, and four columns: `id`, `url`, `path`, `type`.
#' @param type Accepted values are either "contents" (default) oe "index".
#' @param overwrite_file Logical, defaults to FALSE.
#'
#' @return Invisibly returns the full `httr` response.
#' @importFrom cas_write_to_db
#' @export
#'
#' @examples
cass_download_httr <- function(download_df,
                               type = "contents",
                               overwrite_file = FALSE,
                               wait = 1,
                               use_db = NULL,
                               db_connection = NULL,
                               disconnect_db = TRUE) {
  pb <- progress::progress_bar$new(total = nrow(download_df))

  purrr::walk(
    .x = purrr::transpose(download_df),
    .f = function(x) {
      pb$tick()
      if (fs::file_exists(x$path) == FALSE | overwrite_file == TRUE) {
        raw <- httr::GET(url = x$url, httr::write_disk(path = x$path, overwrite = overwrite_file))

        info_df <- tibble::tibble(
          id = x$id,
          datetime = Sys.time(),
          status = raw$status_code,
          size = fs::file_size(x$path)
        )

        cas_write_to_db(
          df = info_df,
          table = stringr::str_c(type, "_", "download"),
          use_db = use_db,
          overwrite = FALSE,
          db_connection = db_connection,
          disconnect_db = disconnect_db
        )
        Sys.sleep(time = wait)
      }
    }
  )
}



#' Create a data frame with not yet downloaded files
#'
#' @param url_df Defaults to NULL. If given, with at least two columns named `id` and `url`. If not given, an attempt will be made to load it from the local database.
#'
#' @importFrom cas_download
#'
#' @return A data frame with four columns: `id`, `url`, `path` and `type`
#' @export
#'
#' @examples
cass_get_files_to_download <- function(url_df = NULL,
                                       type = "contents",
                                       custom_folder = NULL,
                                       custom_path = NULL,
                                       file_format = "html",
                                       project = NULL,
                                       website = NULL,
                                       base_folder = NULL,
                                       use_db = NULL,
                                       db_connection = NULL,
                                       disconnect_db = TRUE) {
  if (is.null(url_df)) {
    if (type == "contents") {
      url_df <- cas_read_db_contents(
        use_db = use_db,
        db_connection = db_connection,
        disconnect_db = disconnect_db
      )
    } else if (type == "index") {
      url_df <- cas_read_db_index(
        use_db = use_db,
        db_connection = db_connection,
        disconnect_db = disconnect_db
      )
    } else {
      usethis::ui_stop("Parameter `type` should be either `index` or `contents`.")
    }
  }


  if (is.null(custom_path)) {
    if (is.null(custom_folder) == FALSE) {
      path <- fs::path(
        cas_get_options(base_folder = base_folder, project = project, website = website)$base_folder,
        cas_get_options(base_folder = base_folder, project = project, website = website)$project,
        cas_get_options(base_folder = base_folder, project = project, website = website)$website,
        stringr::str_c(file_format, "_", custom_folder)
      )
    } else if (type == "contents" | type == "index") {
      path <- fs::path(
        cas_get_options(base_folder = base_folder)$base_folder,
        cas_get_options(project = project)$project,
        cas_get_options(website = website)$website,
        stringr::str_c(file_format, "_", type)
      )
    } else {
      usethis::ui_stop("Parameter `type` should be either `index` or `contents`.")
    }
  }

  if (fs::file_exists(path) == FALSE) {
    fs::dir_create(path = path)
    usethis::ui_info(stringr::str_c("The folder",
      usethis::ui_path(path),
      "has been created.",
      sep = " "
    ))
  }


  previous_files_df <- fs::dir_info(path = path) %>%
    dplyr::transmute(path,
      size,
      filename = fs::path_file(path)
    ) %>%
    dplyr::mutate(id = stringr::str_extract(
      string = filename,
      pattern = "[[:digit:]]+"
    ) %>%
      as.numeric()) %>%
    dplyr::arrange(id)

  expected_filenames_df <- tibble::tibble(
    id = url_df$id,
    path = fs::path(
      path,
      stringr::str_c(url_df$id, ".", file_format)
    )
  )
  files_to_download_df <- dplyr::anti_join(
    x = expected_filenames_df,
    y = previous_files_df,
    by = "path"
  )

  urls_to_download_df <- dplyr::left_join(
    x = files_to_download_df,
    y = url_df,
    by = "id"
  ) %>%
    dplyr::select(.data$id, .data$url, .data$path)

  urls_to_download_df
}
