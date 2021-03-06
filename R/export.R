#' Exports all articles that contain a given term.
#'
#' @param dataset A dataset created with 'castarter'.
#' @param term The term that determines which articles are exported.Must be a character vector of length = 1.
#' @param onlyNaDates Logical, defaults to FALSE. Used to for troubleshooting dataset.
#' @param txt Logical, defaults to TRUE. If TRUE, exports all articles including the term provided in a .txt file.
#' @param csv Logical, defaults to FALSE. If TRUE, exports all articles including the term provided in a .csv file.
#' @param data.frame Logical, defaults to FALSE. If TRUE, the function outputs a data frame. If FALSE, function used only for its side effects (i.e. exporting to files)
#' @param project Name of 'castarter' project. Must correspond to the name of a folder in the current working directory.
#' @param website Name of a website included in a 'castarter' project. Must correspond to the name of a sub-folder of the project folder.Defaults to NULL. If no website is provided, exported files are saved in the project/Outputs folder.
#' @export
#' @examples
#' \dontrun{
#' ExportArticlesWith(dataset, "example")
#' }

ExportArticlesWith <- function(dataset, term, includeNaDates = TRUE, onlyNaDates = FALSE, txt = TRUE, csv = FALSE, xlsx = FALSE, data.frame = FALSE, includeOnly = NULL, sortBy = "date", project = NULL, website = NULL) {
    if (is.null(project) == TRUE) {
        project <- CastarterOptions("project")
    }
    if (is.null(website) == TRUE) {
        website <- CastarterOptions("website")
    }
    if (is.null(CastarterOptions("baseFolder"))) {
        baseFolder <- "castarter"
    } else {
        baseFolder <- CastarterOptions("baseFolder")
    }
    if (onlyNaDates == TRUE) {
        dataset <- dataset[is.na(dataset$date),]
    }
    if (includeNaDates == FALSE) {
        dataset <- dataset[is.na(dataset$date)==FALSE,]
    }
    # Export only items that include...
    tempDataset <- dataset[grep(term, dataset$text, ignore.case = TRUE), ]
    if (is.null(includeOnly) == FALSE) {
        tempDataset <- tempDataset[tempDataset$website == includeOnly, ]
    }
    if (sortBy == "date") {
        tempDataset <- tempDataset[order(tempDataset$date), ]
    }
    if (is.null(website) == TRUE) {
        if (!file.exists(file.path(baseFolder, project, "Outputs"))) {
            dir.create(file.path(baseFolder, project, "Outputs"))
        }
    } else {
        if (!file.exists(file.path(baseFolder, project, website, "Outputs"))) {
            dir.create(file.path(baseFolder, project, website, "Outputs"))
        }
    }
    if (csv == TRUE) {
        if (is.null(website) == TRUE) {
            utils::write.csv(tempDataset, file.path(baseFolder, project, "Outputs", paste(term, " in ", website, ".csv", sep = "")))
            print(paste("File .csv exported to:", file.path(baseFolder, project, "Outputs", paste(term, " in ", website, ".csv", sep = ""))))
        } else {
            utils::write.csv(tempDataset, file.path(baseFolder, project, website, "Outputs", paste(term, " in ", website, ".csv", sep = "")))
            print(paste("File .csv exported to:", file.path(baseFolder, project, website, "Outputs", paste(term, " in ", website, ".csv", sep = ""))))
        }
    }
    if (txt == TRUE) {
        if (is.null(website) == TRUE) {
            writeLines(paste(paste("Date:", tempDataset$date), paste("Title:", tempDataset$title), paste("Link:", tempDataset$link), paste("ID:", tempDataset$id), tempDataset$text, " ___  ______  ______  ______  ______  ______  ______  ______  ______  ___\n  __)(__  __)(__  __)(__  __)(__  __)(__  __)(__  __)(__  __)(__  __)(__\n (______)(______)(______)(______)(______)(______)(______)(______)(______)\n", sep = "\n"), file.path(baseFolder, project, "Outputs", paste(term, " in ", ".txt", sep = "")))
            print(paste("File .txt exported to:", file.path(baseFolder, project, "Outputs", paste(term, " in ", project, ".txt", sep = ""))))
        } else {
            writeLines(paste(paste("Date:", tempDataset$date), paste("Title:", tempDataset$title), paste("Link:", tempDataset$link), paste("ID:", tempDataset$id), tempDataset$text, " ___  ______  ______  ______  ______  ______  ______  ______  ______  ___\n  __)(__  __)(__  __)(__  __)(__  __)(__  __)(__  __)(__  __)(__  __)(__\n (______)(______)(______)(______)(______)(______)(______)(______)(______)\n", sep = "\n"), file.path(baseFolder, project, website, "Outputs", paste(term, " in ", website, ".txt", sep = "")))
            print(paste("File .txt exported to:", file.path(baseFolder, project, website, "Outputs", paste(term, " in ", website, ".txt", sep = ""))))
        }
    }
    if (data.frame == TRUE) {
        tempDataset
    }
}
