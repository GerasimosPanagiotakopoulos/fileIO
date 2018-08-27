#' This function saves a dataset to a file (formats supported: xlsx, csv, txt)
#' @param rObject The object to export
#' @param fullFileName The name of the file to be saved
#' @param separator The field separator (only applicable for csv and txt)
#' @param decimal The decimal point (only applicable for csv and txt)
#' @param header if TRUE, the dataset names will be exported as well
#' @export


saveAs = function(rObject, fullFileName = paste0(getwd(), "/Book1.xlsx"), separator = ";", decimal = ".", header = TRUE, versionDepth = 0, incrementVersion = FALSE) {

    path = dirname(fullFileName)
    fileName = tools::file_path_sans_ext(basename(fullFileName))
    extension = tools::file_ext(fullFileName)

    stopifnot(extension %in% c("xlsx", "csv", "txt"))

    if (path == "") {
        warning(paste0("Path not specified. File will be saved at ", getwd()))
    }

    # Keep versioning
    if (versionDepth > 0) {
        # similarFilesList = list.files(path = path, pattern = paste0(fileName, "_v(?:[:digit:]+\\.){0,", versionDepth - 1, "}[:digit:]\\.", extension, "$"))
        similarFilesList = list.files(path = path)
        similarFilesList = str_subset(similarFilesList, pattern = paste0(fileName, "_v(?:[:digit:]+\\.){0,", versionDepth - 1, "}[:digit:]\\.", extension, "$"))
        similarFiles = sapply(similarFilesList, tools::file_path_sans_ext)
        if (length(similarFiles) > 0) {
            mostUpdatedFile = sort(similarFiles, decreasing = TRUE)[1]
        } else {
            mostUpdatedFile = paste0(fileName, "_v0", paste0(rep(".0", versionDepth - 1), collapse = ""))
        }
        if (incrementVersion) {
            library(stringr)
            if (str_detect(mostUpdatedFile, paste0("[:digit:]+(?:\\.[:digit:]+){", versionDepth - 1, "}$"))) {
                latestSubVersionNumber = as.numeric(str_extract(mostUpdatedFile, "[:digit:]+$"))
                latestSubVersionCore = str_extract(mostUpdatedFile, ".*(?=_v?)")
                newFileName = paste0(latestSubVersionCore, "_v", latestSubVersionNumber + 1)
            } else {
                latestSubVersionDepth = str_count(tail(str_split(mostUpdatedFile, ".*_v")[[1]], 1), "\\.")
                newFileName = paste0(mostUpdatedFile, paste0(rep(".0", versionDepth - latestSubVersionDepth - 1), collapse = ""))
            }
        } else {
            newFileName = mostUpdatedFile
        }
        fullFileName = paste0(c(path, paste0(newFileName, ".", extension)), collapse = "/")
    }

    if (extension %in% c("csv", "txt")) {
        suppressWarnings(write.csv(x = rObject, file = fullFileName, sep = separator, dec = decimal, row.names = FALSE, col.names = header))
    }

    if (extension == "xlsx") {
        library(excel.link)
        xl.save.file(filename = fullFileName, r.obj = rObject, row.names = FALSE, col.names = header)
    }
}



