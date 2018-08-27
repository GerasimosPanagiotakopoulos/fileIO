#' This function reads data from a file. It provides the additional option of "caching" the result to an Rds file for faster future importing.
#' @param fullFileName The name of the file to be read
#' @param sheet The sheet name to be read (can be provided either as the sheet index or as the sheet name)
#' @param startRow The row index where the dataset begins
#' @param cache if TRUE, the dataset will be cached in an Rds file for faster future importing
#' @param update if TRUE, the dataset will ignore any caches and will read the file from scratch
#' @param header if TRUE, the first row is treated as the dataset names
#' @param separator the field separator (only applicable for flat files)
#' @param decimal the decimal point used (only applicable for flat files)
#' @param format the desired class of the output (supported formats: data.table, data.frame, tibble)
#' @return the dataset in the requested format (default is data.table)
#' @export
fileImport = function(fullFileName, sheet = 1, startRow = 1, cache = FALSE, update = FALSE, header = TRUE, separator = ";", decimal = ".", format = "data.table") {

    # Break file name into compοnents
    path = dirname(fullFileName)
    fileName = tools::file_path_sans_ext(basename(fullFileName))
    extension = tools::file_ext(fullFileName)

    # Sanity checks
    if (extension == "") {
        print("Error! File extension is missing! Please check the fullFileName variable.")
        stop()
    }

    stopifnot(extension %in% c("xlsx", "xls", "csv", "dsv", "xlsm", "txt", "tsv"))

    stopifnot(format %in% c("data.table", "data.frame", "tibble"))

    if (path == "") {
        warning(paste0("No path provided. Attempt to read file in ", getwd()))
        path = getwd()
        fullFileName = paste0(path, "/", fileName)
    }

    if (is.numeric(sheet)) {
        sheetIndex = as.numeric(sheet)
    } else {
        if (extension %in% c("xlsx", "xls", "xlsm")) {
            sheetNames = openxlsx::getSheetNames(fullFileName)
            stopifnot(sheet %in% sheetNames)
            sheetIndex = which(sheetNames == sheet)
        } else {
            sheetIndex = 1
        }
    }

    if (extension %in% c("xlsx", "xls", "csv", "dsv", "xlsm", "tsv")) {
        rdsFileName = paste(c(fileName, extension, sheetIndex), collapse = "_")# fix this to include options
    }
    else {
        if (extension == "txt") {
            rdsFileName = paste(c(fileName, extension), collapse = "_")
        }
    }

    fullRdsFileName = paste(path, paste0(rdsFileName, ".Rds"), sep = "/")
    if (!file.exists(fullRdsFileName) || update) {
        # Read file
        if (extension %in% c("csv", "dsv", "tsv", "txt")) {
            library(data.table)
            readData = data.table::fread(input = fullFileName, header = header, sep = separator, dec = decimal)
        }
        if (extension %in% c("xlsx", "xlsm")) {
            library(openxlsx)
            readData = openxlsx::read.xlsx(xlsxFile = fullFileName, sheet = sheetIndex, startRow = startRow, colNames = header)
        }
        if (extension == "xls") {
            library(xlsx)
            readData = xlsx::read.xlsx(file = fullFileName, sheetIndex = sheetIndex, startRow = startRow, header = header)
        }
        # if (extension == "txt") {
        #     readData = utils::read.table(file = fullFileName, header = header, sep = separator, dec = decimal)
        # }

        # Ensure class is appropriate
        if (format == "data.table") {
            library(data.table)
            readData = as.data.table(readData)
        } else if (format == "tibble") {
            library(tibble)
            readData = as.tibble(readData)
        } else {
            readData = as.data.frame(readData)
        }

        # Save read data if asked to
        if (cache) {
            saveRDS(object = readData, file = fullRdsFileName)
        }
    }
    else {
        readData = readRDS(file = fullRdsFileName)
    }
    return(readData)
}
