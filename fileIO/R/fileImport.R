fileImport = function (fullFileName, sheet = 1, startRow = 1, reRead = FALSE, saveToRData = TRUE, header = FALSE, separator = ";", decimal = ".", format = "data.table") {

    path = dirname(fullFileName)
    fileName = tools::file_path_sans_ext(basename(fullFileName))
    extension = tools::file_ext(fullFileName)

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
        rdsFileName = paste(c(extension, extension, sheetIndex), collapse = "_")# fix this to include options
    }
    else {
        if (extension == "txt") {
            rdsFileName = paste(c(fileName, extension), collapse = "_")
        }
    }

    fullRdsFileName = paste(path, paste0(rdsFileName, ".Rds"), sep = "/")
    if (!file.exists(fullRdsFileName) || reRead) {
        if (extension %in% c("xlsx", "xlsm")) {
            library(openxlsx)
            readData = openxlsx::read.xlsx(xlsxFile = fullFileName, sheet = sheetIndex, startRow = startRow, colNames = header)
        }
        else {
            if (extension == "xls") {
                library(xlsx)
                readData = xlsx::read.xlsx(file = fullFileName, sheetIndex = sheetIndex, startRow = startRow, header = header)
            }
            if (extension %in% c("csv", "dsv", "tsv") {
                library(data.table)
                readData = data.table::fread(input = fullFileName, header = header, sep = separator, dec = decimal)
            }
            if (extension == "txt") {
                readData = utils::read.table(file = fullFileName, header = header, sep = separator, dec = decimal)
            }
        }
        if (asDataTable) {
            library(data.table)
            readData = as.data.table(readData)
        }
        if (saveToRData) {
            saveRDS(object = readData, file = fullRdsFileName)
        }
    }
    else {
        readRDS(file = fullRdsFileName)
    }
    return(readData)
}
