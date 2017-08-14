saveAs = function (rObject, path = getwd(), fullFileName = paste0(getwd(), "/Book1.xlsx", sep = ";")) {

    path = dirname(fullFileName)
    fileName = tools::file_path_sans_ext(basename(fullFileName))
    extension = tools::file_ext(fullFileName)

    stopifnot(extension %in% c("xlsx", "csv"))

    if (extension == "csv") {
        suppressWarnings(write.csv(x = rObject, file = fullFileName, sep = sep, dec = ".", row.names = FALSE))
    }

    if (extension == "xlsx") {
        library(excel.link)
        xl.save.file(filename = fullFileName, r.obj = rObject, row.names = FALSE)
    }
}