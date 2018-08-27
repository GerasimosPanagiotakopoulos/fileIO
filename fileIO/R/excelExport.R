#' A "quick and dirty" method to quickly export a dataset to a live Excel worksheet
#' @param rObject The object to export
#' @param fixEncodingIssue [logical]: Sometimes an issue to encoding is observed, when exporting greek characters. Setting this as TRUE resolves it.
#' @export

excelExport = function(rObject, fixEncodingIssue = FALSE) {
    library(excel.link)
    myExcelProcess = COMCreate("Excel.Application")
    xl.workbook.add()
    excelRange = myExcelProcess[["ActiveSheet"]]$Range("A1:A1")

    if (fixEncodingIssue) {
        library(stringi)
        fixVectorEncoding = function(x) {
            x = stri_encode(x, from = "UTF-8", to = "")
        }

        if (any(c("data.table", "data.frame", "matrix") %in% class(rObject))) {
            library(data.table)
            y = as.data.table(copy(rObject))
            for (element in names(y)) {
                if (y[, mode(get(element))] != "character") next
                y[, (element) := fixVectorEncoding(get(element))]
            }
        } else if ("list" == class(rObject)) {
            y = copy(rObject)
            for (i in 1:length(y)) {
                y[[i]] = fixVectorEncoding(y[[i]])
            }
        } else {
            y = fixVectorEncoding(copy(rObject))
        }
        rObject = y
    }

    xl.write(rObject, excelRange, row.names = FALSE)
}
