excelExport = function(rObject) {
    library(excel.link)
    myExcelProcess = COMCreate("Excel.Application")
    xl.workbook.add()
    excelRange = myExcelProcess[["ActiveSheet"]]$Range("A1:A1")
    xl.write(rObject, excelRange, row.names = FALSE)
}