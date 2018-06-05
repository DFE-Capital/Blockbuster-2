
# this code snippet sets the R directory cell in the excel sheet to R.home/bin so the excel sheet can find Rscript.exe
# and sets the package folder in the excel sheet so the sheet can find the R script.
#
# It uses the xlsx package to do this
library(xlsx)

# load excel file into R
file <- "./inst/excel files/Excel input.xlsm"
wb <- loadWorkbook(file)

# Create object rows that contain the cells from the second sheet (Inputs)
sheets <- getSheets(wb)
sheet <- sheets[[2]]
rows <- getRows(sheet)

# set R.home
cell <- getCells(rows[13], 7) # create object containing the cell (row 13, col 7) that needs to contain the folder path
setCellValue(cell[[1]], R.home("bin"))

#set package
cell <- getCells(rows[15, 7]) # create object containing the cell (row 15, col 7) that needs to contain the folder path
setCellValue(cell[[1]], find.package("blockbuster2"))

# save the workbook
saveWorkbook(wb, file)

print(paste0("The file 'Excel input.xlsm' can be found in the folder ", find.package("blockbuster2"), ". To run the blockbuster model, copy this excel sheet to a project folder, amend the model parameters within the workbook as required, and run it using the button on the first worksheet."))


