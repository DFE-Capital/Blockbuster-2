.onAttach <- function(libname, pkgname){
  packageStartupMessage("Welcome to the Blockbuster deterioration model developed by Mat Gregory and Peter Curtis.")
}

globalVariables(c("elementid", "buildingid", "grade", "unit_area",
                  "blockbuster_pds_repair_costs", "blockbuster_det_data",
                  "unit_area", ".", "gifa", "B.repair.total", "C.repair.total",
                  "D.repair.total", "E.repair.total", "B.block.repair.cost",
                  "C.block.repair.cost", "D.block.repair.cost",
                  "E.block.repair.cost", "ab", "bc", "cd", "de", "A", "B", "C",
                  "D", "E", "area", "backlog", "block.rebuild.cost", "strategy",
                  "ratio", "aes", "plot", "timestep"))

.onLoad <- function(libname, pkgname){
  # change the R home and package folder cells in excel sheet to the correct place
  # load excel file into R

  pkg <- find.package("Blockbuster2")

  file <- file.path(pkg, "excel files/Excel input.xlsm")
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
  setCellValue(cell[[1]], find.package("Blockbuster2"))

  # save the workbook
  saveWorkbook(wb, file)

  packageStartupMessage(paste0("The file 'Excel input.xlsm' can be found in the folder ", find.package("Blockbuster2"), "/excel files. To run the blockbuster model, copy this excel sheet to a project folder, amend the model parameters within the workbook as required, and run it using the button on the first worksheet."))

}
