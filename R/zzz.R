.onAttach <- function(libname, pkgname){
  packageStartupMessage(paste0("Welcome to the Blockbuster deterioration model developed by Mat Gregory and Peter Curtis. The file 'Excel input.xlsm' can be found in the folder ", find.package("blockbuster2"), "/excel files. To run the blockbuster model, copy this excel sheet to a project folder, amend the model parameters within the workbook as required, and run it using the button on the first worksheet."))
}

globalVariables(c("elementid", "buildingid", "grade", "unit_area", "year",
                  "blockbuster_pds_repair_costs", "blockbuster_det_data",
                  "unit_area", ".", "gifa", "B.repair.total", "C.repair.total",
                  "D.repair.total", "E.repair.total", "B.block.repair.cost",
                  "C.block.repair.cost", "D.block.repair.cost",
                  "E.block.repair.cost", "ab", "bc", "cd", "de", "A", "B", "C",
                  "D", "E", "area", "backlog", "block.rebuild.cost", "strategy",
                  "ratio", "aes", "plot", "timestep", "B.repair.cost",
                  "C.repair.cost", "D.repair.cost", "E.repair.cost"))

.onLoad <- function(libname, pkgname){
  # on loading the package we change the R home and package folder cells in
  # the excel sheet within the package to the correct place
  # This means that if R or the package is updated or moved, the excel sheet
  # stays current.

  pkg <- find.package("blockbuster2")
  file <- file.path(pkg, "excel files/Excel input.xlsm")

  if(file.exists(file)){
    # load excel file into R
    wb <- loadWorkbook(file)

    # Create object rows that contain the cells from the second sheet (Inputs)
    sheets <- getSheets(wb)
    sheet <- sheets[[2]]
    rows <- getRows(sheet)

    # set R.home
    # create object containing the cell (row 11, col 2) to contain the path to Rscript
    cell1 <- getCells(rows[10], 2)
    setCellValue(cell1[[1]], R.home("bin"))


    #set package
    # create object containing the cell (row 12, col 2) to contain the path to the blockbuster2 library
    cell2 <- getCells(rows[11], 2)
    setCellValue(cell2[[1]], pkg)

    #set simulated data path
    # create object containing the cell (row 1, col 2) to contain the path to simulated data
    cell3 <- getCells(rows[1], 2)
    setCellValue(cell3$`1.2`, file.path(pkg, "excel files", "simulated_data.rds"))

    # save the workbook
    saveWorkbook(wb, file)

  } else {
    packageStartupMessage("The excel input file within the package has not been updated. If you use the excel front end, please make sure the workbook contains the correct paths for R.home and the blockbuster2 package.")
  }

}
