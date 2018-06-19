# This script is run when the button is pushed on Excel input.xlsm.
# It loads the input parameters from the excel sheet and runs the blockbuster model.

library(blockbuster2)
library(readxl)

message ("loading arguments")
args <- commandArgs(trailingOnly = TRUE)

working_dir <- file.path(args[1])
message(paste0("running blockbuster from ", working_dir))

message("Running blockbuster")
try(blockbuster_excel(working_dir), outFile = file.path(working_dir, "blockbuster error.txt"))

message("Finished")

