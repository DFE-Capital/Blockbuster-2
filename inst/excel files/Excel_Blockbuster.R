# This script is run when the button is pushed on Excel input.xlsm.
# It loads the input parameters from the excel sheet and runs the blockbuster model.

library(blockbuster2)
library(readxl)

print ("loading functions")
args <- commandArgs(trailingOnly = TRUE)

working_dir <- file.path(args[1])

sink(file = file.path(working_dir, "log.txt"), type = "message")

time <- Sys.Date()
print(paste0("running blockbuster from ", working_dir))

blockbuster_excel(file.path(working_dir, "Excel input.xlsm"))

message("Finished")

sink()
