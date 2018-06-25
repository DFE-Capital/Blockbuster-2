# This script is run when the button is pushed on Excel input.xlsm.
# It loads the input parameters from the excel sheet and runs the blockbuster model.

library(blockbuster2)
library(readxl)


message ("loading arguments")
args <- commandArgs(trailingOnly = TRUE)

working_dir <- file.path(args[1])
message(paste0("running blockbuster from ", working_dir))

zz <- file(file.path(working_dir, paste0(format(Sys.time(), "%h%m%d%b%y"), "log.txt")), open = "wt")
sink(file = zz, type = "message", split = TRUE)

message(paste0("Started at", Sys.time()))

message("Running blockbuster")
try(blockbuster_excel(working_dir), outFile = file.path(working_dir, "blockbuster error.txt"))

message(paste0("Finished at", Sys.time()))

sink()
