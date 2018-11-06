# This script is run when the button is pushed on Excel input.xlsm.
# It loads the input parameters from the excel sheet and runs the blockbuster model.

library(blockbuster2)
library(readxl)

# get temp folder
temp <- Sys.getenv("TEMP")

# set up text file for output messages
log_file <- file(file.path(temp, paste0("blockbuster_log_", format(Sys.time(), "%a%d%b%Y %H%M"), ".txt")), open = "wt")
results_file <- file(file.path(temp, "blockbuster_out.txt"), open = "wt")

# helper function to print to text log
log_line <- function(x){
  writeLines(paste(Sys.time(), x, sep = " - "), log_file)
  cat(x, sep = "\n")
}

log_line(paste0("Started at ", Sys.time()))

log_line("Loading arguments")
args <- commandArgs(trailingOnly = TRUE)

log_line(paste0("Input excel sheet is at ", args[3]))

log_line("Running blockbuster")

# remove existing error log
file.remove(file.path(temp, "blockbuster error.txt"))
res <- try(blockbuster_excel(args[2]), outFile = file.path(temp, "blockbuster error.txt"))

if(file.exists(file.path(temp, "blockbuster error.txt"))){
  file.copy(file.path(temp, "blockbuster error.txt"),
            file.path(args[2], "blockbuster error.txt"))
  log_line(paste0("Blockbuster returned error, see ", file.path(args[2], "blockbuster error.txt"), " for more details"))
  writeLines("Blockbuster has returned an error.", results_file)
  writeLines(paste0("See ", file.path(args[2], "blockbuster error.txt"), " for more details"), results_file)
} else {
  writeLines("Blockbuster has run successfully.", results_file)
  writeLines(paste0("A high level overview can be found in ", res$doc), results_file)
  writeLines(paste0("More detailed data is in ", res$excel), results_file)
}

log_line(paste0("Finished at ", Sys.time()))
