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

# If output files exist in temp try to copy them to working directory
try(file.copy(res$temp_excel, res$excel))
try(file.copy(res$temp_doc, res$doc))

# Create error or success message in blockbuster_out file
if(file.exists(file.path(temp, "blockbuster error.txt"))){

  log_line("The deterioration model has not completed successfully")
  writeLines("Blockbuster has returned an error.", results_file)

  # copy error log to working directory
  try(file.copy(file.path(temp, "blockbuster error.txt"),
                file.path(args[2], "blockbuster error.txt")))

  # write appropriate path to excel output text
  if(file.exists(file.path(args[2], "blockbuster error.txt"))) {
    writeLines(
      paste0("See ", file.path(args[2], "blockbuster error.txt"), " for more details"),
      results_file)
    } else {
      writeLines(
        paste0("See ", file.path(temp, "blockbuster error.txt"), " for more details"),
        results_file)
      }
  } else {
    writeLines("Blockbuster has run successfully.", results_file)
  }

if(file.exists(res$excel)){
  writeLines(paste0("Model output is available at ", res$excel), results_file)
} else if(file.exists(res$temp_excel)){
  writeLines(paste0("The file ", res$temp_excel, " contains some model output."), results_file)
}
if(file.exists(res$doc)){
  writeLines(paste0("High level output summary is available at ", res$doc), results_file)
} else if(file.exists(res$temp_doc)) writeLines(paste0("The high-level summary is available at ", temp_doc), results_file)

log_line(paste0("Finished at ", Sys.time()))
