# This script is run when the button is pushed on Excel input.xlsm.
# It loads the input parameters from the excel sheet and runs the blockbuster model.

library(blockbuster2)
library(readxl)

print ("loading functions")
args <- commandArgs(trailingOnly = TRUE)

working_dir <- file.path(args[1])

time <- Sys.Date()
print(paste0("running blockbuster from ", working_dir))

results <- blockbuster_excel(file.path(working_dir, "Excel input.xlsm"))

print("producing summary")

results$"element summary" %>%
  filter(grade %in% c("C", "D", "E")) %>%
  group_by(year) %>%
  summarise(backlog = sum(backlog)) %>%
  as.data.frame() %>% # write.xlsx doesn't like tbl_df for some things
  write.xlsx(file = file.path(working_dir, paste0("output", time, ".xlsx")),
             sheetName = "Summary",
             row.names = FALSE)

results$"element summary" %>%
  group_by(year, grade) %>%
  summarise(area = sum(area), backlog = sum(backlog)) %>%
  as.data.frame() %>% # write.xlsx doesn't like tbl_df for some things
  write.xlsx(file = file.path(working_dir, paste0("output", time, ".xlsx")),
             sheetName = "Totals",
             row.names = FALSE,
             append = TRUE)

message("Creating element summary")

results$"element summary" %>%
  group_by(year, elementid) %>%
  summarise(area = sum(area), backlog = sum(backlog)) %>%
  as.data.frame() %>% # write.xlsx doesn't like tbl_df for some things
  write.xlsx(file = file.path(working_dir, paste0("output", time, ".xlsx")),
             sheetName = "Elements",
             row.names = FALSE,
             append = TRUE)

readline(prompt = "Finished")
Sys.sleep(10)

