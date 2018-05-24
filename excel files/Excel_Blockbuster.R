library(Blockbuster2)
library(readxl)

print ("loading functions")
args <- commandArgs(trailingOnly = TRUE)

working_dir <- file.path(args[1])
setwd(working_dir)

load_forecast_horizon <- function(path = "./excel files/Excel input.xlsm"){
  read_excel(path, range = "Inputs!G1", col_names = "forecast_horizon") %>% pull()
}

load_block_unit_rebuild <- function(path ="./excel files/Excel input.xlsm"){
  read_excel(path, range = "Inputs!G2", col_names = "unit_rebuild_cost") %>% pull()
}

load_grade_order <- function(path ="./excel files/Excel input.xlsm"){
  read_excel(path, range = "Inputs!G3", col_names = "grade_order") %>% pull() %>%
    strsplit("") %>% unlist()
}

load_location_factor <- function(path ="./excel files/Excel input.xlsm"){
  read_excel(path, range = "Inputs!G4", col_names = "location_factor") %>% pull()
}

load_save_path <- function(path ="./excel files/Excel input.xlsm"){
  read_excel(path, range = "Inputs!G6", col_names = "save_path") %>% pull()
}

load_file_label <- function(path ="./excel files/Excel input.xlsm"){
  read_excel(path, range = "Inputs!G7", col_names = "file_label") %>% pull()
}

load_data_path <- function(path ="./excel files/Excel input.xlsm"){
  read_excel(path, range = "Inputs!G9", col_names = "data_path") %>% pull()
}

load_project_path <- function(path ="./excel files/"){
  read_excel(path, range = "Inputs!G11", col_names = "project_path") %>% pull()
}

load_repair_budget <- function(path ="./excel files/Excel input.xlsm", years){
  read_excel(path, range = paste0("Inputs!B2:B", years + 1), col_names = "repair_budget") %>% pull()
}

load_rebuild_budget <- function(path ="./excel files/Excel input.xlsm", years){
  read_excel(path, range = paste0("Inputs!C2:C", years + 1), col_names = "rebuild_budget") %>% pull()
}

load_inflation <- function(path ="./excel files/Excel input.xlsm", years){
  read_excel(path, range = paste0("Inputs!D2:D", years + 1), col_names = "inflation") %>% pull()
}

load_det_rates <- function(path ="./excel files/Excel input.xlsm"){
  read_excel(path, range = "Deterioration rates!A1:E129") %>%
    rename(elementid = "Element ID")
}

load_repair_costs <- function(path ="./excel files/Excel input.xlsm"){
  read_excel(path, range = "Repair costs!A1:E129") %>%
    rename(elementid = "Element ID",
           B.repair.cost = "B repair cost",
           C.repair.cost = "C repair cost",
           D.repair.cost = "D repair cost",
           E.repair.cost = "E repair cost")
}

load_excel_inputs <- function(path = "./excel files/Excel input.xlsm"){

  # load inputs
  forecast_horizon <- load_forecast_horizon(path)
  unit_rebuild_cost <- load_block_unit_rebuild(path)
  grade_order <- load_grade_order(path)
  location_factor <- load_location_factor(path)
  save_path <- load_save_path(path)
  file_label <- load_file_label(path)
  data_path <- load_data_path(path)
  project_path <- load_project_path(path)
  repair_budget <- load_repair_budget(path, forecast_horizon)
  rebuild_budget <- load_rebuild_budget(path, forecast_horizon)
  inflation <- load_inflation(path, forecast_horizon)
  det_rates <- load_det_rates(path)
  repair_costs <- load_repair_costs(path)

  # return as list
  return(list(forecast_horizon = forecast_horizon,
              unit_rebuild_cost = unit_rebuild_cost,
              grade_order = grade_order,
              location_factor = location_factor,
              save_path = save_path,
              file_label = file_label,
              data_path = data_path,
              project_path = project_path,
              repair_budget = repair_budget,
              rebuild_budget = rebuild_budget,
              inflation = inflation,
              det_rates = det_rates,
              repair_costs = repair_costs))
}

create_inputs_from_excel <- function(path = "./excel files/Excel input.xlsm"){
  inputs <- load_excel_inputs(path)
  data <- readRDS(file.path(inputs$data_path, "PDS_three_tables.rds"))

  # add the deterioration rates and repair costs to the data. Note that this
  #  will leave NAs as rates and costs aren't defined for some
  # elements (e.g. decorations - unpainted) so we omit NA rows to remove them.
  inputs$element <- data$element %>%
    left_join(inputs$det_rates, by = "elementid") %>%
    left_join(inputs$repair_costs, by = "elementid") %>%
    na.omit() %>%
    # add in the building gifa from the data$building table as this is needed to
    # compute block rebuild costs
    left_join(data$building %>% select(buildingid, gifa), by = "buildingid") %>%
    # update the repair totals
    Blockbuster2:::UpdateElementRepairs()

  return(inputs)
}

blockbuster_excel <- function(path){

  # pull inputs from excel
  inputs <- create_inputs_from_excel(path)
  print(inputs)

  # run Blockbuster
  Blockbuster(element.data = inputs$element,
              forecast.horizon = inputs$forecast_horizon,
              rebuild.money = inputs$rebuild_budget,
              repair.money = inputs$repair_budget,
              block.rebuild.cost = inputs$unit_rebuild_cost,
              inflation = inputs$inflation,
              filelabel = inputs$file_label,
              path = file.path(inputs$project_path, inputs$save_path),
              grade.order = inputs$grade_order)
}

print("running blockbuster")

blockbuster_excel(file.path(working_dir, "Excel input.xlsm"))

save(args, file = file.path(args[1], "test.rda"))

